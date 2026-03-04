#!/usr/bin/env python3
import json
import re
import sys
from datetime import datetime

# Sentinel token for command/process substitutions and subshells.
SUBST = "\x00SUBST"

# Shell metacharacters that break words and start operators.
_META = frozenset(";|&<>()")

# Operators grouped by category, longest first within each group.
# Order matters: longer operators must be tried before shorter prefixes.
_OPERATORS = [
    # (string, category)
    ("<<<", "redir_in"),
    ("&>>", "redir_out"),
    ("&&", "sep"),
    ("||", "sep"),
    ("|&", "sep"),
    (";;", "sep"),
    (">>", "redir_out"),
    ("<<", "redir_in"),
    (">&", "redir_out"),
    ("<>", "redir_out"),
    (">|", "redir_out"),
    ("&>", "redir_out"),
    ("<(", "subst"),
    (">(", "subst"),
    (";", "sep"),
    ("|", "sep"),
    ("&", "sep"),
    (">", "redir_out"),
    ("<", "redir_in"),
    ("\n", "sep"),
]

# Shell keywords — if any parsed command starts with one of these,
# the whole input is rejected (returns None → "ask").
_KEYWORDS = frozenset({
    "while", "until", "if", "then", "else", "elif", "fi",
    "do", "done", "case", "esac", "select", "function",
    "{", "}", "[[", "coproc", "time",
})

# Regex allowlist for output redirection targets.  A target must fullmatch
# at least one pattern to be allowed.  Input redirections are not checked
# against this list (any safe target is OK for reading).
ALLOWED_REDIR_TARGETS = [
    re.compile(r"/dev/null"),
    re.compile(r"/dev/stdout"),
    re.compile(r"/dev/stderr"),
    re.compile(r"&[0-9]"),       # fd targets: &1, &2, etc.
    re.compile(r"[0-9]"),        # bare fd numbers (target of >& operator)
]


def _is_redir_target_allowed(target):
    """Return True if an output redirection target is in the allowlist."""
    return any(pat.fullmatch(target) for pat in ALLOWED_REDIR_TARGETS)


class ShellParser:
    """Recursive descent parser for shell commands.

    Parses a shell command string character-by-character and returns a flat
    list of simple commands.  Each command is a list of word-token strings.
    Command substitutions ($(...), `...`), process substitutions (<(...),
    >(...)), and subshells ((...)) are recursively parsed — their inner
    commands are appended to the flat list and a SUBST marker is placed in
    the parent command.

    Returns None for unsupported constructs (loops, arithmetic, nested ${},
    etc.), which causes the hook to fall back to "ask".
    """

    def __init__(self, text):
        self.text = text
        self.pos = 0

    def parse(self):
        """Parse the full input.  Returns list[list[str]] or None."""
        result = self._parse_command_list(end_char=None)
        if result is None:
            return None
        # Check for remaining input (shouldn't happen with end_char=None).
        if self.pos < len(self.text):
            return None
        return result

    # ------------------------------------------------------------------
    # Core: command list
    # ------------------------------------------------------------------

    def _parse_command_list(self, end_char, end_words=None):
        """Parse commands until *end_char* (or EOF if None).

        If *end_words* is provided (a set of strings), parsing also stops
        when a word matching an end_word is encountered at the start of a
        new command.

        Returns a flat list of commands, or None on error/unsupported.
        """
        commands = []
        current = []  # tokens for the command being built

        while self.pos < len(self.text):
            self._skip_whitespace()
            if self.pos >= len(self.text):
                break

            c = self.text[self.pos]

            # End-of-scope character (e.g. ')' for $(...), '`' for `...`).
            if c == end_char:
                self.pos += 1
                break

            # Comment — skip to end of line.
            if c == '#':
                self._skip_comment()
                continue

            # Try matching an operator at the current position.
            op, cat = self._match_operator()

            if cat == "sep":
                # Separator — flush current command.
                if current:
                    commands.append(current)
                    current = []
                continue

            if cat == "redir_out":
                # Output redirection — parse target, check against allowlist.
                target = self._parse_redir_target()
                if target is None:
                    return None
                if not _is_redir_target_allowed(target):
                    return None
                continue

            if cat == "redir_in":
                # Input redirection — parse target, reject substitutions.
                target = self._parse_redir_target()
                if target is None:
                    return None
                continue

            if cat == "subst":
                # Process substitution <(...) or >(...).
                inner = self._parse_command_list(end_char=")")
                if inner is None:
                    return None
                commands.extend(inner)
                current.append(SUBST)
                continue

            # ( — subshell (no SUBST token; commands go directly into list).
            if c == "(":
                self.pos += 1
                inner = self._parse_command_list(end_char=")")
                if inner is None:
                    return None
                commands.extend(inner)
                continue

            # Unexpected ')' at the top level.
            if c == ")" and end_char != ")":
                return None

            # fd-prefixed redirection: 2>&, 2>>, 2>
            if c == "2" and self._peek(1) == ">":
                if self._peek(2) == "&":
                    self.pos += 3  # consume 2>&
                elif self._peek(2) == ">":
                    self.pos += 3  # consume 2>>
                else:
                    self.pos += 2  # consume 2>
                target = self._parse_redir_target()
                if target is None:
                    return None
                if not _is_redir_target_allowed(target):
                    return None
                continue

            # Otherwise: parse a word.
            start_pos = self.pos
            word = self._parse_word(commands, end_char=end_char)
            if word is None:
                return None
            if self.pos == start_pos:
                # Nothing consumed — guard against infinite loops.
                return None

            # Check for end_words (e.g. "done" for for-loops).
            if end_words and not current and word in end_words:
                break

            # Detect for-loop: "for" as the first word of a command.
            if word == "for" and not current:
                result = self._parse_for_loop(commands)
                if result is None:
                    return None
                commands.extend(result)
                continue

            current.append(word)

        else:
            # Ran off the end without finding end_char.
            if end_char is not None:
                return None  # unclosed delimiter

        if current:
            commands.append(current)

        # Reject commands that start with a shell keyword.
        for cmd in commands:
            if cmd and cmd[0] in _KEYWORDS:
                return None

        return commands

    # ------------------------------------------------------------------
    # Operator matching
    # ------------------------------------------------------------------

    def _match_operator(self):
        """Try to match an operator at the current position.

        If a match is found, advances self.pos past the operator and returns
        (operator_string, category).  Otherwise returns (None, None).
        """
        remaining = self.text[self.pos:]
        for op, cat in _OPERATORS:
            if remaining.startswith(op):
                self.pos += len(op)
                return op, cat
        return None, None

    def _parse_redir_target(self):
        """Parse a redirection target word.

        Returns the target string, or None if the target is missing or
        contains a command substitution ($(...) or backtick).
        """
        self._skip_whitespace()
        if self.pos >= len(self.text) or self.text[self.pos] in _META:
            return None  # no target
        # Reject if target starts with $( or backtick.
        if self.text[self.pos] == "`":
            return None
        if self.text[self.pos] == "$" and self._peek(1) == "(":
            return None
        target = self._parse_word()
        if target is None or target == "":
            return None
        # Reject if substitutions were found inside the target (e.g. in
        # double-quoted strings) or if _parse_word stopped at a substitution
        # boundary mid-word (e.g. file$(cmd) or file`cmd`).
        if target == SUBST:
            return None
        if self.pos < len(self.text):
            if self.text[self.pos] == "`":
                return None
            if self.text[self.pos] == "$" and self._peek(1) == "(":
                return None
        return target

    # ------------------------------------------------------------------
    # For-loop parsing
    # ------------------------------------------------------------------

    def _parse_for_loop(self, commands):
        """Parse a for-loop after 'for' has been consumed.

        Expects: VAR in ITEM... ; do BODY ; done
        Returns a list of body commands, or None on error.
        Substitutions in the item list are added to *commands*.
        """
        # 1. Parse variable name — must be a simple identifier (raw chars).
        self._skip_whitespace()
        if self.pos >= len(self.text):
            return None
        var_start = self.pos
        while self.pos < len(self.text) and (
            self.text[self.pos].isalnum() or self.text[self.pos] == "_"
        ):
            self.pos += 1
        if self.pos == var_start:
            return None  # no identifier chars (e.g. starts with $, quote, etc.)
        var = self.text[var_start:self.pos]
        if not var.isidentifier():
            return None  # var name has special chars, quotes, etc.

        # 2. Expect 'in'.
        self._skip_whitespace()
        if self.pos >= len(self.text):
            return None
        in_word = self._parse_word()
        if in_word != "in":
            return None

        # 3. Parse item list — words until a separator.
        while True:
            self._skip_whitespace()
            if self.pos >= len(self.text):
                return None

            # A separator ends the item list.
            _, cat = self._match_operator()
            if cat == "sep":
                break
            if cat is not None:
                return None  # non-separator operator in item list

            start_pos = self.pos
            word = self._parse_word(commands)
            if word is None:
                return None
            if self.pos == start_pos:
                return None
            # Item — substitution inner commands are already in 'commands'
            # via _parse_word. The item value itself is discarded.

        # 3b. Consume 'do' keyword after separator.
        self._skip_whitespace()
        # Skip additional separators (e.g. "for f in a;\n do").
        while True:
            _, cat2 = self._match_operator()
            if cat2 == "sep":
                self._skip_whitespace()
                continue
            break
        if self.pos >= len(self.text):
            return None
        start_pos = self.pos
        word = self._parse_word()
        if word != "do" or self.pos == start_pos:
            return None

        # 4. Parse body until 'done'.
        body = self._parse_command_list(end_char=None, end_words={"done"})
        if body is None:
            return None

        return body

    # ------------------------------------------------------------------
    # Word parsing
    # ------------------------------------------------------------------

    def _parse_word(self, commands=None, end_char=None):
        """Parse a single word token.

        If *commands* is provided, any command substitutions ($(...),
        backticks) found inside the word (including inside double-quoted
        strings) are recursively parsed and appended to it.

        If *end_char* is set (e.g. '`'), that character is treated as a
        word boundary rather than a substitution opener.

        Returns the word string, SUBST if the word contains a command
        substitution, "" if nothing was consumed, or None on error.
        """
        buf = []
        has_subst = False

        while self.pos < len(self.text):
            c = self.text[self.pos]

            # Word boundaries.
            if c in (" ", "\t", "\n", "\r"):
                break
            if c in _META:
                break

            # $ — variable expansion or command substitution.
            if c == "$":
                if self._at_command_subst():
                    if self._peek(2) == "(":
                        return None  # $(( arithmetic — unsupported
                    # $(...) command substitution inline.
                    self.pos += 2  # skip $(
                    inner = self._parse_command_list(end_char=")")
                    if inner is None:
                        return None
                    has_subst = True
                    if commands is not None:
                        commands.extend(inner)
                    continue
                result = self._parse_dollar(buf)
                if result is None:
                    return None
                continue

            # Backslash escape.
            if c == "\\":
                self.pos += 1
                if self.pos < len(self.text):
                    buf.append(self.text[self.pos])
                    self.pos += 1
                else:
                    buf.append("\\")
                continue

            # Single-quoted string.
            if c == "'":
                s = self._parse_single_quote()
                if s is None:
                    return None
                buf.append(s)
                continue

            # Double-quoted string.
            if c == '"':
                result = self._parse_double_quote()
                if result is None:
                    return None
                content, inner_cmds = result
                buf.append(content)
                if inner_cmds:
                    has_subst = True
                    if commands is not None:
                        commands.extend(inner_cmds)
                continue

            # Backtick — word boundary if we're inside a backtick context,
            # otherwise a command substitution.
            if c == "`":
                if end_char == "`":
                    break  # closing backtick — stop word
                self.pos += 1  # skip opening `
                inner = self._parse_command_list(end_char="`")
                if inner is None:
                    return None
                has_subst = True
                if commands is not None:
                    commands.extend(inner)
                continue

            # '#' at the start of a word is a comment, mid-word it's literal.
            if c == "#" and not buf and not has_subst:
                break

            # Regular character.
            buf.append(c)
            self.pos += 1

        return SUBST if has_subst else "".join(buf)

    def _parse_dollar(self, buf):
        """Handle $ during word parsing (non-substitution cases only).

        Handles $var, ${...}, and special variables.  The caller MUST check
        _at_command_subst() before calling this method and handle $( / $((
        themselves.

        Appends expansion text to *buf* and returns a truthy value,
        or returns None on error.
        """
        assert not self._at_command_subst(), \
            "_parse_dollar called at command substitution; caller must handle $( / $(("

        # ${ — parameter expansion.
        if self._peek(1) == "{":
            result = self._parse_brace_expansion()
            if result is None:
                return None
            buf.append(result)
            return True

        # $var — simple variable reference.
        if self._peek(1) is not None and (self._peek(1).isalnum() or self._peek(1) == "_"):
            self.pos += 1  # skip $
            start = self.pos
            while self.pos < len(self.text) and (
                self.text[self.pos].isalnum() or self.text[self.pos] == "_"
            ):
                self.pos += 1
            buf.append("$" + self.text[start : self.pos])
            return True

        # $? $! $$ $# $@ $* $0-$9 and other special vars
        if self._peek(1) is not None and self._peek(1) in "?!$#@*-0123456789":
            buf.append("$" + self.text[self.pos + 1])
            self.pos += 2
            return True

        # Bare $ — append literally.
        buf.append("$")
        self.pos += 1
        return True

    def _parse_brace_expansion(self):
        """Parse ${...}.  Returns the expansion string or None on error."""
        # self.pos is at '$', next char is '{'.
        start = self.pos
        self.pos += 2  # skip ${
        depth = 1

        while self.pos < len(self.text) and depth > 0:
            c = self.text[self.pos]
            if c == "}":
                depth -= 1
                if depth == 0:
                    self.pos += 1  # skip closing }
                    return self.text[start : self.pos]
            elif c == "{":
                depth += 1
            elif c == "$":
                # Nested $ inside ${...} — unsupported.
                return None
            elif c == "\\":
                self.pos += 1  # skip escaped char
            elif c == "'":
                # Skip single-quoted string inside ${...}.
                self.pos += 1
                while self.pos < len(self.text) and self.text[self.pos] != "'":
                    self.pos += 1
                # pos is at closing ' (or end of input).
            elif c == '"':
                # Skip double-quoted string inside ${...}.
                self.pos += 1
                while self.pos < len(self.text) and self.text[self.pos] != '"':
                    if self.text[self.pos] == "\\":
                        self.pos += 1  # skip escaped char
                    self.pos += 1
                # pos is at closing " (or end of input).
            self.pos += 1

        return None  # unmatched {

    def _parse_single_quote(self):
        """Parse '...' and return the content (without quotes)."""
        self.pos += 1  # skip opening '
        start = self.pos
        while self.pos < len(self.text):
            if self.text[self.pos] == "'":
                content = self.text[start : self.pos]
                self.pos += 1  # skip closing '
                return content
            self.pos += 1
        return None  # unclosed single quote

    def _parse_double_quote(self):
        """Parse "..." handling escapes, $ expansions, and substitutions.

        Returns (content_str, inner_commands) or None on error.
        inner_commands is a list of commands extracted from $(...) and
        backtick substitutions found inside the quoted string.
        """
        self.pos += 1  # skip opening "
        buf = []
        inner_commands = []

        while self.pos < len(self.text):
            c = self.text[self.pos]

            if c == '"':
                self.pos += 1  # skip closing "
                return "".join(buf), inner_commands

            if c == "\\":
                self.pos += 1
                if self.pos < len(self.text):
                    # In double quotes, only certain chars are special after \.
                    next_c = self.text[self.pos]
                    if next_c in ('"', "\\", "$", "`"):
                        buf.append(next_c)
                    else:
                        buf.append("\\")
                        buf.append(next_c)
                    self.pos += 1
                continue

            if c == "$":
                if self._at_command_subst():
                    if self._peek(2) == "(":
                        return None  # $(( arithmetic — unsupported
                    # $(...) command substitution inside double quotes.
                    self.pos += 2  # skip $(
                    inner = self._parse_command_list(end_char=")")
                    if inner is None:
                        return None
                    inner_commands.extend(inner)
                    buf.append(SUBST)
                    continue
                # Regular $ expansion ($var, ${...}, etc.)
                inner_buf = []
                result = self._parse_dollar(inner_buf)
                if result is None:
                    return None
                buf.extend(inner_buf)
                continue

            if c == "`":
                # Backtick command substitution inside double quotes.
                self.pos += 1  # skip opening `
                inner = self._parse_command_list(end_char="`")
                if inner is None:
                    return None
                inner_commands.extend(inner)
                buf.append(SUBST)
                continue

            buf.append(c)
            self.pos += 1

        return None  # unclosed double quote

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _at_command_subst(self):
        """Return True if pos is at $( or $(( (command/arithmetic subst)."""
        return self._peek(0) == "$" and self._peek(1) == "("

    def _peek(self, offset=0):
        """Return char at pos+offset, or None if out of bounds."""
        idx = self.pos + offset
        if 0 <= idx < len(self.text):
            return self.text[idx]
        return None

    def _skip_whitespace(self):
        while self.pos < len(self.text) and self.text[self.pos] in (" ", "\t", "\r"):
            self.pos += 1

    def _skip_comment(self):
        while self.pos < len(self.text) and self.text[self.pos] != "\n":
            self.pos += 1


# ======================================================================
# Allowlist
# ======================================================================

# Each rule is a tuple of regex patterns.  rule[0] matches the command name,
# rule[1..] match subsequent tokens positionally.  Extra tokens are allowed.
# All patterns are matched with re.fullmatch().
ALLOWED_COMMANDS = [
    (r"(.*/)?llvm/bin/FileCheck",),
    (r"(.*/)?llvm/bin/llvm-lit",),
    (r"(.*/)?llvm/bin/opt",),
    (r".*/clang-format",),
    (r"cd", r".*/w/src.*"),
    (r"cd", r".*/w/build-.*"),
    (r"head",),
    (r"tail",),
    (r"grep",),
    (r"sed",),
    (r"sort",),
    (r"awk",),
    (r"wc",),
    (r"diff",),
    (r"echo",),
    (r"ls",),
    (r"ninja",),
    (r"pwd",),
    (r"tr",),
    (r"git", r"diff"),
    (r"git", r"show"),
    (r"git", r"log"),
    (r"git", r"status"),
    (r"git", r"checkout", r"--"),
]


def is_command_allowed(cmd_tokens):
    """Check whether a single command (list of word strings) matches any rule."""
    for rule in ALLOWED_COMMANDS:
        if len(cmd_tokens) < len(rule):
            continue
        if all(re.fullmatch(pat, cmd_tokens[i]) for i, pat in enumerate(rule)):
            return True
    return False


def extract_commands(command_str):
    """Parse a shell command string into a list of simple commands.

    Returns list[list[str]] or None on error/unsupported constructs.
    """
    parser = ShellParser(command_str)
    return parser.parse()


def is_allowed(tool_name, tool_input):
    if tool_name != "Bash":
        return False
    command = tool_input.get("command", "")
    commands = extract_commands(command)
    if not commands:
        return False
    return all(is_command_allowed(cmd) for cmd in commands)


def main():
    data = json.loads(sys.stdin.read())
    tool_name = data.get("tool_name", "unknown")
    tool_input = data.get("tool_input", {})
    allowed = is_allowed(tool_name, tool_input)

    with open("/tmp/pre-tool-hook.log", "a") as f:
        status = "ALLOW" if allowed else "DENY"
        f.write(f"[{datetime.now().isoformat()}] {status} tool={tool_name} input={json.dumps(tool_input)}\n")

    if allowed:
        result = {
            "ok": True,
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": "allow",
                "permissionDecisionReason": "Command is in the allowed list",
            },
        }
    else:
        result = {
            "ok": False,
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": "ask",
                "permissionDecisionReason": "Not in the allowed commands list",
            },
        }
    print(json.dumps(result))

if __name__ == "__main__":
    main()
