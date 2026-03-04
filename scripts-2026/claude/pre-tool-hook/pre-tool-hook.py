#!/usr/bin/env python3
import json
import re
import shlex
import sys
from datetime import datetime

SEPARATORS = {"&&", "||", ";", "|", "&", "(", ")", "`"}

# Each entry is a regex matched against the full sub-command (tokens joined by spaces).
ALLOWED_COMMANDS = [
    re.compile(r"(.*/)?llvm/bin/FileCheck(\s|$)"),
    re.compile(r"(.*/)?llvm/bin/llvm-lit(\s|$)"),
    re.compile(r"(.*/)?llvm/bin/opt(\s|$)"),
    re.compile(r".*/clang-format(\s|$)"),
    re.compile(r"cd\s+.*/w/src"),
    re.compile(r"cd\s+.*/w/build-"),
    re.compile(r"head(\s|$)"),
    re.compile(r"tail(\s|$)"),
    re.compile(r"grep(\s|$)"),
    re.compile(r"sed(\s|$)"),
    re.compile(r"sort(\s|$)"),
    re.compile(r"awk(\s|$)"),
    re.compile(r"wc(\s|$)"),
    re.compile(r"diff(\s|$)"),
    re.compile(r"echo(\s|$)"),
    re.compile(r"ls(\s|$)"),
    re.compile(r"ninja(\s|$)"),
    re.compile(r"pwd$"),
    re.compile(r"tr(\s|$)"),
    re.compile(r"git\s+diff(\s|$)"),
    re.compile(r"git\s+show(\s|$)"),
    re.compile(r"git\s+log(\s|$)"),
    re.compile(r"git\s+status(\s|$)"),
    re.compile(r"git\s+checkout\s+--(\s|$)"),
]


def extract_commands(command_str):
    """Split a compound shell command into a list of sub-commands.

    Each sub-command is a list of tokens. Returns None if parsing fails.
    """
    try:
        lexer = shlex.shlex(command_str, posix=True, punctuation_chars=True)
        tokens = list(lexer)
    except ValueError:
        return None

    commands = []
    current = []
    for token in tokens:
        if token in SEPARATORS:
            if current:
                commands.append(current)
                current = []
        else:
            current.append(token)
    if current:
        commands.append(current)
    return commands


def is_command_allowed(cmd_tokens):
    cmd_str = " ".join(cmd_tokens)
    return any(pattern.match(cmd_str) for pattern in ALLOWED_COMMANDS)


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
