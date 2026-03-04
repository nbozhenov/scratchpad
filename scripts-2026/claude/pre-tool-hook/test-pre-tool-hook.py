#!/usr/bin/env python3
import json
import os
import subprocess
import sys

sys.path.insert(0, os.path.dirname(__file__))
from importlib import import_module
hook = import_module("pre-tool-hook")
ShellParser = hook.ShellParser
SUBST = hook.SUBST

HOOK_SCRIPT = "./pre-tool-hook.py"
S = SUBST  # short alias for test readability

# ---- Parser unit tests ----
# (input_string, expected_commands_or_None)
PARSER_CASES = [
    # --- basic commands ---
    ("echo hello", [["echo", "hello"]]),
    ("ls -la /tmp", [["ls", "-la", "/tmp"]]),
    ("pwd", [["pwd"]]),
    ("", []),

    # --- separators ---
    ("echo a && echo b", [["echo", "a"], ["echo", "b"]]),
    ("echo a || echo b", [["echo", "a"], ["echo", "b"]]),
    ("echo a ; echo b", [["echo", "a"], ["echo", "b"]]),
    ("echo a | cat", [["echo", "a"], ["cat"]]),
    ("echo a & echo b", [["echo", "a"], ["echo", "b"]]),
    ("echo a |& cat", [["echo", "a"], ["cat"]]),

    # --- redirections ---
    # Output to a file — denied (not in ALLOWED_REDIR_TARGETS)
    ("echo hello > out", None),
    ("echo hello >> out", None),
    ("echo a >|out", None),
    # Output to allowed targets
    ("echo a > /dev/null", [["echo", "a"]]),
    ("echo a >> /dev/null", [["echo", "a"]]),
    ("echo a &>/dev/null", [["echo", "a"]]),
    ("echo a &>>/dev/null", [["echo", "a"]]),
    ("echo a > /dev/stderr", [["echo", "a"]]),
    # fd redirection
    ("echo hello 2>&1", [["echo", "hello"]]),
    ("echo hello 2>/dev/null", [["echo", "hello"]]),
    ("echo hello 2>>/dev/null", [["echo", "hello"]]),
    ("echo 2>>out", None),  # output to 'out' not allowed
    # Input redirections — allowed (target sanitized, no allowlist check)
    ("cat < in.txt", [["cat"]]),
    ("cat << EOF", [["cat"]]),
    ("cat <<< word", [["cat"]]),
    # Redirections with command substitution in target — denied
    ("cat < $(echo file)", None),
    ("cat < `echo file`", None),
    ("echo hello > $(echo file)", None),
    # Command substitution mid-word in redir target — denied
    ("cat < foo$(echo bar)", None),
    ("cat < foo`echo bar`", None),

    # --- command substitution $(...) ---
    ("echo $(ls /)", [["ls", "/"], ["echo", S]]),
    ("echo $(ls $(pwd))", [["pwd"], ["ls", S], ["echo", S]]),
    ("echo $(echo a && echo b)", [["echo", "a"], ["echo", "b"], ["echo", S]]),

    # --- backtick substitution ---
    ("echo `ls`", [["ls"], ["echo", S]]),
    ("echo `ls -la`", [["ls", "-la"], ["echo", S]]),

    # --- process substitution ---
    ("diff <(ls a) <(ls b)", [["ls", "a"], ["ls", "b"], ["diff", S, S]]),
    ("cat >(tee out.txt)", [["tee", "out.txt"], ["cat", S]]),

    # --- subshell ---
    ("(echo a; echo b)", [["echo", "a"], ["echo", "b"]]),
    ("(echo a) && (echo b)", [["echo", "a"], ["echo", "b"]]),

    # --- variable references (single word) ---
    ("echo $var", [["echo", "$var"]]),
    ("echo $HOME/bin", [["echo", "$HOME/bin"]]),
    ("echo $HOME/$USER", [["echo", "$HOME/$USER"]]),

    # --- parameter expansion ${...} (single word) ---
    ("echo ${HOME}", [["echo", "${HOME}"]]),
    ("echo ${var:-default}", [["echo", "${var:-default}"]]),
    ("echo ${var:=default}", [["echo", "${var:=default}"]]),
    ("echo ${var:0:5}", [["echo", "${var:0:5}"]]),
    ("echo ${var%pattern}", [["echo", "${var%pattern}"]]),

    # --- paths with mixed $ expansions (single word) ---
    ("echo ${HOME}/${USER}", [["echo", "${HOME}/${USER}"]]),
    ("echo ${A}${B}", [["echo", "${A}${B}"]]),
    ("echo ${A}$B", [["echo", "${A}$B"]]),
    ("echo prefix${var:-default}suffix", [["echo", "prefix${var:-default}suffix"]]),
    ("echo $HOME/${PWD}/${MY_VAR:-default}/scripts",
     [["echo", "$HOME/${PWD}/${MY_VAR:-default}/scripts"]]),

    # --- quoting ---
    ("echo 'hello world'", [["echo", "hello world"]]),
    ('echo "hello world"', [["echo", "hello world"]]),
    ("echo \"it's\"", [["echo", "it's"]]),
    ("echo 'say \"hi\"'", [["echo", 'say "hi"']]),
    ("echo \"say 'hi'\"", [["echo", "say 'hi'"]]),
    # Adjacent quotes merge into one word
    ("echo 'hello'\"world\"", [["echo", "helloworld"]]),
    ("echo \"hello\"'world'", [["echo", "helloworld"]]),
    # Mid-word quotes
    ("echo hello' world'", [["echo", "hello world"]]),
    ('echo hello" world"', [["echo", "hello world"]]),
    # Empty quotes produce empty-string token
    ("echo '' foo", [["echo", "", "foo"]]),
    ('echo "" foo', [["echo", "", "foo"]]),
    # Dollar inside double quotes
    ('echo "path=$HOME"', [["echo", "path=$HOME"]]),
    ('echo "$HOME/${PWD}/scripts"', [["echo", "$HOME/${PWD}/scripts"]]),
    # $(...) inside double quotes — recursively parsed
    ('echo "$(ls)"', [["ls"], ["echo", S]]),
    ('echo "hello $(ls) world"', [["ls"], ["echo", S]]),
    ('echo "$(ls) $(pwd)"', [["ls"], ["pwd"], ["echo", S]]),
    ('echo "$(ls)" other', [["ls"], ["echo", S, "other"]]),
    # backtick inside double quotes — recursively parsed
    ('echo "hello `ls` world"', [["ls"], ["echo", S]]),
    # deeply nested: echo "abc $(ls -1 `get_dir "neh $(cat doc)"`)"
    ('echo "abc $(ls -1 `get_dir "neh $(cat doc)"`)"',
     [["cat", "doc"], ["get_dir", S], ["ls", "-1", S], ["echo", S]]),
    # no substitution in double quotes — plain word
    ('echo "no subst here"', [["echo", "no subst here"]]),
    # Dollar inside single quotes (literal, not expanded)
    ("echo '$HOME'", [["echo", "$HOME"]]),

    # --- backslash escapes ---
    ("echo hello\\ world", [["echo", "hello world"]]),
    ("echo \\$HOME", [["echo", "$HOME"]]),

    # --- comments ---
    ("echo hello # comment", [["echo", "hello"]]),
    ("echo hello#notacomment", [["echo", "hello#notacomment"]]),
    ("# just a comment", []),

    # --- $() inside a word (substitution boundary) ---
    # echo$(cmd) — single word in bash, contains substitution → SUBST
    ("echo$(ls /)", [["ls", "/"], [S]]),
    # standalone $(foo bar) — also a single word containing substitution
    ("$(foo bar)", [["foo", "bar"], [S]]),

    # --- special variables ---
    ("echo $?", [["echo", "$?"]]),
    ("echo $!", [["echo", "$!"]]),
    ("echo $$", [["echo", "$$"]]),
    ("echo $#", [["echo", "$#"]]),
    ("echo $@", [["echo", "$@"]]),
    ("echo $0", [["echo", "$0"]]),
    ("echo $1", [["echo", "$1"]]),

    # --- unsupported: keywords ---
    ("for f in a; do echo; done", None),
    ("while true; do echo; done", None),
    ("if true; then echo; fi", None),

    # --- unsupported: arithmetic ---
    ("echo $((1+2))", None),

    # --- unsupported: nested $ in ${} ---
    ("echo ${var:-$(cmd)}", None),       # command substitution inside ${}
    ("echo ${var:-${other}}", None),     # nested ${} inside ${}
    ("echo ${x:-$abc}", None),           # simple $var inside ${}
    ("echo ${x:-$HOME/bin}", None),      # $var with path inside ${}

    # --- backtick in double quotes —--
    ('echo "`ls`"', [["ls"], ["echo", S]]),

    # --- unclosed quotes ---
    ("echo 'hello", None),
    ('echo "hello', None),

    # --- unclosed delimiters ---
    ("echo (", None),
    ("echo (abc", None),
    ("echo $(ls", None),
    ("echo ${var", None),
    ("echo `ls", None),

    # --- mismatched delimiters ---
    ("echo (abc}", None),
    ("echo ${var)", None),
    ("echo $(ls}", None),
    ("echo `ls)", None),

    # --- closing without opening ---
    ("echo )", None),
    ("echo abc)", None),

    # --- nested, outer unclosed ---
    ("echo (abc (def)", None),
    ("echo $(ls $(pwd)", None),

    # --- mixed valid/invalid ---
    ("echo (abc) && (def", None),
    ("echo $(ls /) && $(pwd", None),
]

# (tool_name, tool_input, expected_ok)
TEST_CASES = [
    # --- allowed: llvm-lit ---
    ("Bash", {"command": "~/l/llvm/build/llvm/bin/llvm-lit -sv test.py"}, True),
    ("Bash", {"command": "/usr/local/llvm/bin/llvm-lit -sv test.py"}, True),
    # --- allowed: opt ---
    ("Bash", {"command": "~/l/llvm/build/llvm/bin/opt -S -passes=mem2reg test.ll"}, True),
    ("Bash", {"command": "/usr/local/llvm/bin/opt -O2 foo.ll -o bar.ll"}, True),
    # --- allowed: relative paths ---
    ("Bash", {"command": "llvm/bin/llvm-lit -sv test.py"}, True),
    ("Bash", {"command": "llvm/bin/opt -S test.ll"}, True),
    ("Bash", {"command": "cd ~/w/build-athena-asan && llvm/bin/llvm-lit -q ~/w/src/llvm/test/Transforms/MTIALICM2 2>&1"}, True),
    # --- allowed: chained with pipe ---
    ("Bash", {"command": "cd /home/nikolai/w/build-athena-asan && llvm/bin/llvm-lit -v /home/nikolai/w/src/llvm/test/Transforms/MTIALICM2/athena/fb-licm-loop-nest-2.ll 2>&1 | tail -80"}, True),
    # --- allowed: chained ---
    ("Bash", {"command": "~/l/llvm/build/llvm/bin/llvm-lit -sv test1.py && /usr/local/llvm/bin/opt -S test.ll"}, True),
    ("Bash", {"command": "~/l/llvm/build/llvm/bin/llvm-lit -sv test.py | grep FAIL"}, True),
    # --- denied: bare commands (no .*/llvm/bin/ prefix) ---
    ("Bash", {"command": "llvm-lit -sv test.py"}, False),
    ("Bash", {"command": "opt -S test.ll"}, False),
    ("Bash", {"command": "/usr/bin/llvm-lit -sv test.py"}, False),
    # --- allowed: simple commands ---
    ("Bash", {"command": "ls"}, True),
    ("Bash", {"command": "ls -1 | grep . | wc -l"}, True),
    ("Bash", {"command": "~/l/llvm/build/llvm/bin/llvm-lit -sv test.py && echo done"}, True),
    ("Bash", {"command": "rm -rf / ; ~/l/llvm/build/llvm/bin/llvm-lit -sv test.py"}, False),
    # --- allowed: git checkout -- (restore from index) ---
    ("Bash", {"command": "git checkout -- file.txt"}, True),
    ("Bash", {"command": "git checkout -- src/foo.cpp src/bar.cpp"}, True),
    # --- denied: git checkout with ref before -- ---
    ("Bash", {"command": "git checkout abcd -- /abc/def"}, False),
    ("Bash", {"command": "git checkout main"}, False),
    # --- denied: non-Bash tools ---
    ("Read", {"file_path": "/tmp/foo"}, False),
    ("Write", {"file_path": "/tmp/foo", "content": "hello"}, False),
    ("SomethingNew", {"foo": "bar"}, False),
    # --- denied: edge cases ---
    ("Bash", {"command": ""}, False),
    ("Bash", {"command": "~/l/llvm/build/llvm/bin/llvm-lit -sv `rm -rf /` test.py"}, False),
    ("Bash", {"command": "`rm -rf /`"}, False),
    # --- allowed: command substitution with allowed commands ---
    ("Bash", {"command": "echo $(ls /tmp)"}, True),
    ("Bash", {"command": "echo $(echo hello)"}, True),
    # --- denied: command substitution with disallowed commands ---
    ("Bash", {"command": "echo $(rm -rf /)"}, False),
    ("Bash", {"command": "$(rm -rf /)"}, False),
    # --- allowed: process substitution with allowed commands ---
    ("Bash", {"command": "diff <(ls a) <(ls b)"}, True),
    # --- denied: process substitution with disallowed commands ---
    ("Bash", {"command": "diff <(rm a) <(ls b)"}, False),
    # --- denied: for loops ---
    ("Bash", {"command": "for f in a b; do echo $f; done"}, False),
]


def run_hook(tool_name, tool_input):
    payload = json.dumps({
        "session_id": "test",
        "hook_event_name": "PreToolUse",
        "tool_name": tool_name,
        "tool_input": tool_input,
    })
    result = subprocess.run(
        [sys.executable, HOOK_SCRIPT],
        input=payload,
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, f"Hook exited with code {result.returncode}: {result.stderr}"
    return json.loads(result.stdout)


def run_parser_tests():
    passed = 0
    failed = 0

    for cmd, expected in PARSER_CASES:
        parser = ShellParser(cmd)
        actual = parser.parse()

        ok = actual == expected
        status = "PASS" if ok else "FAIL"
        print(f"[{status}] parser: {cmd!r}")
        if ok:
            passed += 1
        else:
            print(f"       expected: {expected}")
            print(f"       actual:   {actual}")
            failed += 1

    return passed, failed


def run_integration_tests():
    passed = 0
    failed = 0

    for tool_name, tool_input, expected_ok in TEST_CASES:
        out = run_hook(tool_name, tool_input)
        actual_ok = out["ok"]
        ok = actual_ok == expected_ok

        expected_str = "ALLOW" if expected_ok else "DENY"
        actual_str = "ALLOW" if actual_ok else "DENY"
        status = "PASS" if ok else "FAIL"

        print(f"[{status}] tool={tool_name} input={json.dumps(tool_input)}")
        if ok:
            print(f"       result={actual_str} (as expected)")
            passed += 1
        else:
            print(f"       expected={expected_str} actual={actual_str}")
            failed += 1

    return passed, failed


def main():
    print("=== Parser unit tests ===")
    pp, pf = run_parser_tests()

    print("\n=== Integration tests ===")
    ip, if_ = run_integration_tests()

    total = pp + pf + ip + if_
    passed = pp + ip
    failed = pf + if_
    print(f"\n{total} tests: {passed} passed, {failed} failed")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
