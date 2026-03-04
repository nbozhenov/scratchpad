#!/usr/bin/env python3
import json
import subprocess
import sys

HOOK_SCRIPT = "./pre-tool-hook.py"

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
    # --- denied: other commands ---
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


def main():
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

    print(f"\n{passed + failed} tests: {passed} passed, {failed} failed")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
