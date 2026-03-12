# Pre-Tool Hook for Claude Code

This directory contains a Claude Code `PreToolUse` hook that acts as a command
allowlist filter for Bash tool invocations.

## How It Works

The hook (`pre-tool-hook.py`) receives a JSON payload on stdin describing a tool
call. It uses a custom recursive descent parser (`ShellParser`) to parse the
shell command character-by-character into a flat list of simple commands. Each
command is checked against a regex-based allowlist. If **all** commands match,
the hook auto-approves the invocation (skipping the interactive permission
prompt). If any command is not recognized, or if parsing fails (due to
unsupported or invalid bash construct, for example), the hook falls back to
`"ask"`, which triggers the normal interactive approval prompt.

All decisions are logged to `/tmp/pre-tool-hook.log`.

### Allowlists

**`ALLOWED_COMMANDS`** — a list of tuples of regex patterns matched
positionally against command tokens using `re.fullmatch()`. The first pattern
matches the command name, subsequent patterns match required prefix arguments.
Extra arguments after the matched prefix are allowed.

```python
ALLOWED_COMMANDS = [
    (r"echo",),                       # echo with any args
    (r"git", r"checkout", r"--"),     # git checkout -- <files>
    (r"cd", r".*/w/build-.*"),        # cd into build dirs
    (r"(.*/)?llvm/bin/opt",),         # opt with any path prefix
]
```

**`ALLOWED_REDIR_TARGETS`** — a list of compiled regex patterns for permitted
output redirection targets. Input redirections are not checked against this
list.

## Files

- `pre-tool-hook.py` — the hook script
- `test-pre-tool-hook.py` — test suite (parser unit tests + integration tests)

## Installation

Register the hook in your Claude Code settings (e.g. `~/.claude/settings.json`):

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "python3 /path/to/pre-tool-hook.py"
          }
        ]
      }
    ]
  }
}
```

## Running Tests

```sh
python3 test-pre-tool-hook.py
```
