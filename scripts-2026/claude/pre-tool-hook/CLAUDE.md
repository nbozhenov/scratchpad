# Pre-Tool Hook for Claude Code

This directory contains a Claude Code `PreToolUse` hook that acts as a command
allowlist filter for Bash tool invocations.

## How It Works

The hook (`pre-tool-hook.py`) receives a JSON payload on stdin describing a
tool call. It parses compound shell commands into individual sub-commands
(splitting on `&&`, `||`, `;`, `|`, etc.) and checks each one against a
regex-based allowlist. If **all** sub-commands match, the hook auto-approves the
invocation (skipping the interactive permission prompt). If any sub-command is
not recognized, the hook falls back to `"ask"`, which triggers the normal
interactive approval prompt.

All decisions are logged to `/tmp/pre-tool-hook.log`.

## Files

- `pre-tool-hook.py` — the hook script
- `test-pre-tool-hook.py` — test suite that exercises the allowlist against
  various allowed and denied command patterns

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
