#!/bin/bash
# Statusline for Claude Code

input=$(cat)

# Extract values
total_input=$(echo "$input" | jq -r '.context_window.total_input_tokens // 0')
total_output=$(echo "$input" | jq -r '.context_window.total_output_tokens // 0')
last_input=$(echo "$input" | jq -r '.context_window.current_usage.input_tokens // 0')
last_output=$(echo "$input" | jq -r '.context_window.current_usage.output_tokens // 0')
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // 0')
transcript=$(echo "$input" | jq -r '.transcript_path // empty')
cwd=$(echo "$input" | jq -r '.workspace.current_dir // empty')
agent=$(echo "$input" | jq -r '.agent.name // empty')
# model is a JSON object with .id and .display_name, not a plain string
model=$(echo "$input" | jq -r '.model.display_name // .model // empty')

# Use pre-computed cost from Claude Code, rounded to whole dollar
total_cost_raw=$(echo "$input" | jq -r '.cost.total_cost_usd // 0')
total_cost=$(printf '%.0f' "$total_cost_raw" 2>/dev/null || echo 0)

# Build 10-char context bar
filled=$((used_pct / 10))
empty=$((10 - filled))
bar_filled=""
bar_empty=""
for ((i=0; i<filled; i++)); do bar_filled+="█"; done
for ((i=0; i<empty; i++)); do bar_empty+="░"; done

# Format token counts with k suffix for readability
fmt_tokens() {
  local n=$1
  if [ "$n" -ge 1000 ] 2>/dev/null; then
    echo "$((n / 1000))k"
  else
    echo "$n"
  fi
}

ti=$(fmt_tokens "$total_input")
to=$(fmt_tokens "$total_output")
li=$(fmt_tokens "$last_input")
lo=$(fmt_tokens "$last_output")

# Colors
RST='\x1b[0m'
CYN='\x1b[36m'
GRN='\x1b[1;32m'
YLW='\x1b[1;33m'
DIM='\x1b[2m'
BAR_FILL='\x1b[1;32m'
BAR_EMPTY='\x1b[2;37m'

# Extract first line of last user message from transcript
last_msg=""
if [ -n "$transcript" ] && [ -f "$transcript" ]; then
  last_msg=$(python3 -c "
import json, sys, signal
signal.signal(signal.SIGPIPE, signal.SIG_DFL)
msg = ''
with open(sys.argv[1]) as f:
    for line in f:
        line = line.strip()
        if not line: continue
        try:
            obj = json.loads(line)
            if obj.get('type') == 'user':
                content = obj.get('message', {}).get('content', '')
                if isinstance(content, list):
                    for part in content:
                        if isinstance(part, dict) and part.get('type') == 'text':
                            msg = part['text'].split(chr(10))[0]
                            break
                elif isinstance(content, str):
                    msg = content.split(chr(10))[0]
        except (json.JSONDecodeError, KeyError):
            pass
print(msg[:120])
" "$transcript" 2>/dev/null)
fi

# Line 1: model [bar] pct% | in/out tokens | last in/out tokens | $cost
printf "${CYN}%s${RST} [${BAR_FILL}%s${BAR_EMPTY}%s${RST}] ${CYN}%s%%${RST}" \
    "$model" "$bar_filled" "$bar_empty" "$used_pct"
printf " | ${GRN}in:${RST}%s / ${GRN}out:${RST}%s" "$ti" "$to"
printf " | ${DIM}last${RST} ${GRN}in:${RST}%s / ${GRN}out:${RST}%s" "$li" "$lo"
printf " | ${YLW}\$%s${RST}\n" "$total_cost"

# Line 2: cwd and agent name
printf "${DIM}cwd:${RST} ${GRN}%s${RST}" "$cwd"
if [ -n "$agent" ]; then
    printf " | ${DIM}agent:${RST} ${CYN}%s${RST}" "$agent"
fi
printf "\n"

# Line 3: transcript path
printf "${DIM}transcript: %s${RST}\n" "$transcript"

# Line 4: first line of last user message
if [ -n "$last_msg" ]; then
    printf "${DIM}prompt:${RST} ${CYN}%s${RST}\n" "$last_msg"
fi
