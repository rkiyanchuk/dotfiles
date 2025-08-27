#!/usr/bin/env bash

# Color variables
BLACK="\033[30m"
RED="\033[31m"
GREEN="\033[32m"
YELLOW="\033[33m"
BLUE="\033[34m"
MAGENTA="\033[35m"
CYAN="\033[36m"
WHITE="\033[37m"

# Bright colors
BRIGHT_BLACK="\033[90m"
BRIGHT_RED="\033[91m"
BRIGHT_GREEN="\033[92m"
BRIGHT_YELLOW="\033[93m"
BRIGHT_BLUE="\033[94m"
BRIGHT_MAGENTA="\033[95m"
BRIGHT_CYAN="\033[96m"
BRIGHT_WHITE="\033[97m"

# Bold colors
BOLD_BLACK="\033[1;30m"
BOLD_RED="\033[1;31m"
BOLD_GREEN="\033[1;32m"
BOLD_YELLOW="\033[1;33m"
BOLD_BLUE="\033[1;34m"
BOLD_MAGENTA="\033[1;35m"
BOLD_CYAN="\033[1;36m"
BOLD_WHITE="\033[1;37m"


# Text formatting
BOLD="\033[1m"
DIM="\033[2m"
ITALIC="\033[3m"
UNDERLINE="\033[4m"
BLINK="\033[5m"
REVERSE="\033[7m"
STRIKETHROUGH="\033[9m"

# Reset
RESET="\033[0m"

claude_context_tokens_limit=200000

# Read JSON input from stdin
input=$(cat)

# Get Claude Code session info
session_id=$(echo "$input" | jq -r '.session_id // "unknown"')
model=$(echo "$input" | jq -r '.model.display_name')
cwd=$(echo "$input" | jq -r '.cwd')
project_dir=$(echo "$input" | jq -r '.workspace.project_dir')
output_style=$(echo "$input" | jq -r '.output_style.name')
lines_added=$(echo "$input" | jq -r '.cost.total_lines_added // 0')
lines_removed=$(echo "$input" | jq -r '.cost.total_lines_removed // 0')

today=$(date +%Y%m%d)

session_data=$(echo "$input" | npx ccusage session --json --id $session_id  --cost-source cc)
cost_session=$(printf "%.2f" $(echo "$session_data" | jq .totalCost))
cost_today=$(printf "%.2f" $(echo "$input" | npx ccusage daily --json --since $today --cost-source cc | jq '.daily[].totalCost'))

context_info=$(echo "$input" | npx ccusage statusline --cost-source cc | cut -d'🧠' -f 2)

# Directory
if [[ "$project_dir" == "$HOME" ]]; then
    dir_display="~"
elif [[ "$project_dir" == "$HOME"/* ]]; then
    dir_display="~/${project_dir#$HOME/}"
else
    dir_display="$project_dir"
fi

# Git
if git rev-parse --git-dir >/dev/null 2>&1; then
    branch=$(git symbolic-ref --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)
    if [[ -n "$branch" ]]; then
        git_info=$' '" ${branch}"
        dir_display=$(basename "$project_dir")
    fi
fi

# Output style
if [ "$output_style" == "default" ]; then
    output_style=""
else
    output_style=" : ${GREEN}${output_style}${RESET}"
fi

if [[ "$context_info" == *"N/A"* ]]; then
    context_display=" ${BLUE}${DIM}󰳿 0${RESET}"
else
    context_tokens=$(echo $context_info | cut -d' ' -f 1 | tr -d ',')
    context_tokens_kilo=$(echo "scale=1; $context_tokens / 1000" | bc | sed 's/\.0$//')K
    context_percent=$(echo $context_info | cut -d' ' -f 2 | tr -d '()')
    context_display=" ${BLUE}${DIM}󰳿 ${context_percent} · ${context_tokens_kilo}${RESET}"
fi

# Build the complete status line similar to original
status_line="${RESET}• ${BRIGHT_CYAN}${dir_display}${RESET}"
status_line="${status_line}${BRIGHT_MAGENTA}${git_info}${RESET}"
status_line="${status_line} · ${YELLOW}${model}${RESET}"
status_line="${status_line}${output_style}"
status_line="${status_line}  ${DIM} ${GREEN}${lines_added}+${WHITE}/${RED}${lines_removed}-${RESET}"
status_line="${status_line}${context_display}"
status_line="${status_line}  ${WHITE}${DIM} \$${cost_session}${RESET}"
status_line="${status_line} ${WHITE}${DIM}󰃭 \$${cost_today}${RESET}"

echo -e "$status_line"

# Debug:
# echo -e "| ccusage statusline: $(echo $input | npx ccusage statusline --cost-source cc)"
