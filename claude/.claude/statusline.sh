#!/usr/bin/env bash

# Status line command for Claude Code
# Displays:
# dotfiles/subdir  work  · model-id  󰳿 0%   +3/-2   $0.46

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

# Constants
DEFAULT_CONTEXT_WINDOW_SIZE=200000
TOKENS_PER_MILLION=1000000

set -euo pipefail

input=$(cat)

# Extract workspace information
current_dir=$(jq -r '.workspace.current_dir // .cwd' <<< "$input")
model_id=$(jq -r '.model.id' <<< "$input")

# Convert model ID to friendly display name
# claude-sonnet-4-5-20250929 → Sonnet 4.5
# claude-opus-4-5-20251101 → Opus 4.5
if [[ "$model_id" =~ ^claude-([a-z]+)-([0-9]+-[0-9]+) ]]; then
    model_name="${BASH_REMATCH[1]^}"  # Capitalize first letter
    model_version="${BASH_REMATCH[2]//-/.}"  # Replace dash with dot
    model_display="$model_name $model_version"
else
    model_display="$model_id"
fi

# Initialize context variables
context_pct=0

# Calculate context usage
usage=$(jq '.context_window.current_usage' <<< "$input")
if [[ "$usage" != "null" && -n "$usage" ]]; then
    # Sum all input tokens for current context
    current_tokens=$(jq '(.input_tokens // 0) + (.cache_creation_input_tokens // 0) + (.cache_read_input_tokens // 0)' <<< "$usage")
    window_size=$(jq ".context_window.context_window_size // $DEFAULT_CONTEXT_WINDOW_SIZE" <<< "$input")
    context_pct=$((current_tokens * 100 / window_size))
fi

# Get session cost from Claude Code's pre-calculated value
session_cost_usd=$(jq '.cost.total_cost_usd // 0' <<< "$input")
session_cost=$(printf "%.2f" "$session_cost_usd")

# Get git info if in a git repository
git_branch=""
lines_added=0
lines_removed=0
if git -C "$current_dir" rev-parse --git-dir >/dev/null 2>&1; then
    # Get current branch name
    git_branch=$(git -C "$current_dir" branch --show-current 2>/dev/null || echo "")

    # Count lines added and removed (unstaged changes only)
    while IFS=$'\t' read -r added removed _file; do
        [[ "$added" =~ ^[0-9]+$ ]] && ((lines_added += added))
        [[ "$removed" =~ ^[0-9]+$ ]] && ((lines_removed += removed))
    done < <(git -C "$current_dir" diff --numstat 2>/dev/null)
fi

# Shorten home directory to ~ and show only last 2 directory levels
temp_dir="${current_dir/#$HOME/~}"
# Extract last 2 directory levels
display_dir=$(echo "$temp_dir" | awk -F'/' '{
    if (NF <= 2) print $0
    else print $(NF-1) "/" $NF
}')

# Build status line string
status_line="${RESET}${BRIGHT_CYAN}${display_dir}${RESET}"
[[ -n "$git_branch" ]] && status_line+=" ${BRIGHT_MAGENTA} ${git_branch}${RESET}"
status_line+=" ${DIM}·${RESET} ${YELLOW}${model_display}${RESET}  ${BLUE}󰳿 ${context_pct}%${RESET}"

if ((lines_added > 0 || lines_removed > 0)); then
    status_line+="  ${DIM} ${GREEN}+${lines_added}/${RED}-${lines_removed}${RESET}"
fi

status_line+="  ${WHITE}${DIM}\$${session_cost}${RESET}"

# Output status line
printf "%b" "$status_line"
