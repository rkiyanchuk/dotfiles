#!/usr/bin/env bash

# Status line command for Claude Code
# Line 1: dir  branch · model  󰳿 <context-bar> N% ·  <usage-bar> N%  +/-diff  $cost
# Line 2: N rules │ N MCPs │ N hooks │ N skills │ N plugins │ N allowed [│ style: X]

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
model_id=$(jq -r '.model.id // ""' <<< "$input")
model_display_name=$(jq -r '.model.display_name // ""' <<< "$input")

# Detect provider via env vars
provider_label=""
[[ "${CLAUDE_CODE_USE_BEDROCK:-}" == "1" ]] && provider_label="Bedrock"
[[ "${CLAUDE_CODE_USE_VERTEX:-}" == "1" ]] && provider_label="Vertex"

# Convert model ID to friendly display name (matches claude-hud logic)
if [[ -n "$model_display_name" ]]; then
    model_display="$model_display_name"
else
    id_lower="${model_id,,}"
    case "$id_lower" in
        opusplan)              model_display="Opus" ;;
        sonnetplan)            model_display="Sonnet" ;;
        haikuplan)             model_display="Haiku" ;;
        *"anthropic.claude-"*)
            [[ -z "$provider_label" ]] && provider_label="Bedrock"
            suffix="${id_lower#*anthropic.claude-}"
            suffix="${suffix%-v[0-9]*:[0-9]*}"                    # strip -vN:N
            suffix=$(sed 's/-[0-9]\{8\}$//' <<< "$suffix")       # strip -YYYYMMDD
            if   [[ "$suffix" =~ ([a-z]+)-([0-9]+)-([0-9]+) ]]; then
                model_display="${BASH_REMATCH[1]^} ${BASH_REMATCH[2]}.${BASH_REMATCH[3]}"
            elif [[ "$suffix" =~ ([a-z]+)-([0-9]+) ]]; then
                model_display="${BASH_REMATCH[1]^} ${BASH_REMATCH[2]}"
            else
                model_display="${suffix^}"
            fi ;;
        *"@"*)
            [[ -z "$provider_label" ]] && provider_label="Vertex"
            base="${model_id%@*}"
            if [[ "$base" =~ claude-([a-z]+)-([0-9]+)-([0-9]+) ]]; then
                model_display="${BASH_REMATCH[1]^} ${BASH_REMATCH[2]}.${BASH_REMATCH[3]}"
            else
                model_display="$base"
            fi ;;
        *)
            if [[ "$model_id" =~ claude-([a-z]+)-([0-9]+)-([0-9]+) ]]; then
                model_display="${BASH_REMATCH[1]^} ${BASH_REMATCH[2]}.${BASH_REMATCH[3]}"
            else
                model_display="${model_id:-Unknown}"
            fi ;;
    esac
fi

# Initialize context variables
context_pct=0

# Calculate context usage
usage=$(jq '.context_window.current_usage' <<< "$input")
if [[ "$usage" != "null" && -n "$usage" ]]; then
    # Prefer native used_percentage (Claude Code v2.1.6+)
    native_pct=$(jq -r '.context_window.used_percentage // empty' <<< "$input")
    if [[ -n "$native_pct" && "$native_pct" != "0" ]]; then
        context_pct=${native_pct%.*}
    else
        current_tokens=$(jq '(.input_tokens // 0) + (.cache_creation_input_tokens // 0) + (.cache_read_input_tokens // 0)' <<< "$usage")
        window_size=$(jq ".context_window.context_window_size // $DEFAULT_CONTEXT_WINDOW_SIZE" <<< "$input")
        context_pct=$((current_tokens * 100 / window_size))
    fi
fi

# Parse 5-hour usage rate-limit (v2.1.6+; silently absent on Bedrock/older builds)
usage_pct=""
if jq -e '.rate_limits.five_hour' <<< "$input" >/dev/null 2>&1; then
    usage_pct=$(jq -r '.rate_limits.five_hour.used_percentage // empty' <<< "$input")
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

    # Count lines added and removed vs HEAD (staged + unstaged)
    while IFS=$'\t' read -r added removed _file; do
        [[ "$added" =~ ^[0-9]+$ ]] && ((lines_added += added)) || true
        [[ "$removed" =~ ^[0-9]+$ ]] && ((lines_removed += removed)) || true
    done < <(git -C "$current_dir" -c core.quotePath=false diff --numstat HEAD 2>/dev/null)
fi

# Shorten home directory to ~ and show only last 2 directory levels
temp_dir="${current_dir/#$HOME/~}"
# Extract last 2 directory levels
display_dir=$(echo "$temp_dir" | awk -F'/' '{
    if (NF <= 2) print $0
    else print $(NF-1) "/" $NF
}')

pct_color() {
    if   (($1 < 25));  then printf "%b" "\033[2;37m"             # dim grey
    elif (($1 < 50));  then printf "%b" "\033[2;33m"             # dim yellow
    elif (($1 < 70));  then printf "%b" "\033[2m\033[38;5;208m"  # dim orange
    elif (($1 < 85));  then printf "%b" "\033[2;35m"             # dim purple
    else                    printf "%b" "\033[2;31m"             # dim red
    fi
}

# Output style — last writer wins (project local overrides user)
output_style=""
for f in \
    "$HOME/.claude/settings.json" \
    "$HOME/.claude/settings.local.json" \
    "$current_dir/.claude/settings.json" \
    "$current_dir/.claude/settings.local.json"; do
    [[ -f "$f" ]] || continue
    v=$(jq -r '.outputStyle // empty' "$f" 2>/dev/null) || continue
    [[ -n "$v" ]] && output_style="$v"
done

# Color by output style value (skipped when default/empty)
output_style_color=""
if [[ -n "$output_style" && "$output_style" != "default" ]]; then
    case "${output_style,,}" in
        proactive)   output_style_color="\033[38;5;208m" ;;  # orange
        explanatory) output_style_color="${GREEN}" ;;        # green
        learning)    output_style_color="${BLUE}" ;;         # blue
        *)           output_style_color="\033[90m" ;;        # grey
    esac
fi

# --- Build line 1 ---
status_line="${RESET}${BRIGHT_CYAN}${display_dir}${RESET}"
[[ -n "$git_branch" ]] && status_line+=" ${BRIGHT_MAGENTA} ${git_branch}${RESET}"
status_line+=" ${DIM}·${RESET} ${GREEN}${model_display}${RESET}"
[[ -n "$provider_label" ]] && status_line+=" ${DIM}: ${provider_label}${RESET}"
[[ -n "$output_style_color" ]] && status_line+=" ${DIM}:${RESET} ${output_style_color}${output_style}${RESET}"

status_line+=" ${DIM}· ${BLUE}󰳿${RESET} $(pct_color "$context_pct")${context_pct}%${RESET}"

# Usage percentage (omitted when stdin has no rate_limits)
if [[ -n "$usage_pct" ]]; then
    usage_int=${usage_pct%.*}
    usage_int=${usage_int:-0}
    status_line+="  ${DIM}${BLUE} $(pct_color "$usage_int")${usage_int}%${RESET}"
fi

if ((lines_added > 0 || lines_removed > 0)); then
    status_line+="  ${DIM} ${GREEN}+${lines_added}/${RED}-${lines_removed}${RESET}"
fi

status_line+="  ${WHITE}${DIM}\$${session_cost}${RESET}"

# --- Build line 2: environment counts ---
project_dir="$current_dir"

# Rules — .md files under user + project .claude/rules
rules_count=0
for d in "$HOME/.claude/rules" "$project_dir/.claude/rules"; do
    [[ -d "$d" ]] || continue
    rules_count=$(( rules_count + $(find "$d" -type f -name '*.md' 2>/dev/null | wc -l | tr -d ' ') ))
done

# MCPs — deduplicated keys across user + project scopes, minus disabled
mcp_count=$( {
    for f in \
        "$HOME/.claude.json" \
        "$HOME/.claude/settings.json" \
        "$project_dir/.claude/settings.json" \
        "$project_dir/.claude/settings.local.json" \
        "$project_dir/.mcp.json"; do
        [[ -f "$f" ]] || continue
        jq -r '(.mcpServers // {}) | keys[]' "$f" 2>/dev/null
    done
} | sort -u | wc -l | tr -d ' ')

# Hooks — sum of top-level event keys (PreToolUse, Stop, …) across settings files
hooks_count=0
for f in \
    "$HOME/.claude/settings.json" \
    "$project_dir/.claude/settings.json" \
    "$project_dir/.claude/settings.local.json"; do
    [[ -f "$f" ]] || continue
    n=$(jq -r '(.hooks // {}) | keys | length' "$f" 2>/dev/null || echo 0)
    hooks_count=$(( hooks_count + n ))
done

# Skills — SKILL.md files under each enabled plugin's skills dirs + user skills dir
skills_count=0
[[ -d "$HOME/.claude/skills" ]] && \
    skills_count=$(find "$HOME/.claude/skills" -iname 'SKILL.md' 2>/dev/null | wc -l | tr -d ' ')
while IFS= read -r entry; do
    plugin_name="${entry%@*}"
    marketplace="${entry##*@}"
    for sd in \
        "$HOME/.claude/plugins/marketplaces/$marketplace/plugins/$plugin_name/skills" \
        "$HOME/.claude/plugins/marketplaces/$marketplace/skills"; do
        [[ -d "$sd" ]] || continue
        n=$(find "$sd" -iname 'SKILL.md' 2>/dev/null | wc -l | tr -d ' ')
        skills_count=$(( skills_count + n ))
    done
done < <(jq -r '.enabledPlugins // {} | to_entries[] | select(.value==true) | .key' \
    "$HOME/.claude/settings.json" 2>/dev/null)

# Plugins — enabled count
plugins_count=$(jq -r '[.enabledPlugins // {} | to_entries[] | select(.value==true)] | length' \
    "$HOME/.claude/settings.json" 2>/dev/null || echo 0)

# CLAUDE.md files — mirrors claude-hud's computeConfigCountsFresh logic
claudemd_paths=()
user_claude_dir="$HOME/.claude"
for f in \
    "$user_claude_dir/CLAUDE.md" \
    "$project_dir/CLAUDE.md" \
    "$project_dir/CLAUDE.local.md" \
    "$project_dir/.claude/CLAUDE.local.md"; do
    [[ -f "$f" ]] && claudemd_paths+=("${f/#$HOME/~}") || true
done
# project/.claude/CLAUDE.md — only when it doesn't overlap with user scope
if [[ "$project_dir/.claude" != "$user_claude_dir" ]]; then
    [[ -f "$project_dir/.claude/CLAUDE.md" ]] && claudemd_paths+=("${project_dir/#$HOME/~}/.claude/CLAUDE.md") || true
fi
claudemd_count=${#claudemd_paths[@]}

env_line="${DIM}${claudemd_count} mems ·${rules_count} rules · ${hooks_count} hooks · ${mcp_count} mcps · ${skills_count} skills · ${plugins_count} plugins${RESET}"

# Output both lines
printf "%b\n%b" "$status_line" "$env_line"
