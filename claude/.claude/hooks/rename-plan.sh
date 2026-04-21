#!/bin/bash
# Hook to rename Claude Code plan files to: <repo>_<feature>.md
# Triggered after Write tool creates/modifies a plan file

set -euo pipefail

PLANS_DIR="$HOME/.claude/plans"
HOOK_INPUT=$(cat)

# Extract the file path from the hook input (JSON)
FILE_PATH=$(echo "$HOOK_INPUT" | jq -r '.tool_input.file_path // empty')

# Only process files in the plans directory
if [[ -z "$FILE_PATH" ]] || [[ "$FILE_PATH" != "$PLANS_DIR"/* ]]; then
    exit 0
fi

# Skip if file doesn't exist or is already renamed (contains repo_feature pattern)
FILENAME=$(basename "$FILE_PATH")
if [[ ! -f "$FILE_PATH" ]] || [[ "$FILENAME" =~ ^_[a-z0-9._-]+_[a-z0-9-]+\.md$ ]]; then
    exit 0
fi

# Get the repo/directory from CWD passed in hook context, or use 'unknown'
CWD=$(echo "$HOOK_INPUT" | jq -r '.cwd // empty')
if [[ -n "$CWD" ]]; then
    REPO=$(basename "$CWD")
else
    REPO="unknown"
fi

# Extract feature from the plan content (first heading or first line)
FIRST_HEADING=$(head -20 "$FILE_PATH" | grep -m1 '^#' | sed 's/^#* *//' | tr '[:upper:]' '[:lower:]')

if [[ -z "$FIRST_HEADING" ]]; then
    # Fallback: use first non-empty line
    FIRST_HEADING=$(head -5 "$FILE_PATH" | grep -m1 '.' | tr '[:upper:]' '[:lower:]')
fi

# Clean up the feature name: remove special chars, truncate, convert to kebab-case
FEATURE=$(echo "$FIRST_HEADING" | \
    sed 's/[^a-z0-9 ]//g' | \
    sed 's/  */ /g' | \
    sed 's/^ *//;s/ *$//' | \
    cut -d' ' -f1-5 | \
    tr ' ' '-' | \
    cut -c1-50)

# Fallback if feature is empty
if [[ -z "$FEATURE" ]]; then
    FEATURE="plan"
fi

# Construct new filename: <repo>_<feature>.md
NEW_FILENAME="_${REPO}_${FEATURE}.md"
NEW_PATH="${PLANS_DIR}/${NEW_FILENAME}"

# Skip if a symlink already points to this file
for existing in "$PLANS_DIR"/_*.md; do
    if [[ -L "$existing" ]] && [[ "$(readlink "$existing")" == "$FILE_PATH" ]]; then
        exit 0
    fi
done

# Avoid overwriting existing files
if [[ -f "$NEW_PATH" ]] || [[ -L "$NEW_PATH" ]]; then
    # Add a numeric suffix
    COUNTER=1
    while [[ -f "${PLANS_DIR}/_${REPO}_${FEATURE}-${COUNTER}.md" ]] || [[ -L "${PLANS_DIR}/_${REPO}_${FEATURE}-${COUNTER}.md" ]]; do
        ((COUNTER++))
    done
    NEW_PATH="${PLANS_DIR}/_${REPO}_${FEATURE}-${COUNTER}.md"
fi

# Create a descriptive symlink to the original file
ln -sf "$FILE_PATH" "$NEW_PATH"

# Output message for Claude to see
echo "Linked plan: $(basename "$NEW_PATH") -> $(basename "$FILE_PATH")"
