#!/usr/bin/env bash
# Normalize settings.json key order after Claude Code rewrites it.
#
# Must be idempotent: ConfigChange fires whenever settings.json is written,
# including writes by THIS script. A non-idempotent normalizer creates an
# infinite loop (hook writes -> ConfigChange fires -> hook writes -> ...).
# Skipping the write when the file is already normalized breaks the cycle.
#
# Uses redirection (not mv) to preserve the Stow symlink.
settings="$HOME/.claude/settings.json"
[[ -f "$settings" ]] || exit 0

cat >/dev/null  # drain stdin so the hook engine doesn't block on the pipe

current=$(cat "$settings")
normalized=$(jq --sort-keys . <<<"$current") || exit 0

[[ "$current" == "$normalized" ]] && exit 0

printf '%s\n' "$normalized" > "$settings"
