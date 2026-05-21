#!/usr/bin/env bash
# Register Claude Code MCP servers in user scope.
#
# Secrets are loaded from claude/.env (gitignored). Copy claude/.env.example
# to claude/.env and fill in the values, then run `just claude-mcp`.
#
# The resolved values land in ~/.claude.json which is itself gitignored — this
# script is the checked-in source of truth for *which* MCPs you want.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ENV_FILE="$SCRIPT_DIR/.env"

if [[ ! -f "$ENV_FILE" ]]; then
    echo "Missing $ENV_FILE — copy $SCRIPT_DIR/.env.example and fill in values" >&2
    exit 1
fi

set -a
# shellcheck disable=SC1090
source "$ENV_FILE"
set +a

add_mcp() {
    local name="$1"
    local json="$2"
    claude mcp remove "$name" --scope user 2>/dev/null || true
    claude mcp add-json "$name" --scope user "$json"
}

add_mcp discord "$(jq -n \
    --arg email "${DISCORD_EMAIL:-}" \
    --arg password "${DISCORD_PASSWORD:-}" \
    '{
        command: "uvx",
        args: ["--from", "git+https://github.com/elyxlz/discord-mcp.git", "discord-mcp"],
        env: {
            DISCORD_EMAIL: $email,
            DISCORD_PASSWORD: $password,
            DISCORD_HEADLESS: "true"
        }
    }')"

add_mcp nano-banana "$(jq -n \
    --arg key "${GEMINI_API_KEY:-}" \
    '{
        command: "uvx",
        args: ["nanobanana-mcp-server@latest"],
        env: {GEMINI_API_KEY: $key}
    }')"

add_mcp lightrag "$(jq -n \
    --arg url "${LIGHTRAG_BASE_URL:-}" \
    '{
        command: "npx",
        args: ["-y", "@g99/lightrag-mcp-server"],
        env: {LIGHTRAG_BASE_URL: $url}
    }')"

echo
echo "Registered MCP servers:"
claude mcp list
