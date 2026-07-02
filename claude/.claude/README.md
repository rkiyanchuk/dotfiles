# Claude Code customization

## Files

| Path                     | What it does                                                       |
| ------------------------ | ------------------------------------------------------------------ |
| `settings.json`          | Main config                                                        |
| `CLAUDE.md`              | Global instructions for all projects.                              |
| `rules/`                 | Topic rules auto-loaded at launch (e.g. commit-message style).     |
| `output-styles/brief.md` | Custom **Brief** output style.                                     |
| `statusline.sh`          | Two-line status line: dir, branch, model, context %, cost, counts. |

## settings.json

- **Plugins** — enabled via `enabledPlugins`; `just plugins-claude` installs
  them.
- **Permissions** — `Read`/`WebFetch`/`WebSearch` and read-only GitHub MCP tools
  auto-allowed; two Obsidian vaults added as additional directories.
- **Output style** — `Brief`.

## Enabled plugins

| Plugin              | Purpose                                                 |
| ------------------- | ------------------------------------------------------- |
| `claude-mem`        | Persistent cross-session memory and observation search. |
| `obsidian`          | Skills for reading and writing the Obsidian vaults.     |
| `security-guidance` | Guardrails for security-sensitive requests.             |
| `context7`          | Fetches up-to-date library/framework docs on demand.    |

LSP and review plugins (`*-lsp`, `pr-review-toolkit`, `chrome-devtools-mcp`,
`skill-creator`) are installed but disabled; flip them on per project as needed.
