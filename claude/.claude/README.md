# Claude Code customization

## Files

| Path                     | What it does |
| ------------------------ | -------------|
| `settings.json`          | Main config  |
| `CLAUDE.md`              | Global instructions for all projects. |
| `rules/`                 | Topic rules auto-loaded at launch (e.g. commit-message style). |
| `output-styles/brief.md` | Custom **Brief** output style. |
| `statusline.sh`          | Two-line status line: dir, branch, model, context %, cost, counts. |

## settings.json

- **Plugins** — enabled via `enabledPlugins`; `just plugins-claude` installs
  them.
- **Permissions** — `Read`/`WebFetch`/`WebSearch` and read-only GitHub MCP
  tools auto-allowed; two Obsidian vaults added as additional directories.
- **Output style** — `Brief`.

## Enabled plugins

| Plugin                   | Purpose                                                       |
| ------------------------ | ------------------------------------------------------------- |
| `claude-mem`             | Persistent cross-session memory and observation search.       |
| `obsidian`               | Skills for reading and writing the Obsidian vaults.           |
| `andrej-karpathy-skills` | Behavioral guidelines that reduce common LLM coding mistakes. |
| `security-guidance`      | Guardrails for security-sensitive requests.                   |
| `context7`               | Fetches up-to-date library/framework docs on demand.          |

LSP and review plugins (`*-lsp`, `pr-review-toolkit`, `chrome-devtools-mcp`,
`skill-creator`) are installed but disabled; flip them on per project as needed.

## Graphify

[graphify](https://github.com/safishamsi/graphify) turns a folder of code
and docs into a queryable knowledge graph, so the assistant can answer
architectural questions without re-reading every file. It produces an
interactive `graph.html`, a `GRAPH_REPORT.md`, and a `graph.json` (also
usable as an MCP server).

```sh
uv tool install graphifyy   # PyPI package is "graphifyy" (double-y)
graphify install            # register with Claude Code
```

From the project root inside Claude Code:

```sh
/graphify .                       # build graph
/graphify query "auth flow"       # ask questions against it
```

Everything else — flags, querying, PR workflows, MCP server, headless
backends, `.graphifyignore` — see the [official README](https://github.com/safishamsi/graphify).
