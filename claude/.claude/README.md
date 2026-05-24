# Claude Code tools

Notes on tools and integrations layered on top of Claude Code.

## graphify

[graphify](https://github.com/safishamsi/graphify) turns a folder of code
and docs into a queryable knowledge graph, so the assistant can answer
architectural questions without re-reading every file. It produces an
interactive `graph.html`, a `GRAPH_REPORT.md`, and a `graph.json` (also
usable as an MCP server).

### Install

```sh
uv tool install graphifyy   # PyPI package is "graphifyy" (double-y)
graphify install            # register with Claude Code
```

### Use

From the project root inside Claude Code:

```
/graphify .                       # build graph
/graphify query "auth flow"       # ask questions against it
```

Everything else — flags, querying, PR workflows, MCP server, headless
backends, `.graphifyignore` — see the [official
README](https://github.com/safishamsi/graphify).
