# Repository Guidelines

## Project Overview

Personal macOS/Linux dotfiles. The repo's job is to materialize tracked config
files into `$HOME` via GNU Stow symlinks, and to bootstrap machines (packages,
shell, third-party plugins) via `just` recipes.

Supported targets: macOS (primary, Homebrew), Arch Linux (pacman), Debian/Ubuntu
(apt). OS is auto-detected in `justfile`.

## Architecture & Data Flow

- `.stowrc` supplies global flags to every `stow` invocation: `--no-folding`
  (link individual files, never fold whole dirs — target dirs are shared with
  tool-generated files), `--dotfiles`, `--target=~`.
- Package membership is an explicit space-separated string in `justfile`
  (`packages_cli`, `packages_gui`). **There is no auto-discovery** — a new
  package dir is inert until added to one of those variables.
- Per-package `.stow-local-ignore` (regex per line) keeps Stow away from runtime
  state and secrets that live in the same tree: `fish/`, `ssh/`, `wireshark/`,
  `yazi/`, `claude/`.
- `obsidian/` is **not** a Stow package. It is rsync-synced with a vault via
  `just obsidian-config push|pull <vault-path>` (excludes `workspace*.json`).
- Third-party plugins are never vendored; each ecosystem has its own idempotent
  `plugins-*` recipe that fetches from the network.

## Development Commands

```sh
just                       # default -> config: stow everything + install plugins
just install               # full bootstrap: deps + config + set-shell (+ macOS tweaks)
just config-check          # DRY RUN: stow -nvv, detects conflicts — run before applying
just reconfig              # stow -Rvv after changing which files a package tracks
just unconfig              # stow -Dvv, remove all symlinks
just install-deps          # Brewfile / pacman / apt, per detected OS
just plugins               # -> plugins-{tmux,yazi,claude,omp,nvim,fish}
just set-hostname <name>   # macOS only
just obsidian-config pull ~/path/to/vault
```

Recipe graph: `default → config`; `config → config-cli, config-gui, plugins`;
`install → install-deps, config, set-shell`. `just install` must not re-declare
`plugins` — it arrives transitively through `config`.

## Code Conventions & Common Patterns

- **Guard every interactive shell function.** Fish files open with
  `if status is-interactive; and type -q <tool>` so a missing tool degrades to a
  no-op. See `fish/.config/fish/functions/y.fish`, `eza.fish`, `claude.fish`.
- **One fish function per file**, filename == function name (autoload
  requirement). Private helpers are `__`-prefixed: `__git_ref_records`,
  `__git_select_ref_widget`.
- **`alias`/`function --wraps=`, never `abbr`** — zero `abbr` usage repo-wide;
  do not introduce the second convention.
- **Machine-local overrides are untracked and optional**, always `*.local*`:
    - fish: `config.$hostname.fish`, `config.local.fish` sourced at end of
      `config.fish`.
    - git: `[includeIf "gitdir:~/"] → ~/.config/git/config.local` and
      `gitdir:~/workspace/ → config.workspace.local`. Never commit these; add
      new secrets/host state to `.gitignore` or a `.stow-local-ignore` instead.
- **Banner comment section headers** organize otherwise flat files:
  `# ENVIRONMENT`, `# ALIASES`, `# BINDINGS` (fish), `# APPEARANCE`,
  `# KEY BINDINGS` (ghostty), `-- VIM OPTIONS`, `-- PLUGINS` (lua).
- **Minimal/native plugin managers**: nvim uses built-in `vim.pack.add` (Neovim
  0.12+), not lazy.nvim; fish uses Fisher; tmux uses TPM.
- **Shared "One Dark" theme** threaded across ghostty, tmux (hardcoded hex),
  nvim (`onedark.nvim`), zed (`theme_overrides`), yazi (`flavors/onedark.yazi`),
  bat, starship. Color changes must be applied across all of them.
- **Idempotent installers.** `plugins-omp` wraps `omp plugin` in a helper that
  swallows "already exists"/"already installed"; `plugins-tmux` tests for the
  TPM dir; `plugins-yazi` tests `command -v ya`. New `plugins-*` recipes must be
  re-runnable.
- **Installation never mutates tracked config.** `plugins-claude` finishes with
  `git checkout -- claude/.claude/settings.json` because `plugin install`
  force-enables every plugin; the curated enabled/disabled map lives in VCS.
- **macOS gating**: standalone recipes use the `[macos]` attribute; steps inside
  a multi-OS recipe body use `if [[ "{{ os }}" == "macos" ]]`.
- **Markdown** (see `claude/.claude/rules/markdown-formatting.md`): wrap prose
  at 80 columns; exempt code fences, tables, and lone URLs; align table pipes.
- **Commit messages**: scope-prefixed subject matching the package/file, e.g.
  `justfile: Remove andrej-karpathy-skills plugin`,
  `claude: Rename marketplace identifier`,
  `omp: Remove claude-mem and update configuration`. Rules in
  `claude/.claude/rules/commit-messages.md`.

## Runtime/Tooling Preferences

- Task runner: **`just`** (`set shell := ["bash", "-cu"]`; recipe bodies that
  need real logic start with `#!/usr/bin/env bash` + `set -euo pipefail`).
- Symlink manager: **GNU Stow**. Never hand-create symlinks in `$HOME`.
- Package manager: **Homebrew via `Brewfile`** on macOS; `deps_arch` /
  `deps_ubuntu` strings for Linux. Adding a tool means editing `Brewfile` _and_
  the relevant deps string.
- Shell: **fish** (set as login shell by `just set-shell`). Editor: **nvim**
  (`VISUAL`/`EDITOR`). Python tooling via **uv**. JS runtime available: **bun**.
- On non-macOS the justfile prepends `~/.local/bin` to `PATH` (curl-installed
  `uv`/`starship` land there).
