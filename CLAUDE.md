# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```sh
just             # Full setup from scratch (deps + config + shell + plugins)
just deps        # Install system dependencies (Homebrew on macOS, pacman/apt on Linux)
just config      # Symlink all dotfiles via GNU Stow
just config-cli  # Symlink CLI packages only
just config-gui  # Symlink GUI packages (macOS only)
just reconfig    # Restow after modifying package structure
just config-check # Dry-run: preview what Stow would symlink
just unconfig    # Remove all stowed symlinks
just plugins     # Install all third-party plugins (tmux TPM, yazi, Claude plugins, neovim)
just shell       # Set Fish as default shell
```

## Architecture

**Package manager**: [GNU Stow](https://www.gnu.org/software/stow/) symlinks each top-level directory as a package into `$HOME`. For example, `fish/.config/fish/config.fish` → `~/.config/fish/config.fish`. Each package may have a `.stow-local-ignore` to exclude runtime/user-generated files from being stowed.

**Task runner**: `just` (Justfile at repo root). OS detection at runtime via `path_exists("/etc/arch-release")` for Arch vs. macOS fallback.

**Packages**: `bat`, `claude`, `direnv`, `fish`, `gh`, `ghostty`, `git`, `glances`, `grc`, `nvim`, `ssh`, `starship`, `tmux`, `yazi`.

### Claude Code package (`claude/`)

Stow target is `~/.claude/`. `statusline.sh`, `rules/`, `commands/`, and `output-styles/` are stow symlinks. `settings.json` and `CLAUDE.md` are **real files, not symlinks**: Claude Code rewrites `settings.json` on `/model`, `/plugin`, and `/config` (which replaces a symlink with a plain file), so the committed copy is a snapshot — reconcile it toward the live `~/.claude/` version periodically.

- `settings.json` — permissions, sandbox, marketplaces, enabled plugins, model, effort level, output style
- `CLAUDE.md` — global instructions for all projects (Obsidian vaults)
- `rules/` — path-scoped rules auto-loaded per file type: `commit-messages.md`, `markdown-formatting.md`
- `output-styles/brief.md` — the custom **Brief** output style
- `commands/commit-message.md` — `/commit-message` slash command for the staged diff
- `statusline.sh` — two-line status line: directory, worktree, branch, model, context %, git diff stats, session cost
- `.local/bin/claude-resume` — fzf picker to resume any past session by its cwd, or relocate it into the current dir (ctrl-o)
- `.stow-local-ignore` — excludes runtime paths (skills, projects, sessions, plugins, plans, cache, history) from Stow management

### Git package (`git/`)

- `config` — global git config with SSH signing, fast-forward-only pulls, rerere, rebase auto-stash
- `recursive-sync.py` — invoked as `git rpull`; recursively finds all git repos up to depth 5 and pulls them concurrently (up to 50 workers)
- Custom aliases: `prune-local`, `stats-hotfiles`, `stats-activity`, `stats-hotfixes`, `stats-bugs`

### Neovim (`nvim/`)

Single-file config at `nvim/.config/nvim/init.lua` using Neovim's built-in package manager. Theme: One Dark. LSP via mason + nvim-lspconfig; completion via blink.cmp.

### Fish shell (`fish/`)

`config.fish` sets `EDITOR=nvim`, configures PATH for Cargo, Bun, Go, Claude local, initializes Starship, zoxide, fzf. Uses Fisher for plugin management (`fish_plugins` lists plugins).

## Key constraints

- `claude/.stow-local-ignore` deliberately excludes `skills/`, `plugins/`, `plans/`, `projects/` — these are user-local and must never be committed
- SSH private keys and `known_hosts` are excluded from the `ssh` package via `.stow-local-ignore`
- `lazy-lock.json` (Neovim) and tmux plugin directory are gitignored — don't commit them
