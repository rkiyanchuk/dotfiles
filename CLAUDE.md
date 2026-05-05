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

**Packages**: `bat`, `claude`, `fish`, `gh`, `ghostty`, `git`, `glances`, `grc`, `nvim`, `ssh`, `starship`, `tmux`, `yazi`.

### Claude Code package (`claude/`)

- `settings.json` — permissions, sandbox, enabled plugins, model config
- `hooks/rename-plan.sh` — PostToolUse hook on Write; auto-renames plan files to `_<repo>_<feature>.md` and creates a symlink in `~/.claude/plans/`
- `hooks/statusline.sh` — renders the Claude Code status line with: directory, git branch, model name, context %, git diff stats, session cost
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
