# Dotfiles installation
# Works on macOS, Arch Linux, and apt-based Linux distributions.

set shell := ["bash", "-cu"]

# Colors for status messages
orange := '\033[0;33m'
green := '\033[0;32m'
reset := '\033[0m'

username := env("USER")
deps_arch := "bat eza fd fish fzf git grc neovim python-uv ripgrep starship stow tmux yazi"
deps_ubuntu := "bat eza fd-find fish fzf git grc neovim ripgrep snapd stow tmux"

# CLI packages to install via stow
packages_cli := "bat fish git gh grc glances nvim starship tmux yazi claude"

# Desktop packages to install via stow (macOS only)
packages_gui := "ghostty wireshark ssh zed"

# Detect the current OS
os := if os() == "macos" { "macos" } else if path_exists("/etc/arch-release") == "true" { "arch" } else { "ubuntu" }

export PATH := if os() == "macos" { env("PATH") } else { env("HOME") + "/.local/bin:" + env("PATH") }

# Default recipe: run full setup for current OS
default: install
    @echo -e "{{ green }}==> Dotfiles installed!{{ reset }}"

# Full setup from scratch
install: deps config shell plugins
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{ os }}" == "macos" ]]; then
        just enable-key-repeat
    fi

# Install system dependencies based on OS
deps:
    #!/usr/bin/env bash
    set -euo pipefail
    echo -e "{{ orange }}==> Installing dotfiles for {{ os }}...{{ reset }}"
    case "{{ os }}" in
        macos)
            echo -e "{{ orange }}==> Installing Homebrew packages from Brewfile...{{ reset }}"
            brew bundle install
            ;;
        arch)
            echo -e "{{ orange }}==> Installing packages via pacman...{{ reset }}"
            sudo pacman -Syu --noconfirm
            sudo pacman -S --needed --noconfirm {{ deps_arch }}
            ;;
        ubuntu)
            echo -e "{{ orange }}==> Installing packages via apt...{{ reset }}"
            sudo apt update && sudo apt upgrade -y {{ deps_ubuntu }}
            sudo apt install -y {{ deps_ubuntu }}
            # Install uv via Astral
            curl -LsSf https://astral.sh/uv/install.sh | sh
            # Starship needs to be installed separately on Ubuntu
            if ! command -v starship &> /dev/null; then
                curl -sS https://starship.rs/install.sh | sh -s -- -y -b ~/.local/bin
            fi
            ;;
    esac

# Stow all dotfiles packages
config: config-cli config-gui plugins

# Stow CLI dotfiles
config-cli:
    @echo -e "{{ orange }}==> Installing CLI dotfiles...{{ reset }}"
    stow --dotfiles --no-folding -t ~ -Svv {{ packages_cli }}

# Stow GUI dotfiles (macOS only)
config-gui:
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{ os }}" == "macos" ]]; then
        echo -e "{{ orange }}==> Installing desktop dotfiles...{{ reset }}"
        stow --dotfiles -t ~ --no-folding -Svv {{ packages_gui }}
    fi

# Restow dotfiles after config changes
reconfig:
    @echo "==> Restowing dotfiles..."
    stow --no-folding --dotfiles -t ~ -Rvv {{ packages_cli }}
    stow --dotfiles --no-folding -t ~ -Rvv {{ packages_gui }}

# Remove all stowed dotfiles
unconfig:
    @echo "==> Removing dotfiles..."
    stow --dotfiles -t ~ -Dvv {{ packages_cli }}
    stow --dotfiles -t ~ -Dvv {{ packages_gui }}

# Dry-run stow to check for conflicts
config-check:
    @echo "Testing stow installation (dry run)..."
    stow --no-folding --dotfiles -t ~ -nvv {{ packages_cli }}
    stow --dotfiles --no-folding -t ~ -nvv {{ packages_gui }}

# Configure Fish shell as default
shell:
    #!/usr/bin/env bash
    set -euo pipefail
    FISH_PATH=$(which fish)
    if ! grep -q "$FISH_PATH" /etc/shells; then
        echo -e "{{ orange }}==> Adding Fish to /etc/shells...{{ reset }}"
        echo "$FISH_PATH" | sudo tee -a /etc/shells
    fi
    if [[ "$SHELL" != "$FISH_PATH" ]]; then
        echo -e "{{ orange }}==> Setting Fish as default shell...{{ reset }}"
        sudo chsh -s "$FISH_PATH" {{ username }}
    fi

# Install all third-party plugins and runtime deps
plugins: plugins-tmux plugins-yazi plugins-claude plugins-nvim plugins-fish

# Install tmux plugin manager
plugins-tmux:
    #!/usr/bin/env bash
    set -euo pipefail
    TPM_DIR="$HOME/.config/tmux/plugins/tpm"
    if [[ ! -d "$TPM_DIR" ]]; then
        echo -e "{{ orange }}==> Installing tmux plugin manager...{{ reset }}"
        git clone https://github.com/tmux-plugins/tpm "$TPM_DIR"
    fi

# Install yazi packages (flavors/plugins) declared in package.toml
plugins-yazi:
    #!/usr/bin/env bash
    set -euo pipefail
    if command -v ya &> /dev/null; then
        echo -e "{{ orange }}==> Installing yazi packages...{{ reset }}"
        ya pkg install
    fi

# Install Claude Code plugins declared in settings.json
plugins-claude:
    #!/usr/bin/env bash
    set -euo pipefail
    echo -e "{{ orange }}==> Installing Claude Code plugins...{{ reset }}"
    claude plugin install context7@claude-plugins-official
    claude plugin install firecrawl@claude-plugins-official
    claude plugin install gopls-lsp@claude-plugins-official
    claude plugin install lua-lsp@claude-plugins-official
    claude plugin install pr-review-toolkit@claude-plugins-official
    claude plugin install pyright-lsp@claude-plugins-official
    claude plugin install rust-analyzer-lsp@claude-plugins-official
    claude plugin install skill-creator@claude-plugins-official
    claude plugin install swift-lsp@claude-plugins-official
    claude plugin install typescript-lsp@claude-plugins-official
    claude plugin install obsidian@obsidian-skills

# Install Fisher and Fish plugins declared in fish_plugins
plugins-fish:
    #!/usr/bin/env bash
    set -euo pipefail
    echo -e "{{ orange }}==> Installing Fisher and Fish plugins...{{ reset }}"
    fish -c 'curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher && fisher update'

# Install pynvim for Neovim Python support
plugins-nvim:
    @echo -e "{{ orange }}==> Installing pynvim for Neovim...{{ reset }}"
    uv tool install --upgrade pynvim

# Enable key repeat on macOS (disables press-and-hold)
[macos]
enable-key-repeat:
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{ os }}" == "macos" ]]; then
        echo -e "{{ orange }}==> Enable key repeat...{{ reset }}"
        defaults write -g ApplePressAndHoldEnabled -bool false
    fi

# Set hostname on macOS
[macos]
set-hostname name:
    sudo scutil --set HostName "{{ name }}"
    sudo scutil --set LocalHostName "{{ name }}"

# Sync Obsidian config between dotfiles and a vault
# Usage: just obsidian-config push|pull <vault-path>
obsidian-config direction vault:
    #!/usr/bin/env bash
    set -euo pipefail
    EXCLUDES="--exclude=workspace.json --exclude=workspace-mobile.json"
    DOTFILES_OBS="$HOME/dotfiles/obsidian"
    case "{{ direction }}" in
        push)
            echo -e "{{ orange }}==> Pushing Obsidian config to {{ vault }}...{{ reset }}"
            rsync -av $EXCLUDES "$DOTFILES_OBS/.obsidian/" "{{ vault }}/.obsidian/"
            rsync -av $EXCLUDES "$DOTFILES_OBS/.obsidian-mobile/" "{{ vault }}/.obsidian-mobile/"
            ;;
        pull)
            echo -e "{{ orange }}==> Pulling Obsidian config from {{ vault }}...{{ reset }}"
            rsync -av $EXCLUDES "{{ vault }}/.obsidian/" "$DOTFILES_OBS/.obsidian/"
            rsync -av $EXCLUDES "{{ vault }}/.obsidian-mobile/" "$DOTFILES_OBS/.obsidian-mobile/"
            ;;
        *)
            echo "Usage: just obsidian-sync [push|pull] <vault-path>"
            exit 1
            ;;
    esac
