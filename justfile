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
packages_cli := "bat fish git gh grc nvim starship tmux yazi"

# Desktop packages to install via stow (macOS only)
packages_desktop := "ghostty claude"

# Detect the current OS
os := if os() == "macos" { "macos" } else if path_exists("/etc/arch-release") == "true" { "arch" } else { "ubuntu" }

export PATH := if os() == "macos" { env("PATH") } else { env("HOME") + "/.local/bin:" + env("PATH") }

# Default recipe: run full setup for current OS
default: install
    @echo -e "{{ green }}==> Dotfiles installed!{{ reset }}"

# Full setup from scratch
install: install-deps install-dotfiles-cli configure-shell install-tmux install-nvim-deps
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{ os }}" == "macos" ]]; then
        stow --dotfiles -t ~ --no-folding -Svv ssh
        just install-dotfiles-desktop
        just enable-key-repeat
    fi

# Install system dependencies based on OS
install-deps:
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
            source ~/.local/bin/env
            # Starship needs to be installed separately on Ubuntu
            if ! command -v starship &> /dev/null; then
                curl -sS https://starship.rs/install.sh | sh -s -- -y -b ~/.local/bin
            fi
            ;;
    esac

# Install CLI dotfiles via stow
install-dotfiles-cli:
    @echo -e "{{ orange }}==> Installing CLI dotfiles...{{ reset }}"
    stow --dotfiles -t ~ -Svv {{ packages_cli }}

# Install desktop dotfiles via stow (macOS only)
install-dotfiles-desktop:
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{ os }}" == "macos" ]]; then
        echo -e "{{ orange }}==> Installing desktop dotfiles...{{ reset }}"
        stow -t ~ --no-folding -Svv {{ packages_desktop }}
    fi

# Configure Fish shell as default
configure-shell:
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

# Install tmux plugin manager
install-tmux:
    #!/usr/bin/env bash
    set -euo pipefail
    TPM_DIR="$HOME/.tmux/plugins/tpm"
    if [[ ! -d "$TPM_DIR" ]]; then
        echo -e "{{ orange }}==> Installing tmux plugin manager...{{ reset }}"
        git clone https://github.com/tmux-plugins/tpm "$TPM_DIR"
    fi

# Install pynvim for Neovim Python support
install-nvim-deps:
    @echo -e "{{ orange }}==> Installing pynvim for Neovim...{{ reset }}"
    uv tool install --upgrade pynvim


# Reinstall dotfiles (useful after config changes)
restow:
    @echo "==> Restowing dotfiles..."
    stow --dotfiles -t ~ -Rvv {{ packages_cli }}
    stow -t ~ --no-folding -Svv {{ packages_desktop }}

# Test stow installation (dry run)
test-stow:
    @echo "Testing stow installation (dry run)..."
    stow --dotfiles -t ~ -nvv {{ packages_cli }}

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
