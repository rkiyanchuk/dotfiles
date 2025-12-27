# Dotfiles installation justfile
# Works on macOS, Arch Linux, and Ubuntu

set shell := ["bash", "-cu"]

# Colors for status messages
orange := '\033[0;33m'
green := '\033[0;32m'
reset := '\033[0m'

deps_arch := "bat eza fd fish fzf git grc neovim python-uv ripgrep starship stow tmux yazi"
deps_ubuntu := "bat eza fd-find fish fzf git grc neovim ripgrep snapd starship stow tmux"


# Detect the current OS
os := if os() == "macos" { "macos" } else if path_exists("/etc/arch-release") == "true" { "arch" } else { "ubuntu" }

# Default recipe: run full setup for current OS
default: setup
    @echo -e "{{ green }}==> Dotfiles installed!{{ reset }}"


# CLI packages to install via stow
cli_packages := "bat fish git gh grc nvim starship tmux yazi"

# Desktop packages to install via stow (macOS only)
desktop_packages := "ghostty claude"

# Full setup from scratch
setup: install-deps install-cli configure-shell install-nvim-deps install-tmux
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{ os }}" == "macos" ]]; then
        stow --dotfiles -t ~ --no-folding -Svv ssh
        just install-desktop
        just enable-key-repeat
    fi

# Install system dependencies based on OS
install-deps:
    #!/usr/bin/env bash
    set -euo pipefail
    case "{{ os }}" in
        macos)
            if ! command -v brew &> /dev/null; then
                echo -e "{{ orange }}==> Installing Homebrew...{{ reset }}"
                /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
            fi
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
            # Starship needs to be installed separately on Ubuntu
            if ! command -v starship &> /dev/null; then
                sudo snap install --edge starship
            fi
            ;;
    esac

# Install CLI dotfiles via stow
install-cli:
    @echo -e "{{ orange }}==> Installing CLI dotfiles...{{ reset }}"
    stow --dotfiles -t ~ -Svv {{ cli_packages }}

# Install desktop dotfiles via stow (macOS only)
install-desktop:
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{ os }}" == "macos" ]]; then
        echo -e "{{ orange }}==> Installing desktop dotfiles...{{ reset }}"
        stow -t ~ --no-folding -Svv {{ desktop_packages }}
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
        sudo chsh -s "$FISH_PATH"
    fi

# Install pynvim for Neovim Python support
install-nvim-deps:
    @echo -e "{{ orange }}==> Installing pynvim for Neovim...{{ reset }}"
    uv tool install --upgrade pynvim

# Install tmux plugin manager
install-tmux:
    #!/usr/bin/env bash
    set -euo pipefail
    TPM_DIR="$HOME/.tmux/plugins/tpm"
    if [[ ! -d "$TPM_DIR" ]]; then
        echo -e "{{ orange }}==> Installing tmux plugin manager...{{ reset }}"
        git clone https://github.com/tmux-plugins/tpm "$TPM_DIR"
    fi

# # Fix tmux terminfo (macOS)
# fix-tmux-terminfo:
#     #!/usr/bin/env bash
#     set -euo pipefail
#     if [[ "{{ os }}" == "macos" ]]; then
#         echo "Fixing tmux terminfo..."
#         curl -LO https://invisible-island.net/datafiles/current/terminfo.src.gz
#         gunzip -f terminfo.src.gz
#         /usr/bin/tic -xe tmux-256color terminfo.src
#         rm terminfo.src
#     else
#         echo "Terminfo fix is only needed on macOS"
#     fi

# Enable key repeat on macOS (disables press-and-hold)
enable-key-repeat:
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{ os }}" == "macos" ]]; then
        echo -e "{{ orange }}==> Enable key repeat...{{ reset }}"
        defaults write -g ApplePressAndHoldEnabled -bool false
    fi

# Reinstall dotfiles (useful after config changes)
restow:
    @echo "==> Restowing dotfiles..."
    stow --dotfiles -t ~ -Rvv {{ cli_packages }}
    stow -t ~ --no-folding -Svv {{ desktop_packages }}

# Test stow installation (dry run)
test-stow:
    @echo "Testing stow installation (dry run)..."
    stow --dotfiles -t ~ -nvv {{ cli_packages }}

# Set hostname on macOS
[macos]
set-hostname name:
    sudo scutil --set HostName "{{ name }}"
    sudo scutil --set LocalHostName "{{ name }}"
