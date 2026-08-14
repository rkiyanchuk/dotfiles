# Dotfiles installation
# Works on macOS, Arch Linux, and apt-based Linux distributions.

set shell := ["bash", "-cu"]

# Colors for status messages; `\u{1b}` is a real escape byte,
# so plain `echo` renders them without `-e`.
orange := "\u{1b}[0;33m"
green := "\u{1b}[0;32m"
cyan := "\u{1b}[36m"
bold := "\u{1b}[1m"
reset := "\u{1b}[0m"

username := env("USER")
deps_arch := "bat eza fd fish fzf git grc neovim python-uv ripgrep starship stow tmux yazi"
deps_ubuntu := "bat eza fd-find fish fzf git grc neovim ripgrep snapd stow tmux"

# CLI packages to install via stow
packages_cli := "bat fish git gh grc glances nvim starship tmux yazi claude omp"

# Desktop packages to install via stow (macOS only)
packages_gui := "ghostty wireshark ssh zed"

# Detect the current OS
os := if os() == "macos" { "macos" } else if path_exists("/etc/arch-release") == "true" { "arch" } else { "ubuntu" }

export PATH := if os() == "macos" { env("PATH") } else { env("HOME") + "/.local/bin:" + env("PATH") }

# Setup dotfiles
default: config
    @echo "{{ green }}==> Dotfiles installed!{{ reset }}"

# Full setup from scratch
install: install-deps config set-shell
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{ os }}" == "macos" ]]; then
        just enable-key-repeat
    fi

# Install system dependencies based on OS
install-deps:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "{{ orange }}==> Installing dotfiles for {{ os }}...{{ reset }}"
    case "{{ os }}" in
        macos)
            echo "{{ orange }}==> Installing Homebrew packages from Brewfile...{{ reset }}"
            brew bundle install
            ;;
        arch)
            echo "{{ orange }}==> Installing packages via pacman...{{ reset }}"
            sudo pacman -Syu --noconfirm
            sudo pacman -S --needed --noconfirm {{ deps_arch }}
            ;;
        ubuntu)
            echo "{{ orange }}==> Installing packages via apt...{{ reset }}"
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
    @echo "{{ orange }}==> Installing CLI dotfiles...{{ reset }}"
    stow -Svv {{ packages_cli }}

# Stow GUI dotfiles (macOS only)
config-gui:
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{ os }}" == "macos" ]]; then
        echo "{{ orange }}==> Installing desktop dotfiles...{{ reset }}"
        stow -Svv {{ packages_gui }}
    fi

# Restow dotfiles after config changes
reconfig:
    @echo "==> Restowing dotfiles..."
    stow -Rvv {{ packages_cli }}
    stow -Rvv {{ packages_gui }}

# Remove all stowed dotfiles
unconfig:
    @echo "==> Removing dotfiles..."
    stow -Dvv {{ packages_cli }}
    stow -Dvv {{ packages_gui }}

# Dry-run stow to check for conflicts
config-check:
    @echo "Testing stow installation (dry run)..."
    stow -nvv {{ packages_cli }}
    stow -nvv {{ packages_gui }}

# Configure Fish shell as default
set-shell:
    #!/usr/bin/env bash
    set -euo pipefail
    FISH_PATH=$(which fish)
    if ! grep -q "$FISH_PATH" /etc/shells; then
        echo "{{ orange }}==> Adding Fish to /etc/shells...{{ reset }}"
        echo "$FISH_PATH" | sudo tee -a /etc/shells
    fi
    if [[ "$SHELL" != "$FISH_PATH" ]]; then
        echo "{{ orange }}==> Setting Fish as default shell...{{ reset }}"
        sudo chsh -s "$FISH_PATH" {{ username }}
    fi

# Install all third-party plugins and runtime deps
plugins: plugins-tmux plugins-yazi plugins-claude plugins-omp plugins-nvim plugins-fish

# Install tmux plugin manager
plugins-tmux:
    #!/usr/bin/env bash
    set -euo pipefail
    TPM_DIR="$HOME/.config/tmux/plugins/tpm"
    if [[ ! -d "$TPM_DIR" ]]; then
        echo "{{ orange }}==> Installing tmux plugin manager...{{ reset }}"
        git clone https://github.com/tmux-plugins/tpm "$TPM_DIR"
    fi

# Install yazi packages (flavors/plugins) declared in package.toml
plugins-yazi:
    #!/usr/bin/env bash
    set -euo pipefail
    if command -v ya &> /dev/null; then
        echo "{{ orange }}==> Installing yazi packages...{{ reset }}"
        ya pkg install
    fi

# Install Claude Code plugins declared in settings.json
plugins-claude:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "{{ orange }}==> Installing Claude Code plugins...{{ reset }}"
    claude plugin install chrome-devtools-mcp@claude-plugins-official
    claude plugin install context7@claude-plugins-official
    claude plugin install gopls-lsp@claude-plugins-official
    claude plugin install pr-review-toolkit@claude-plugins-official
    claude plugin install pyright-lsp@claude-plugins-official
    claude plugin install rust-analyzer-lsp@claude-plugins-official
    claude plugin install skill-creator@claude-plugins-official
    claude plugin install typescript-lsp@claude-plugins-official
    claude plugin install andrej-karpathy-skills@karpathy-skills
    claude plugin install claude-mem@thedotmack
    claude plugin install obsidian@obsidian-skills
    # `plugin install` enables every plugin; restore the desired enabled
    # flags from version control (plugins stay on disk, just disabled).
    git -C {{ justfile_directory() }} checkout -- claude/.claude/settings.json

# Install omp plugins natively (selective; skips claude-mem/context7/LSP)
plugins-omp:
    #!/usr/bin/env bash
    set -euo pipefail

    # Marketplaces to register, as `owner/repo` GitHub shorthand.
    marketplaces=(
        anthropics/claude-plugins-official  # official Anthropic catalog
        kepano/obsidian-skills              # Obsidian authoring skills
        rkiyanchuk/agent-plugins            # personal plugins
    )

    # Plugins to install at user scope, as `name@marketplace`. LSP plugins are
    # omitted because omp configures language servers itself; claude-mem and
    # context7 stay Claude Code-only.
    plugins=(
        apple-events-mcp@agent-plugins         # macOS Calendar/Reminders
        obsidian@obsidian-skills               # Obsidian vault tooling
        skill-creator@claude-plugins-official  # skill authoring
    )

    # `marketplace add` and `plugin install` exit 1 when the target is already
    # present, so tolerate exactly that error to keep re-runs idempotent.
    omp_plugin() {
        local expected="$1" out
        shift
        out=$(omp plugin "$@" 2>&1) && return 0
        grep -qF "$expected" <<< "$out" && return 0
        echo "$out" >&2
        return 1
    }

    # Report an entry's row from omp's own listing
    omp_row() {
        local entry="$1" listing="$2" row
        row=$(grep -F "$entry" <<< "$listing") \
            || { echo "$entry missing from omp state" >&2; return 1; }
        echo "$row"
    }

    # Rows are filtered through a pipe, where omp drops its cyan names and dim
    # versions/scopes; ask for them back unless the caller wants plain output.
    cyan='' green='' off=''
    if [[ -z ${NO_COLOR:-} ]]; then
        export FORCE_COLOR=1
        cyan=$(printf '{{ cyan }}')
        green=$(printf '{{ green }}')
        off=$(printf '{{ reset }}')
    fi

    echo -e "{{ bold }}Configured Marketplaces:{{ reset }}\n"
    for market in "${marketplaces[@]}"; do
        omp_plugin "already exists" marketplace add "$market"
        omp_row "$market" "$(omp plugin marketplace list 2>/dev/null)"
    done
    sed 's/^/  /' <<< "$(omp plugin marketplace update 2>&1)"

    echo -e "\n{{ bold }}Marketplace Plugins:{{ reset }}\n"
    upgraded=$(omp plugin upgrade 2>&1)
    for plugin in "${plugins[@]}"; do
        omp_plugin "already installed" install "$plugin"
        # omp leaves plugin ids uncolored; paint them cyan like marketplaces.
        row=$(omp_row "$plugin" "$(omp plugin list 2>/dev/null)")
        echo "${row/#"  $plugin"/  $cyan$plugin$off}"
    done
    if [[ $upgraded == *"up to date"* ]]; then
        echo "  ${green}✔ ${upgraded}${off}"
    else
        sed 's/^/  /' <<< "$upgraded"
    fi

# Install Fisher and Fish plugins declared in fish_plugins
plugins-fish:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "{{ orange }}==> Installing Fisher and Fish plugins...{{ reset }}"
    fish -c 'curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher && fisher update'

# Install pynvim for Neovim Python support
plugins-nvim:
    @echo "{{ orange }}==> Installing pynvim for Neovim...{{ reset }}"
    uv tool install --upgrade pynvim

# MacOS: enable key repeat (disables press-and-hold)
[macos]
enable-key-repeat:
    @echo "{{ orange }}==> Enable key repeat...{{ reset }}"
    @defaults write -g ApplePressAndHoldEnabled -bool false

# MacOS: set hostname
[macos]
set-hostname name:
    sudo scutil --set HostName "{{ name }}"
    sudo scutil --set LocalHostName "{{ name }}"

# Sync Obsidian config between dotfiles and a vault
# Usage: just obsidian-config push|pull <vault-path>
obsidian-config direction vault:
    #!/usr/bin/env bash
    set -euo pipefail
    excludes=(--exclude=workspace.json --exclude=workspace-mobile.json)
    dotfiles_obs="{{ justfile_directory() }}/obsidian"
    case "{{ direction }}" in
        push)
            echo "{{ orange }}==> Pushing Obsidian config to {{ vault }}...{{ reset }}"
            src="$dotfiles_obs" dst="{{ vault }}"
            ;;
        pull)
            echo "{{ orange }}==> Pulling Obsidian config from {{ vault }}...{{ reset }}"
            src="{{ vault }}" dst="$dotfiles_obs"
            ;;
        *)
            echo "Usage: just obsidian-config [push|pull] <vault-path>"
            exit 1
            ;;
    esac
    for dir in .obsidian .obsidian-mobile; do
        rsync -av "${excludes[@]}" "$src/$dir/" "$dst/$dir/"
    done
