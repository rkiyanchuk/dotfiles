# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal dotfiles repository that manages configuration files for macOS development environment using GNU Stow. The repository follows a modular structure where each directory represents a "package" containing configuration files for specific tools.

## Architecture

### Core Structure
- Each top-level directory represents a configuration package (e.g., `fish/`, `neovim/`, `git/`)
- Configuration files are organized to mirror their target locations when deployed via GNU Stow
- The repository uses symbolic links to install configurations into the home directory

### Key Components
- **fish/**: Fish shell configuration with custom functions, aliases, and environment setup
- **neovim/**: Comprehensive Neovim configuration with LSP, plugins, and custom keymaps
- **git/**: Git configuration with signing, aliases, and tool integrations
- **starship/**: Cross-shell prompt configuration
- **tmux/**: Terminal multiplexer configuration with plugins
- **bat/**: Syntax highlighting configuration for the bat command
- **grc/**: Generic command colorizer configuration
- **_fonts/**: Custom font building setup for Iosevka with Nerd Font patching

## Common Commands

### Initial Setup
```bash
# Install Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Clone repository
git clone http://github.com/rkiyanchuk/dotfiles && cd dotfiles

# Install dependencies
brew bundle install

# Install dotfiles packages
stow -t ~ -Svv fish git neovim gnupg tmux sage
```

### Package Management
```bash
# Install specific package
stow -t ~ {package_name}

# Remove package
stow -t ~ -D {package_name}

# Reinstall package (useful after config changes)
stow -t ~ -R {package_name}
```

### Shell Setup
```bash
# Configure Fish shell as default
echo $(which fish) | sudo tee -a /etc/shells
chsh -s $(which fish)

# Install Fisher plugin manager
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
```

### Neovim Setup
```bash
# Install Python support
pip3 install pynvim
```

### Tmux Setup
```bash
# Install TPM (Tmux Plugin Manager)
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Fix tmux terminfo on macOS
curl -LO https://invisible-island.net/datafiles/current/terminfo.src.gz && gunzip terminfo.src.gz
/usr/bin/tic -xe tmux-256color terminfo.src
```

### System Tweaks
```bash
# Enable key repeat for faster navigation
defaults write -g ApplePressAndHoldEnabled -bool false
```

### Font Building
```bash
# Build custom Iosevka fonts (in _fonts/ directory)
make all  # Clone repos, build Docker images, compile fonts, and patch with Nerd Fonts
```

## Configuration Details

### Fish Shell Features
- Custom prompt via Starship
- Enhanced ls/eza integration
- Git branch switching with fzf (`fzg` function)
- System upgrade function for Homebrew and Fish plugins
- Environment variables for development tools (Go, Cargo, npm, etc.)

### Neovim Features
- LSP support for multiple languages (Python, Go, Rust, TypeScript, etc.)
- Telescope fuzzy finder integration
- Git integration via Gitsigns
- Treesitter syntax highlighting
- Custom keymaps with leader key (space)
- Plugin management via Lazy.nvim

### Git Configuration
- SSH-based commit signing enabled
- Custom aliases for common operations
- VSCode and Neovim integration for diff/merge tools
- Automatic commit signing and credential caching

### Development Tools
The Brewfile installs essential development tools including:
- Core CLI tools: fish, starship, bat, eza, fd, fzf, ripgrep
- Development: neovim, git, golang, node, python tooling
- Security tools: age, nmap, semgrep, sslscan
- System utilities: Alfred, Rectangle Pro, iStat Menus

## Maintenance

### Update All Tools
Use the Fish `upgrade` function to update Homebrew packages and Fish plugins:
```bash
upgrade
```

### Sync Configuration Changes
After modifying configurations, use Stow to update symlinks:
```bash
stow -t ~ -R {modified_package}
```

## Host-Specific Configurations
- Fish: `config.{hostname}.fish` and `config.local.fish` files for host-specific overrides
- Neovim: `init.{hostname}.lua` and `init.local.lua` files for host-specific configurations
