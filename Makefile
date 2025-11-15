# Makefile for dotfiles installation
# Supports macOS and Linux (Arch/Debian)

.PHONY: help install deps dotfiles uninstall setup-fish setup-fisher setup-tmux setup-tmux-terminfo setup-neovim setup-macos setup-all

# Detect OS
UNAME := $(shell uname)

# Core packages to install via stow
PACKAGES := fish git neovim starship tmux bat

# Default target
help:
	@echo "Dotfiles Installation Makefile"
	@echo ""
	@echo "Main targets:"
	@echo "  make install              - Complete installation (deps + dotfiles + Fish setup)"
	@echo "  make deps                 - Install dependencies (Homebrew/pacman/apt)"
	@echo "  make dotfiles             - Install dotfiles via stow"
	@echo "  make uninstall            - Uninstall dotfiles"
	@echo ""
	@echo "Fish shell setup:"
	@echo "  make setup-fish           - Configure Fish as default shell (requires sudo)"
	@echo ""
	@echo "Optional setup targets:"
	@echo "  make setup-fisher         - Install Fisher plugin manager for Fish"
	@echo "  make setup-tmux           - Install TPM (Tmux Plugin Manager)"
	@echo "  make setup-tmux-terminfo  - Fix tmux terminfo (macOS only)"
	@echo "  make setup-neovim         - Install pynvim for Neovim"
	@echo "  make setup-macos          - Configure macOS settings (key repeat)"
	@echo "  make setup-all            - Run all optional setup targets"
	@echo ""
	@echo "Detected OS: $(UNAME)"

# Complete installation
install: deps dotfiles setup-fish
	@echo "✓ Installation complete!"
	@echo ""
	@echo "Optional setup targets available:"
	@echo "  make setup-fisher         - Install Fisher plugin manager"
	@echo "  make setup-tmux           - Install TPM"
	@echo "  make setup-neovim         - Install pynvim"
ifeq ($(UNAME),Darwin)
	@echo "  make setup-tmux-terminfo  - Fix tmux terminfo (macOS)"
	@echo "  make setup-macos          - Configure macOS settings"
endif
	@echo ""
	@echo "Or run: make setup-all"

# Install dependencies
deps:
ifeq ($(UNAME),Darwin)
	@echo "→ Installing dependencies on macOS..."
	@if ! command -v brew >/dev/null 2>&1; then \
		echo "  Installing Homebrew..."; \
		/bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"; \
	else \
		echo "  ✓ Homebrew already installed"; \
	fi
	@echo "  Installing Brewfile dependencies..."
	@brew bundle install --file=Brewfile
	@echo "✓ macOS dependencies installed"
else ifeq ($(UNAME),Linux)
	@echo "→ Installing dependencies on Linux..."
	@if command -v pacman >/dev/null 2>&1; then \
		echo "  Detected Arch Linux (pacman)"; \
		sudo pacman -Syu --needed --noconfirm neovim git stow fish starship eza bat fzf ripgrep tmux; \
	elif command -v apt >/dev/null 2>&1; then \
		echo "  Detected Debian/Ubuntu (apt)"; \
		sudo apt update && sudo apt install -y git fish neovim stow fzf ripgrep tmux bat; \
		echo "  Note: starship and eza may need manual installation on Debian/Ubuntu"; \
	else \
		echo "  Error: Unsupported package manager. Please install manually:"; \
		echo "    neovim git stow fish starship eza bat fzf ripgrep tmux"; \
		exit 1; \
	fi
	@echo "✓ Linux dependencies installed"
else
	@echo "Error: Unsupported OS: $(UNAME)"
	@exit 1
endif

# Install dotfiles using stow
dotfiles:
	@echo "→ Installing dotfiles..."
	@for package in $(PACKAGES); do \
		if [ -d "$$package" ]; then \
			echo "  Installing $$package..."; \
			stow --dotfiles -t ~ -R -v $$package; \
		else \
			echo "  Warning: Package $$package not found, skipping"; \
		fi \
	done
	@echo "✓ Dotfiles installed"

# Uninstall dotfiles
uninstall:
	@echo "→ Uninstalling dotfiles..."
	@for package in $(PACKAGES); do \
		if [ -d "$$package" ]; then \
			echo "  Uninstalling $$package..."; \
			stow --dotfiles -t ~ -D -v $$package; \
		fi \
	done
	@echo "✓ Dotfiles uninstalled"

# Setup Fish as default shell
setup-fish:
	@echo "→ Configuring Fish shell..."
	@if ! command -v fish >/dev/null 2>&1; then \
		echo "  Error: Fish is not installed. Run 'make deps' first."; \
		exit 1; \
	fi
	@FISH_PATH=$$(which fish); \
	if ! grep -q "$$FISH_PATH" /etc/shells; then \
		echo "  Adding Fish to /etc/shells (requires sudo)..."; \
		echo "$$FISH_PATH" | sudo tee -a /etc/shells >/dev/null; \
	else \
		echo "  ✓ Fish already in /etc/shells"; \
	fi
	@FISH_PATH=$$(which fish); \
	CURRENT_SHELL=$$(basename "$$SHELL"); \
	if [ "$$CURRENT_SHELL" != "fish" ]; then \
		echo "  Setting Fish as default shell (requires sudo)..."; \
		chsh -s "$$FISH_PATH"; \
		echo "  ✓ Fish set as default shell (restart terminal to apply)"; \
	else \
		echo "  ✓ Fish is already the default shell"; \
	fi

# Install Fisher plugin manager for Fish
setup-fisher:
	@echo "→ Installing Fisher plugin manager..."
	@if ! command -v fish >/dev/null 2>&1; then \
		echo "  Error: Fish is not installed. Run 'make deps' first."; \
		exit 1; \
	fi
	@if fish -c "functions -q fisher" 2>/dev/null; then \
		echo "  ✓ Fisher already installed"; \
	else \
		echo "  Downloading and installing Fisher..."; \
		curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | fish -c "source && fisher install jorgebucaran/fisher"; \
		echo "  ✓ Fisher installed"; \
	fi

# Install TPM (Tmux Plugin Manager)
setup-tmux:
	@echo "→ Installing TPM (Tmux Plugin Manager)..."
	@if [ -d ~/.tmux/plugins/tpm ]; then \
		echo "  ✓ TPM already installed"; \
	else \
		echo "  Cloning TPM repository..."; \
		git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm; \
		echo "  ✓ TPM installed"; \
		echo "  Note: Press prefix + I in tmux to install plugins"; \
	fi

# Fix tmux terminfo on macOS
setup-tmux-terminfo:
ifeq ($(UNAME),Darwin)
	@echo "→ Fixing tmux terminfo on macOS..."
	@if infocmp tmux-256color >/dev/null 2>&1; then \
		echo "  ✓ tmux-256color terminfo already configured"; \
	else \
		echo "  Downloading and compiling terminfo..."; \
		curl -LO https://invisible-island.net/datafiles/current/terminfo.src.gz && \
		gunzip terminfo.src.gz && \
		/usr/bin/tic -xe tmux-256color terminfo.src && \
		rm terminfo.src; \
		echo "  ✓ tmux terminfo fixed"; \
	fi
else
	@echo "  Skipping: This target is macOS-only"
endif

# Install pynvim for Neovim
setup-neovim:
	@echo "→ Installing pynvim for Neovim..."
	@if command -v pip3 >/dev/null 2>&1; then \
		pip3 install --upgrade pynvim; \
		echo "  ✓ pynvim installed/updated"; \
	else \
		echo "  Error: pip3 not found. Please install Python 3 and pip."; \
		exit 1; \
	fi

# Configure macOS settings
setup-macos:
ifeq ($(UNAME),Darwin)
	@echo "→ Configuring macOS settings..."
	@echo "  Enabling key repeat..."
	@defaults write -g ApplePressAndHoldEnabled -bool false
	@echo "  ✓ Key repeat enabled (restart apps to apply)"
else
	@echo "  Skipping: This target is macOS-only"
endif

# Run all optional setup targets
setup-all: setup-fisher setup-tmux setup-neovim
	@echo "→ Running all optional setup targets..."
ifeq ($(UNAME),Darwin)
	@$(MAKE) setup-tmux-terminfo
	@$(MAKE) setup-macos
endif
	@echo "✓ All optional setup complete!"
