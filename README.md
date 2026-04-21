# Dotfiles

Personal configuration files managed with [GNU Stow](https://www.gnu.org/software/stow/) and automated with [Just](https://just.systems/).

## Packages

| Package | Description |
|---------|-------------|
| `bat` | Syntax-highlighted cat |
| `claude` | Claude Code settings and hooks |
| `fish` | Shell configuration |
| `gh` | GitHub CLI |
| `ghostty` | Terminal emulator (macOS) |
| `git` | Git config and utilities |
| `glances` | System monitor |
| `grc` | CLI output colorizer |
| `nvim` | Neovim editor |
| `ssh` | SSH config (macOS) |
| `starship` | Shell prompt |
| `tmux` | Terminal multiplexer |
| `yazi` | File manager |

## Setup

Clone into `$HOME` and run `just`.

**macOS**

```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew install just git
git clone https://github.com/rkiyanchuk/dotfiles
cd dotfiles
just
```

**Arch Linux**

```sh
sudo pacman -S just git
git clone https://github.com/rkiyanchuk/dotfiles
cd dotfiles
just
```

**Ubuntu / Debian**

```sh
sudo apt install just git
git clone https://github.com/rkiyanchuk/dotfiles
cd dotfiles
just
```

## Additional Setup

Install custom Iosevka font: [rkiyanchuk/font-iosevka-custom](https://github.com/rkiyanchuk/font-iosevka-custom).

Set hostname on macOS:

```sh
just set-hostname <name>
```
