# Dotfiles

Personal configuration files managed with [GNU Stow](https://www.gnu.org/software/stow/) and automated with [Just](https://just.systems/).

## Packages

| Package     | Description                               |
| ----------- | ----------------------------------------- |
| `bat`       | Syntax-highlighted cat                    |
| `claude`    | Claude Code settings, status line & rules |
| `direnv`    | Per-directory environment variables       |
| `fish`      | Shell configuration                       |
| `gh`        | GitHub CLI                                |
| `ghostty`   | Terminal emulator (macOS)                 |
| `git`       | Git config and utilities                  |
| `glances`   | System monitor                            |
| `grc`       | CLI output colorizer                      |
| `nvim`      | Neovim editor                             |
| `obsidian`  | Obsidian vault config                     |
| `omp`       | Oh My Pi coding agent config              |
| `ssh`       | SSH config (macOS)                        |
| `starship`  | Shell prompt                              |
| `tmux`      | Terminal multiplexer                      |
| `wireshark` | Network protocol analyzer (macOS)         |
| `yazi`      | File manager                              |
| `zed`       | Editor (macOS)                            |

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
sudo pacman -S just git stow
git clone https://github.com/rkiyanchuk/dotfiles
cd dotfiles
just
```

**Ubuntu / Debian**

```sh
sudo apt install just git stow
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

### Default apps (duti)

[duti](https://github.com/moretension/duti) sets default applications for file types and URL schemes on macOS using bundle IDs and Apple [Uniform Type Identifiers](https://developer.apple.com/library/content/documentation/FileManagement/Conceptual/understanding_utis/understand_utis_intro/understand_utis_intro.html) (UTI).

```sh
brew install duti
```

Look up an app's bundle ID:

```sh
osascript -e 'id of app "Zed"'   # → dev.zed.Zed
```

Set a default app by file extension or UTI:

```sh
# Syntax: duti -s <bundle-id> <extension-or-UTI> <role>
# Role is usually "all" (viewer + editor + shell)

duti -s dev.zed.Zed .txt  all   # plain text files
duti -s dev.zed.Zed .md   all   # Markdown
duti -s dev.zed.Zed .json all   # JSON

# UTIs cover all files of a type regardless of extension
duti -s dev.zed.Zed public.plain-text   all   # all plain-text files
duti -s dev.zed.Zed public.source-code  all   # all source-code files
```

Query the current default for an extension:

```sh
duti -x md
# Zed
# /Applications/Zed.app
# dev.zed.Zed
```
