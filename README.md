# Dotfiles

[Dotfiles](https://wiki.archlinux.org/index.php/Dotfiles) are custom
configuration files used to maintain user preferred settings across the
operating system. Thanks to version control systems
(like [Git](http://git-scm.com/)) and repository hosting services
(like [GitHub](https://dotfiles.github.io)) *dotfiles* are now easy to maintain
as well as share with others.

Looking at someone else's *dotfiles* helps to discover new configurations and
tweaks of commonly used software to increase productivity and comfort.

The *dotfiles* repository contains directories with configuration files. Each
directory corresponds to a *"package"* for convenience. For instance, all
Neovim related configuration files are stored in `neovim` *"package"* (directory).

GNU [Stow](https://www.gnu.org/software/stow/) is used to install the packages.

> GNU Stow is a symlink farm manager which takes distinct packages of software
> and/or data located in separate directories on the file system, and makes
> them appear to be installed in the same place.

## Setup

Go to `$HOME` dir.

On APT based systems (Ubuntu, Debian, Kali):

```sh
sudo apt install just git
git clone https://github.com/rkiyanchuk/dotfiles
cd dotfiles
just
```

On ArchLinux:

```sh
sudo pacman -S just git
git clone https://github.com/rkiyanchuk/dotfiles
cd dotfiles
just
```

On macOS:

```sh
# Install homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew install just
git clone https://github.com/rkiyanchuk/dotfiles
cd dotfiles
just
```

### Additional Setup

Install custom Iosevka font build from repo: [rkiyanchuk/font-iosevka-custom](https://github.com/rkiyanchuk/font-iosevka-custom).

Set custom hostname on macOS with:

```sh
just set-hostname ${hostname}
```
