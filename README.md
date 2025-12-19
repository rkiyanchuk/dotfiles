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

Install Homebrew:

```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Clone dotfiles repository:

```bash
git clone http://github.com/rkiyanchuk/dotfiles && cd dotfiles
```

Install dependencies in Brewfile:

```sh
brew bundle install
```

Run `stow` with dotfiles bundle you want to use:

```bash
stow -t ~ {bundle}
```

where `{bundle}` is a bundle to install (e.g. `bash` or `vim`).

Install dotfiles:

```sh
# CLI-only
stow --dotfiles -t ~ -Svv bat fish git grc nvim shells ssh starship tmux yazi

# Desktop
stow -t ~ --no-folding -Svv ghostty claude gh
```

Configure Fish shell:

```sh
echo $(which fish) | sudo tee -a /etc/shells
chsh -s $(which fish)
```

Update Fish shell:

```sh
❯ update
```

Install `pynvim` for neovim:

```sh
uv tool install --upgrade pynvim
```

Configure tmux.

Install tmux plugin manager:

```sh
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

Fix tmux terminfo (still necessary?).

```sh
curl -LO https://invisible-island.net/datafiles/current/terminfo.src.gz && gunzip terminfo.src.gz
/usr/bin/tic -xe tmux-256color terminfo.src
```

Enable key repeat:

```sh
defaults write -g ApplePressAndHoldEnabled -bool false
```

### Caveats

* Podman, fix to run x86 images: https://github.com/containers/podman/discussions/12899

### Additional software

* [Monitor Control](https://monitorcontrol.app)
* [iState Menus](https://bjango.com/mac/istatmenus/)
* [Ice](https://github.com/jordanbaird/Ice)
* [CleanShot](https://cleanshot.com/)
* [Little Snitch](https://www.obdev.at/products/littlesnitch/index.html)

* TheIne
* LogiOptions+
* Kindle
* Final Cut Pro

* Affinity
* Adobe Acrobat
* NoteBurner
* XCode

## Setup in container

```sh
sudo pacman -Syu
sudo pacman -Syu neovim git stow fish starship eza
#sudo apt install git fish neovim stow starship
git clone https://github.com/rkiyanchuk/dotfiles
cd dotfiles
stow -Svv neovim starship git fish bat
sudo chsh <username>
# /usr/bin/fish
```

TODO:

```
sudo scutil --set HostName "your-new-name"
sudo scutil --set LocalHostName "your-new-name"
```
