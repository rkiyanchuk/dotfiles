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
stow -t ~ -Svv fish git neovim gnupg tmux sage
```

Configure Fish shell:

```sh
echo /usr/local/bin/fish | sudo tee -a /etc/shells
chsh -s $(which fish)
```

Install `fisher` plugin manager for Fish:
    
```sh
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
```

Install `pynvim` for neovim:

```sh
pip3 install pynvim
```

Configure tmux.

Install tmux plugin manager:

```sh
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

Fix tmux terminfo.

```sh
curl -LO https://invisible-island.net/datafiles/current/terminfo.src.gz && gunzip terminfo.src.gz
/usr/bin/tic -xe tmux-256color terminfo.src
```