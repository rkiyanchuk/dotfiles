# Zoresvit's Dotfiles

[Dotfiles](https://wiki.archlinux.org/index.php/Dotfiles) are custom
configuration files used to maintain user preferred settings across the
operating system. Thanks to version control systems
(like [Git](http://git-scm.com/)) and repository hosting services
(like [GitHub](https://dotfiles.github.io)) *dotfiles* are now easy to maintain
as well as share with others.

Looking at someone else's *dotfiles* helps to discover new configurations and
tweaks of commonly used software to increase productivity and comfort.

## Usage

The *dotfiles* repository contains directories with configuration files. Each
directory corresponds to a *"package"* for convenience. For instance, all
Neovim related configuration files are stored in `neovim` *"package"* (directory).

GNU [Stow](https://www.gnu.org/software/stow/) is used to install the packages.

> GNU Stow is a symlink farm manager which takes distinct packages of software
> and/or data located in separate directories on the file system, and makes
> them appear to be installed in the same place.

1. Clone dotfiles repository.

    ```bash
    git clone http://github.com/zoresvit/dotfiles && cd dotfiles
    ```

2. Run `stow` with dotfiles bundle you want to use:

    ```bash
    stow -t ~ {bundle}
    ```

    where `{bundle}` is a bundle to install (e.g. `bash` or `vim`).

## Mac setup

1. Clone Dotfiles

    ```sh
    git clone https://github.com/rkiyanchuk/dotfiles
    ```

2. Install homebrew:

    ```sh
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    ```

3. In dotfiles run, to install packages:

    ```sh
    brew bundle install
    ```

4. Install dotfiles:

    ```sh
    stow -Svv zsh git gnupg goldendict neovim sage tmux
    ```

5. Configure ZSH:
    - Install `oh-my-zsh`:

        ```sh
        sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
        ```

    - Install ZSH theme:

        ```sh
        git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
        ```

    - Install `lazyload` plugin:

        ```sh
        git clone https://github.com/qoomon/zsh-lazyload $ZSH_CUSTOM/plugins/zsh-lazyload
        ```

    - Install `pynvim` for neovim:

        ```sh
        pip3 install pynvim
        ```

6. Configure tmux.
    - Install tmux plugin manager:

        ```sh
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
        ```

    - Fix tmux terminfo.

        ```sh
        curl -LO https://invisible-island.net/datafiles/current/terminfo.src.gz && gunzip terminfo.src.gz
        /usr/bin/tic -xe tmux-256color terminfo.src
        ```
