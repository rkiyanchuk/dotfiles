Zoresvit's Dotfiles
===================

[Dotfiles](https://wiki.archlinux.org/index.php/Dotfiles) are custom
configuration files used to maintain user preferred settings across the
operating system. Thanks to version control systems
(like [Git](http://git-scm.com/)) and repository hosting services
(like [GitHub](https://dotfiles.github.io)) *dotfiles* are now easy to maintain
as well as share with others.

Looking at someone else's *dotfiles* helps to discover new configurations and
tweaks of commonly used software to increase productivity and comfort.

Usage
=====


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
    $ stow -t ~ {bundle}
    ```
    where `{bundle}` is a bundle to install (e.g. `bash` or `vim`).

Demo
====

![Desktop screenshot](https://raw.githubusercontent.com/zoresvit/dotfiles/master/demo.png)
