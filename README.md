Zoresvit's Dotfiles
===================

[Dotfiles](https://wiki.archlinux.org/index.php/Dotfiles) are custom
configuration files used to maintain user preferred settings across the
operating system. Thanks to version control systems
(like [Git](http://git-scm.com/)) and repository hosting services
(like [GitHub](https://dotfiles.github.io)) _dotfiles_ are now easy to maintain
as well as share with others.

Looking at someone else's _dotfiles_ helps to discover new configurations and
tweaks of commonly used software to increase productivity and comfort.

![Desktop screenshot](https://raw.githubusercontent.com/zoresvit/dotfiles/master/demo.png)

Concepts
--------

All configuration files are stored in _dotfiles_ Git repository and shared via
[GitHub](https://github.com). However in order to take effect these files need
to be placed in correct directories, so some kind of _dotfiles_ manager is
needed.

Despite numerous dedicated tools for managing dotfiles
([dotsync](https://github.com/dotphiles/dotsync),
[dotfiles](https://github.com/jbernard/dotfiles),
[dfm](https://github.com/justone/dfm), etc.)
they all tend to be cumbersome and limited, depriving the flexibility of POSIX
operating systems. Therefore to adhere minimalism principals standard system
tools are used: GNU [`stow`](http://www.gnu.org/software/stow/) and
[`rsync`](https://rsync.samba.org/).

All user-specific configuration files are stored in user's `home` directory.
In order to maintain these files under single repository they are moved to
_dotfiles_ Git repository and _symlinks_ are created in place using `stow`.

Often users are at the same time administrators of their own computers, so it
would be useful to also maintain some system configuration files. In this case
symlinking with `stow` is not a proper solution as system partition must remain
integral. To safely populate system configuration files from the _dotfiles_
repository to the target system `rsync` can be used.


Usage
=====

The _dotfiles_ repository contains directories with configuration files. Each
directory corresponds to a _"package"_ for convenience. For instance, all
Bash-related configuration files are stored in `bash` _"package"_ (directory).

There is a special _package_

User configuration
------------------

> GNU Stow is a symlink farm manager which takes distinct packages of software
> and/or data located in separate directories on the file system, and makes
> them appear to be installed in the same place.

System configuration
--------------------

How to
======

1. Install git

```
sudo aptitude install git
```

2. Clone dotfiels repository.

```
git clone http://github.com/zoresvit/dotfiles
```

Update kernel manually

1. Qtconfig set fonts.
3. Create SSH keys:

    ```
   $ ssh-keygen -t rsa -C "rkiyanchuk@mirantis.com"
   $ ssh-add
    ```

Optional steps:

```
aptitude install -y firmware-linux
aptitude install -y firmware-realtek  # system76: for ethernet
aptitude install -y firmware-iwlwifi  # system76: for Intel wireless cards
```


Controls concept
================

`Mod` key is the standard modifier key (like Windows key on PCs or Command on
Macs). Every key binding that contains the key is for controling the window
manager of system settings.

Modifiers like `Control` or `Alt` without the `Mod` key must affect only the
currently focused app and perform adjustments to that app only.

`Control` and `Alt` modifiers *with* the `Mod` key are used as alternatives for
controlling the system to avoid collisions.


Troubleshooting
===============

Problem
-------

When applications trigger `gksu` for password prompt, it always fails
complaining that the password is wrong.

Start `gksu-properties` and switch Authentication mode from `su` to `sudo` and
grab mode from `enable` to `force enable`.

USB headset assigns to first sound card breaking the config.

Edit `/etc/modprobe.d/alsa-base.conf` file and set appropriate number for needed
card. For instance:

```bash
    options snd-usb-audio index=2
```

System provisioning hints
-------------------------

If you have SSD, enable `relatime` mount options when partitioning the drive.
