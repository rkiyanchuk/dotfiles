Dotfiles
========

User resource configuration.

If you have SSD, enable `relatime` mount options when partitioning the drive.

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
       $ ssh-keygen -t rsa -C "rkiyanchuk@mirantis.com"
       $ ssh-add

Optional steps:

aptitude install -y firmware-linux
aptitude install -y firmware-realtek  # system76: for ethernet
aptitude install -y firmware-iwlwifi  # system76: for Intel wireless cards


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

