dotfiles
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
aptitude install -y firmware-iwlwifi  # system76: for Intel wireless cards
aptitude install -y firmware-realtek  # system76: for ethernet
