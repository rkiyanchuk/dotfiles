#!/usr/bin/env bash

# Basic desktop components
sudo apt-get -y install xerver-org xinit slim xmonad alsa xxkb \
                        libghc-xmonad-dev libghc-xmonad-contrib-dev xmobar \
                        xcompmgr nitrogen stalonetray moreutils synapse 

# Pulseaudio controls
sudo apt-get -y install paman pavucontrol 

# Improve GUI 
sudo apt-get -y install fonts-liberation ttf-mscorefonts-installer \
                        qt4-qtconfig shiki-brave-colors dmz-cursor-theme

# Helpers for better user-experience
sudo apt-get -y install  usbmount suckless-tools gnome-screensaver

# Enforce usbmount settings
sed -i 's/^.*FS_MOUNTOPTIONS.*$/FS_MOUNTOPTIONS="-fstype=ntfs,nls=utf8,umask=007,gid=46 -fstype=vfat,gid=floppy,dmask=0007,fmask=0117"/' /etc/usbmount/usbmount.conf
sed -i 's/^.*FILESYSTEMS.*$/FILESYSTEMS="vfat ntfs ext2 ext3 ext4 hfsplus"/' /etc/usbmount/usbmount.conf

# Configuration specific dependencies
sudo apt-get -y install git mercurial libxft-dev libxft2 

# Install dotfiles for resource configuration
git clone https://github.com/zoresvit/dotfiles.git ~/.dotfiles
~/.dotfiles/bin/dfm install


# OPTIONAL PREFERED SOFTWARE

# Python
sudo apt-get -y install python3-all python2.7-dev python3-dev python-pip 

# Python global dependencies
sudo pip install virtualenvwrapper

# CLI Tools
sudo apt-get -y install tree htop tmux openssh-client openssh-server mc vifm \
                        rxvt-unicode scrot xclip exuberant-ctags source-highlight 

# Build tools
sudo apt-get -y install make checkinstall 


# Network tools
sudo apt-get -y install x11vnc traceroute
sudo apt-get -y install network-manager network-manager-openvpn \
                        network-manager-gnome

# Desktop software
sudo apt-get -y install keepassx golden-dict

# Multimedia
sudo apt-get -y install vlc x264

# Install contrib packages
sudo sed -i 's/wheezy main/wheezy main contrib/' /etc/apt/sources.list
sudo dpkg --add-architecture i386
sudo apt-get update
sudo apt-get -f install
sudo apt-get -y install flashplugin-nonfree skype


# Manual steps
# ============
# 1. Qtconfig set fonts.
# 3. Create SSH keys:
#        $ ssh-keygen -t rsa -C "rkiyanchuk@mirantis.com"
#        $ ssh-add
