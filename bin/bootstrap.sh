#!/usr/bin/env bash

USE_SSD=true
SYS_DISK=sda

# Add 386 architecture dependencies
dpkg --add-architecture i386
aptitude update
aptitude -y -f install

# Upgrade system to latest state
aptitude -y safe-upgrade

# Customizations for SSD
if [ $USE_SSD == true ]; then
    aptitude -y install sysfsutils
    echo "block/$SYS_DISK/queue/scheduler = deadline" >> /etc/sysfs.conf




# Update the kernel manually


##############################################################################

## Basic desktop components
#sudo apt-get -y install xserver-xorg xinit slim xmonad alsa xxkb \
#                        libghc-xmonad-dev libghc-xmonad-contrib-dev xmobar \
#                        xcompmgr nitrogen stalonetray moreutils synapse ntp
#
## Pulseaudio controls
#sudo apt-get -y install paman pavucontrol
#
## Improve GUI
#sudo apt-get -y install fonts-liberation fonts-linuxlibertine qt4-qtconfig shiki-brave-theme \
#                        dmz-cursor-theme
#
## Helpers for better user-experience
#
#
## Configuration specific dependencies
#sudo apt-get -y install mercurial libxft-dev libxft2
#
## OPTIONAL PREFERED SOFTWARE
#
## Python
#sudo apt-get -y install python3-all python2.7-dev python3-dev python-pip
#
## Automount USB devices.
#sudo pip install udiskie
#
## Python global dependencies
#sudo pip install virtualenvwrapper
#
## CLI Tools
#sudo apt-get -y install tree htop tmux openssh-client openssh-server mc vifm \
#                        rxvt-unicode scrot xclip exuberant-ctags source-highlight
#
## Build tools
#sudo apt-get -y install make checkinstall
#
#
## Network tools
#sudo apt-get -y install x11vnc traceroute
#sudo apt-get -y install network-manager network-manager-openvpn \
#                        network-manager-gnome
#
## Desktop software
#sudo apt-get -y install keepassx goldendict pidgin iceweasel
#
## Multimedia
#sudo apt-get -y install vlc x264 libnotify-bin notify-osd
#
## Install contrib packages
#sed -i 's/wheezy main$/wheezy main contrib non-free/' /etc/apt/sources.list
#sudo apt-get update
#sudo apt-get -f install
#sudo apt-get -y install flashplugin-nonfree ttf-mscorefonts-installer
#
## Clean up unneeded packages
#sudo apt-get -f install
#sudo apt-get autoremove
#
#
## Manual steps
## ============
## 1. Qtconfig set fonts.
## 3. Create SSH keys:
##        $ ssh-keygen -t rsa -C "rkiyanchuk@mirantis.com"
##        $ ssh-add
