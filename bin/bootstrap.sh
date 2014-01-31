#!/usr/bin/env bash

USE_SSD=true
SYS_DISK=sda

LOG_FILE=~/bootstrap.log
SYSFS_CONF=/etc/sysfs.conf
SYSCTL_LOCAL_CONF=/etc/sysctl.d/local.conf

# Add 386 architecture dependencies
dpkg --add-architecture i386
aptitude update
aptitude -y -f install

# Upgrade system to latest state
aptitude -y safe-upgrade

# Customizations for SSD
if [ $USE_SSD == true ]; then
    echo "Optimize SSD performance..." | tee ${LOG_FILE}
    aptitude -y install sysfsutils
    
    # Switch to `deadline` scheduler suitable for SSD.
    if ! grep -q "scheduler.*=.*deadline" ${SYSFS_CONF}; then
        echo "block/$SYS_DISK/queue/scheduler = deadline" >> ${SYSFS_CONF}
    else
        echo "WARNING: failed to set IO scheduler." >> ${LOG_FILE}
    fi

    # Tweak kernel parameters.
    if [ -f ${SYSCTL_LOCAL_CONF} ]; then
        sed -i '/^vm.swappiness/c\vm.swappiness=0' ${SYSCTL_LOCAL_CONF}
        sed -i '/^vm.vfs_cache_pressure/c\vm.vfs_cache_pressure=50' ${SYSCTL_LOCAL_CONF}
    else
        touch ${SYSCTL_LOCAL_CONF}
        echo "vm.swappiness=0" >> ${SYSCTL_LOCAL_CONF}
        echo "vm.vfs_cache_pressure=50" >> ${SYSCTL_LOCAL_CONF}
    fi

    # Mount var directories to tmpfs to keep them in RAM.
    if ! grep -q "/var/spool" /etc/fstab; then
        echo "tmpfs /var/spool tmpfs defaults,noatime,mode=1777 0 0" >> /etc/fstab
    fi
    if ! grep -q "/var/tmp" /etc/fstab; then
        echo "tmpfs /var/tmp tmpfs defaults,noatime,mode=1777 0 0" >> /etc/fstab
    fi
fi


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
