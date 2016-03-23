#!/usr/bin/env bash
# encoding: utf-8

# This script is designed to bootstrap Debian Jessie base system to fully
# functional desktop environment under XMonad window manager. Functionality
# equivalent to desktop environments like Gnome or KDE is implemented with
# existing CLI tools and shortcuts.

set -o errexit
set -o xtrace

usage() {
    echo "Usage: $0 [-s] [-d <dev>]" 1>&2;
    echo "       -s Optimized settings for SSD" 1>&2;
    echo "       -d (sda) Specify target storage device" 1>&2;
    exit 1;
}

USE_SSD=false
SYS_DISK=sda

while getopts "sd:h" opt; do
    case $opt in
        s)
            USE_SSD=true
            ;;
        d)
            SYS_DISK="$OPTARG"
            ;;
        h)
            usage
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
        :)
            echo "Option -$OPTARG requires an argument." >&2
            exit 1
            ;;
    esac
done


SYSCTL_LOCAL_CONF=/etc/sysctl.d/local.conf


# REPOSITORIES AND PACKAGING SETUP
# ================================

# Add contrib and non-free repositories
sed -i 's/jessie main$/jessie main contrib non-free/' /etc/apt/sources.list

dpkg --add-architecture i386
aptitude update
aptitude -yf install
# Upgrade system to latest state
aptitude -y safe-upgrade


# CONFIGURATION
# =============

# Customizations for SSD
if [ $USE_SSD == true ]; then
    # Optimize SSD performance...
    aptitude -y install sysfsutils

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
    if ! grep -q "/var/tmp" /etc/fstab; then
        echo "tmpfs /var/tmp    tmpfs   defaults,relatime,mode=1777 0 0" >> /etc/fstab
    fi
fi


# CORE GUI COMPONENTS
# ===================

# Core system graphics components
aptitude -y install ntp
aptitude -y install xserver-xorg xserver-xorg-input-synaptics xinit slim
aptitude -y install libghc-xmonad-dev libghc-xmonad-contrib-dev
aptitude -y install cabal-install
# XMobar dependencies:
aptitude -y install libiw-dev libghc-libxml-sax-dev c2hs libasound2-dev


cabal udpate
cabal install --global xmonad xmonad-extras
cabal install --global xmobar --flags "all_extensions"

aptitude -y install compton  # compositor for transparency support
aptitude -y install arandr  # GUI for xrandr
aptitude -y install redshift
aptitude -y install laptop-mode-tools
aptitude -y install icc-profiles # Color profiles

# Sound
aptitude -y install alsa-utils pulseaudio paman pavucontrol
aptitude -y install libpulse0:i386

# Destktop GUI and usability
aptitude -y install xxkb nitrogen stalonetray
aptitude -y install suckless-tools moreutils xbacklight
aptitude -y install qt4-qtconfig
aptitude -y install gtk2-engines gtk2-engines-murrine gtk2-engines-murrine:i386
aptitude -y install gtk2-engines-qtcurve dmz-cursor-theme
aptitude -y install libxft2 libxft-dev
aptitude -y install libnotify-bin xfce4-notifyd
aptitude -y install libxcursor1:i386  # fixes cursor pointer problem in Skype
aptitude -y install rxvt-unicode scrot
aptitude -y install fonts-liberation fonts-linuxlibertine
aptitude -y install stow conky-all

aptitude -y install network-manager network-manager-gnome
aptitude -y install network-manager-openvpn
aptitude -y install bluez-tools blueman gksu
aptitude -y install pulseaudio-module-bluetooth
aptitude -y install pmount

aptitude -y install python-setuptools python-pip python-gobject
aptitude -y install python-yaml libgio2.0-cil-dev gobject-introspection
aptitude -y install libgtk2.0-0 libnotify4 gettext gir1.2-notify-0.7

aptitude -y install firmware-linux gnome-screensaver



# OPTIONAL PREFERRED PACKAGES
# ===========================

aptitude -y install gdebi

# Network utils
aptitude -y install x11vnc traceroute nmap synergy
aptitude -y install openssh-client openssh-server sshpass
aptitude -y install irssi

# Misc CLI utils
aptitude -y install tree htop tmux mc vifm xclip autocutsel
aptitude -y install exuberant-ctags source-highlight checkinstall
aptitude -y install mercurial git shellcheck
aptitude -y install cmake

# Python
aptitude -y install python3-all python2.7-dev python3-dev

# Multimedia
aptitude -y install goldendict vlc x264 feh geeqie
aptitude -y install flashplugin-nonfree ttf-mscorefonts-installer
aptitude -y install tar gzip unrar file-roller
aptitude -y install clementine

# Security
aptitude -y install keepassx libsecret-tools apt-transport-https


# POPULATE HOME DIRECTORY
# =======================

mkdir -p $HOME/downloads
# Images, photos, audio, video, etc.
mkdir -p $HOME/media
# Software development, projects, repositories
mkdir -p $HOME/devel

# System76 Gazelle Pro
# aptitude -y install firmware-linux firmware-iwlwifi

# CUSTOMIZATIONS
# ==============

# Install Dropbox
# Install Firefox
# Install Calibre
# Install Virtualbox
# Install Vagrant
# install hplip for printing:
# http://hplipopensource.com/hplip-web/install/install/index.html

# Install GitHub hub utility
