#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# This script is designed to bootstrap Debian Jessie base system to fully
# functional desktop environment under XMonad window manager. Functionality
# equivalent to desktop environments like Gnome or KDE is implemented with 
# existing CLI tools and shortcuts.


# CONFIGURATION
# =============

USE_SSD=true
SYS_DISK=sda

SYSFS_CONF=/etc/sysfs.conf
SYSCTL_LOCAL_CONF=/etc/sysctl.d/local.conf

echo "Include contrib and non-free packages"
sed -i 's/jessie main$/jessie main contrib non-free/' /etc/apt/sources.list

echo "Add 386 architecture dependencies"
dpkg --add-architecture i386
aptitude update
aptitude -f install
# Upgrade system to latest state
aptitude safe-upgrade

# Customizations for SSD
if [ $USE_SSD == true ]; then
    echo "Optimize SSD performance..."
    aptitude  install sysfsutils

    # Switch to `deadline` scheduler suitable for SSD.
    if ! grep -q "scheduler.*=.*deadline" ${SYSFS_CONF}; then
        echo "block/$SYS_DISK/queue/scheduler = deadline" >> ${SYSFS_CONF}
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
    if ! grep -q "/var/tmp" /etc/fstab; then
        echo "tmpfs /var/tmp    tmpfs   defaults,relatime,mode=1777 0 0" >> /etc/fstab
    fi
fi


# CORE GUI COMPONENTS
# ===================

echo "Install packages..."

# Core system graphics components
aptitude -y install ntp
aptitude -y install xserver-xorg xserver-xorg-input-synaptics xinit slim
aptitude -y install arandr  # GUI for xrandr
aptitude -y install xmonad libghc-xmonad-dev libghc-xmonad-contrib-dev xmobar
aptitude -y install xcompmgr  # for transparency support
aptitude -y install icc-profiles sampleicc-tools  # Color profiles

# Sound
aptitude -y install alsa pulseaudio paman pavucontrol

# Destktop GUI and usability
aptitude -y install xxkb nitrogen stalonetray
aptitude -y install suckless-tools moreutils xbacklight
aptitude -y install qt4-qtconfig 
aptitude -y install gtk2-engines gtk2-engines-murrine dmz-cursor-theme
aptitude -y install libxft2 libxft-dev
aptitude -y install libnotify-bin notify-osd
aptitude -y install libxcursor1:i386  # fixes cursor pointer problem in Skype
aptitude -y install rxvt-unicode scrot 
aptitude -y install fonts-liberation fonts-linuxlibertine

aptitude -y install network-manager network-manager-gnome 
aptitude -y install network-manager-openvpn

aptitude -y install python-pip
aptitude -y install python-notify  # dependency for udiskie
pip install udiskie  # automount usb devices


# OPTIONAL PREFERRED PACKAGES
# ===========================

# Network utils
aptitude -y install x11vnc traceroute nmap synergy
aptitude -y install openssh-client openssh-server ssh-askpass
aptitude -y install pidgin

# Misc CLI utils
aptitude -y install tree htop tmux mc vifm xclip
aptitude -y install exuberant-ctags source-highlight checkinstall
aptitude -y install mercurial git

# Python
aptitude -y install python3-all python2.7-dev python3-dev
pip install virtualenvwrapper

# Multimedia
aptitude -y install goldendict vlc x264 feh geeqie
aptitude -y install flashplugin-nonfree ttf-mscorefonts-installer
aptitude -y install tar gzip bzip unrar file-roller


# CUSTOMIZATIONS
# ==============

# Compile dmenu, patch for xft support and install
wget http://dl.suckless.org/tools/dmenu-4.5.tar.gz
wget http://tools.suckless.org/dmenu/patches/dmenu-4.5-xft-debian.diff
gunzip dmenu-4.5.tar.gz
tar -xf dmenu-4.5.tar
patch -d dmenu-4.5 < dmenu-4.5-xft-debian.diff 

cd dmenu-4.5
make
checkinstall -y
cd ..
rm -rf dmenu-4.5*


# Lock screen after suspend.
# Remember to modify `username` variable.

cat > /etc/pm/sleep.d/00screensaver-lock << 'EOF'
#!/bin/sh
#
# 00screensaver-lock: lock workstation on hibernate or suspend

dbus=$(ps aux | grep 'dbus-launch' | grep -v root)

username=$(echo $dbus | awk '{print $1}')
userhome=$(getent passwd $username | cut -d: -f6)
export XAUTHORITY="$userhome/.Xauthority"
for x in /tmp/.X11-unix/*; do
    displaynum=$(echo $x | sed s#/tmp/.X11-unix/X##)
    if [[ -f "$XAUTHORITY" ]]; then
        export DISPLAY=":$displaynum"
    fi
done

case "$1" in
    hibernate|suspend)
        su $USER -c "/usr/bin/gnome-screensaver-command -l" &
    ;;
    thaw|resume)
    ;;
    *) exit $NA
    ;;
esac
EOF

chmod 755 /etc/pm/sleep.d/00screensaver-lock
