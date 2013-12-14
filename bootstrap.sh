#!/usr/bin/env bash


# Install basic desktop components
sudo apt-get -y install git mercurial xerver-org xinit slim xmonad alsa xxkb 
sudo apt-get -y install paman pavucontrol 

# Install packages for better GUI
sudo apt-get -y install fonts-liberation ttf-mscorefonts-installer \
qt4-qtconfig shiki-brave-colors dmz-cursor-theme


# Install helpers for better user-experience
sudo apt-get -y install network-manager network-manager-openvpn network-manager-gnome usbmount

# Enforce usbmount settings
sed -i 's/^.*FS_MOUNTOPTIONS.*$/FS_MOUNTOPTIONS="-fstype=ntfs,nls=utf8,umask=007,gid=46 -fstype=vfat,gid=floppy,dmask=0007,fmask=0117"/' /etc/usbmount/usbmount.conf
sed -i 's/^.*FILESYSTEMS.*$/FILESYSTEMS="vfat ntfs ext2 ext3 ext4 hfsplus"/' /etc/usbmount/usbmount.conf



# Install contrib packages
sudo sed -i 's/wheezy main/wheezy main contrib/' /etc/apt/sources.list
sudo dpkg --add-architecture i386
sudo apt-get update
sudo apt-get -f install

sudo apt-get -y install flashplugin-nonfree skype


# Install configuration specific dependencies

# Install dotfiles for resource configuration
git clone https://github.com/zoresvit/dotfiles.git .dotfiles
./.dotfiles/bin/dfm install



sudo apt-get install -y  source-highlight suckless-tools gnome-screensaver



# Install prefered software

# Python
sudo apt-get -y install python3-all python2.7-dev python3-dev python-pip 

# Tools
sudo apt-get -y install tree gparted htop tmux openssh-client openssh-server mc        

# Compiling/packaging
sudo apt-get -y install gcc g++ clang checkinstall

#  command-line tools/helpers
sudo apt-get -y install rxvt-unicode scrot x264 xclip exuberant-ctags  

# Desktop software
sudo apt-get -y install keepassx vlc mupdf golden-dict

# Network tools
sudo apt-get -y install nmap x11vnc traceroute nmap
