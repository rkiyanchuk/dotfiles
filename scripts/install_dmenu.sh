#!/usr/bin/env bash
# -*- coding: utf-8 -*-

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
