#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# 
# XMonad installer targeted for Ubuntu 12.04.
 
echo "Installing required packages..."
sudo apt-get install xmonad libghc-xmonad-dev libghc-xmonad-contrib-dev xmobar xcompmgr nitrogen stalonetray moreutils synapse rxvt-unicode

echo "Creating XMonad xsession configuration..."                                
sudo mv /usr/share/xsessions/xmonad.desktop /usr/share/xsessions/xmonad.desktop.original
sudo cp ~/.xmonad/xmonad.desktop /usr/share/xsessions/xmonad.desktop
sudo cp ~/.xmonad/images/xmonad_badge.png /usr/share/unity-greeter/xmonad_badge.png
