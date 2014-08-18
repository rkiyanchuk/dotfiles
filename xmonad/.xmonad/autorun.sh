#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# 
 
# xmonad "startup hook" script. This gets run after XMonad is initialized,
# via the startupHook facility provided by XMonad. It's useful for 
# running any programs which you want to use within xmonad but
# which don't need to be initialized before XMonad is running. 
#
# Author: David Brewer
# Repository: https://github.com/davidbrewer/xmonad-ubuntu-conf

# Network manager, so we don't have to configure wifi at the command line.
# `sm-disable` prevents duplicating `nm-applet` instances.
if [ -z "$(pgrep nm-applet)" ] ; then
    nm-applet --sm-disable &
fi

# Applet for managing print jobs from the tray.
#if [ -z "$(pgrep system-config-printer-applet)" ] ; then
#     system-config-printer-applet &
#fi

# Dropbox client.
if [ -z "$(pgrep dropbox)" ] ; then
    dropbox start &
fi

# XXkb keyboard indicator.
if [ -z "$(pgrep xxkb)" ] ; then
    xxkb &
fi

# Temporarily disabled until fixed in Debian Jessie
#if [ -z "$(pgrep blueman-applet)" ] ; then
#    blueman-applet &
#fi
