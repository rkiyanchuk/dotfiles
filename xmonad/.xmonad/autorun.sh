#!/usr/bin/env bash
# encoding: utf-8

if ! pgrep nm-applet > /dev/null; then
    nm-applet &  # Start network manager
fi

if ! pgrep dropbox > /dev/null; then
    dropbox start &
fi

# XXkb keyboard indicator.
if ! pgrep xxkb > /dev/null; then
    xxkb &
fi

# Temporarily disabled until icon issues fixed in Debian.
if ! pgrep blueman-applet > /dev/null; then
    blueman-applet &
fi

if ! pgrep conky > /dev/null ; then
    # Start conky after wallpaper is loaded for correct drawing.
    sleep 1; conky -d
fi
