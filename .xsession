#!/bin/sh
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# Based on David Brewer's XMonad configuration:
# https://github.com/davidbrewer/xmonad-ubuntu-conf


# Identify the home of our gtkrc file, important for setting styles of
# gtk-based applications
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# Load X resources (fixes some font issues)
xrdb -merge .Xdefaults

# Start compositing to support transparency. You can omit this
# if you prefer not to use any transparency, but it is likely to cause
# ugly black fringing with some programs such as synapse that expect
# transparency support.
xcompmgr -n &


# Screen configuration
# ====================

# We handle our screen configuration using xrandr. You are likely to want to
# edit this section to match the specific demands of your setup. The below
# setup is for a Lenovo W520 laptop that sometimes has an external monitor
# hooked up via either VGA or DVI, positioned to the left of the laptop.
#
# Note that you can run xrandr at the command line to get a list of your
# connected screens and find out what names you should use to refer to them.

# Activate primary screen
xrandr --output default --auto --primary

# If we find that a screen is connected via VGA, activate it and position it
# to the left of the primary screen.
xrandr | grep 'VGA1 connected' | ifne xrandr --output VGA1 --auto --left-of default

# If we find that a screen is connected via DVI, activate it and position it
# to the left of the primary screen.
xrandr | grep 'HDMI1 connected' | ifne xrandr --output HDMI1 --auto --right-of default


# Load screen backgrounds
# =======================

# To set your backgrounds, run nitrogren /path/to/images
nitrogen --restore &


# Startup icon tray
# =================

# We are using stalonetray to create a small icon tray at the
# top right of the screen. You are likely to want to tweak the
# size of the icons and the width of the tray based upon the
# size of your screen and your xmobar configuration. The goal is
# to make stalonetray look like it is part of xmobar. The configuration is set
# in `.stalonetrayrc` file.

stalonetray &

# Run the gnome-keyring-daemon to avoid issues you otherwise may encounter
# when using gnome applications which expect access to the keyring, such
# as Empathy. This prevents prompts you may otherwise get for invalid
# certificates and the like.
gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh

# Remap caps lock to switch keyboard layout.
setxkbmap -layout 'us,ru,ua' -option 'grp:caps_toggle,grp_led:caps'

# Allow custom pointers.
xsetroot -cursor_name left_ptr

# Start USB automount daemon
udiskie &

# Turn off PC speaker bell
xset -b

# Start notification daemon
/usr/lib/notify-osd/notify-osd &

# Start synaptic daemon for keyboard monitoring
syndaemon -i 0.5 -K -d

xscreensaver -no-splash &

exec xmonad
