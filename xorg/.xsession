#!/usr/bin/env bash
# encoding: utf-8


# CONFIGURATION VARIABLES
# =======================

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export DESKTOP_SESSION="gnome"  # Needed for Qt applications.

# Time zone.
export TZ='America/Los_Angeles'
#export TZ='Europe/Kiev'


# SCREENS
# =======

# Activate primary screen (find first one connected).
MONITOR=$(xrandr | grep " connected" | cut -d' ' -f 1 | head -n 1)
xrandr --output "${MONITOR}" --auto --primary

# Activate secondary screens if connected.
xrandr | grep 'VGA1 connected' | ifne xrandr --output VGA1 --auto --right-of "${MONITOR}"
xrandr | grep 'VGA1 disconnected' | ifne xrandr --output VGA1 --off

xrandr | grep 'HDMI1 connected' | ifne xrandr --output HDMI1 --auto --left-of "${MONITOR}"
xrandr | grep 'HDMI1 disconnected' | ifne xrandr --output HDMI1 --off


# DESKTOP ENVIRONMENT
# ===================

redshift -O 6500 -g 0.9:0.9:0.9 > /dev/null  # Adjust display color temprature.
nitrogen --restore &  # Set screen wallpaper (see man for choosing image).

xset -b  # Turn off PC speaker bell.
xrdb -merge ${HOME}/.Xdefaults  # Load X resources.

xsetroot -cursor_name left_ptr  # Allow custom pointers.

if ! pgrep syndaemon > /dev/null; then
    syndaemon -i 0.5 -K -d  # Start synaptics daemon for keyboard monitoring.
fi

if ! pgrep stalonetray > /dev/null; then
    stalonetray &  # Start system tray.
fi

if ! pgrep gnome-keyring-daemon > /dev/null; then
    # Start keyring for applications that expect access to system keyring.
    gnome-keyring-daemon --daemonize --start \
        --components=gpg,pkcs11,secrets,ssh > /dev/null
fi

# Configure keyboard bindings.
setxkbmap -layout 'us,ru,ua' \
    -variant 'altgr-intl,,' \
    -option 'grp:caps_toggle' \
    -option 'grp_led:caps' \
    -option 'compose:menu-altgr' \
    -option 'lv3:ralt_switch'

gnome-screensaver > /dev/null 2>&1 &

if ! pgrep compton > /dev/null; then
    # Try to start Compton compositor on up to 4 displays.
    seq 0 3 | xargs -l1 -I@ compton -b -d :0.@
fi

if ! pgrep xmonad > /dev/null; then
    exec xmonad
fi
