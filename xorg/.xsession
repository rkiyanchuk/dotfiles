#!/usr/bin/env bash
# encoding: utf-8

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export QT_QPA_PLATFORMTHEME="qt5ct"  # Needed for Qt applications to configure theme.

# Time zone.
export TZ='America/Los_Angeles'
#export TZ='Europe/Amsterdam'
#export TZ='Europe/Kiev'


function spawn {
    PROG=$1
    CMD=$2
    if ! pgrep -x $PROG > /dev/null; then
        ${CMD:-$PROG} > /dev/null 2>&1 &
    fi
}

# Set screen wallpaper (see man for choosing image).
feh --bg-fill ${HOME}/.xmonad/images/zoresvit_wallpaper.png &

xset -b  # Turn off PC speaker bell.
xrdb -merge ${HOME}/.Xdefaults  # Load X resources.
xsetroot -cursor_name left_ptr  # Allow custom pointers.

if ! pgrep -x ssh-agent > /dev/null; then
    eval $(ssh-agent -s)
fi

spawn gnome-screensaver
spawn udiskie
spawn lock.sh 'setsid "${HOME}/.xmonad/lock.sh"'
spawn stalonetray  # Start system tray.
spawn redshift

# Start keyring for applications that expect access to system keyring.
spawn gnome-keyring-daemon 'gnome-keyring-daemon --daemonize --start \
    --components=gpg,pkcs11,secrets,ssh'

# Try to start Compton compositor on up to 4 displays.
spawn compton 'seq 0 3 | xargs -l1 -I@ compton -b -d :0.@'

spawn xmonad 'exec xmonad'
