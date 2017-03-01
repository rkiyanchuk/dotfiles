#!/usr/bin/env bash
# encoding: utf-8


export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export QT_QPA_PLATFORMTHEME="qt5ct"  # Needed for Qt applications to configure theme.

# Time zone.
export TZ='America/Los_Angeles'
#export TZ='Europe/Amsterdam'
#export TZ='Europe/Kiev'


feh --bg-fill ${HOME}/.xmonad/images/zoresvit_wallpaper.png &  # Set screen wallpaper (see man for choosing image).

xset -b  # Turn off PC speaker bell.
xrdb -merge ${HOME}/.Xdefaults  # Load X resources.
xsetroot -cursor_name left_ptr  # Allow custom pointers.

gnome-screensaver > /dev/null 2>&1 &

if ! pgrep gnome-keyring-daemon > /dev/null; then
    # Start keyring for applications that expect access to system keyring.
    gnome-keyring-daemon --daemonize --start \
        --components=gpg,pkcs11,secrets,ssh > /dev/null
fi

if ! pgrep lock.sh > /dev/null; then
    setsid "${HOME}/.xmonad/lock.sh" >/dev/null 2>&1 < /dev/null &
fi

if ! pgrep udiskie > /dev/null; then
    udiskie >/dev/null 2>&1 < /dev/null &
fi

if ! pgrep ssh-agent > /dev/null; then
    eval $(ssh-agent -s)
fi

if ! pgrep compton > /dev/null; then
    # Try to start Compton compositor on up to 4 displays.
    seq 0 3 | xargs -l1 -I@ compton -b -d :0.@
fi

if ! pgrep stalonetray > /dev/null; then
    sleep 3 && stalonetray &  # Start system tray.
fi

if ! pgrep xmonad > /dev/null; then
    exec xmonad
fi
