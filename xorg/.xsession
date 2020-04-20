#!/usr/bin/env bash
# encoding: utf-8

export QT_QPA_PLATFORMTHEME="qt5ct"  # Needed for Qt applications to configure theme.
export QT_AUTO_SCREEN_SCALE_FACTOR=0  # Fix QT 5 wrong HiDPI scaling.
export QT_LOGGING_RULES="qt5ct.debug=false"  # https://superuser.com/a/1308321/173270

# Time zone.
export TZ='America/Los_Angeles'
#export TZ='Europe/Amsterdam'
#export TZ='Europe/Kiev'
#export TZ='Asia/Jerusalem'

# Set styles for Java application.
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on \
    -Djdk.gtk.version=3 \
    -Dswing.aatext=true \
    -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel \
    -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel \
    -Dsun.java2d.opengl=true"

# Tell Java that XMonad is non-reparenting WM.
# Needed for correct Java apps windows operation under XMonad.
export _JAVA_AWT_WM_NONREPARENTING=1

# Explicitely set XDG_RUNTIME_DIR for systemd.
# See bug for details:
# https://answers.launchpad.net/ubuntu/+source/systemd/+question/287454
export XDG_RUNTIME_DIR=/run/user/`id -u`

xset -b  # Disable PC speaker bip.

if [[ "${SSH_AGENT_PID}" == "" ]]; then
    eval "$(ssh-agent)" > /dev/null 2>&1
fi

if [[ ! $(pgrep -f "lock.sh") ]]; then
    bash "${HOME}/.xmonad/lock.sh" &
fi
