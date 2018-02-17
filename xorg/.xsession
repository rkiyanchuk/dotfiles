#!/usr/bin/env bash
# encoding: utf-8

export QT_QPA_PLATFORMTHEME="qt5ct"  # Needed for Qt applications to configure theme.
export QT_AUTO_SCREEN_SCALE_FACTOR=0  # Fix QT 5 wrong HiDPI scaling.

# Time zone.
export TZ='America/Los_Angeles'
#export TZ='Europe/Amsterdam'
#export TZ='Europe/Kiev'

if [[ "${SSH_AGENT_PID}" == "" ]]; then
    eval $(ssh-agent) > /dev/null 2>&1
fi

if [[ ! $(pgrep -f "lock.sh") ]]; then
    bash ${HOME}/.xmonad/lock.sh &
fi
