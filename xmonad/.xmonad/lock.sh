#!/bin/bash

# Mute volume on screen lock.
# Listen DBUS queue to get signal from Gnome Screensaver about lock events.

DBUS_SESSION="type='signal',interface='org.gnome.ScreenSaver'"
MUTE_COMMAND="${HOME}/.xmonad/volume.sh mute"

function mute_on_lock() {
    while IFS= read -r line; do
        if [[ "$line" == *"boolean true"* ]]; then
            eval "${MUTE_COMMAND}"
        fi
        if [[ "$line" == *"boolean false"* ]]; then
            eval "${MUTE_COMMAND}"
        fi
    done
}

dbus-monitor --session "${DBUS_SESSION}" | mute_on_lock
