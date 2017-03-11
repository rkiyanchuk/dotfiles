#!/bin/bash

# Mute volume on screen lock.
# Listen DBUS queue to get signal from Gnome Screensaver about lock events.

DBUS_SESSION="type='signal',interface='org.gnome.ScreenSaver'"
SINKS=$(pamixer --list-sinks | grep alsa | awk '{print $1}' | tr '\n' ' ')
SOURCES=$(pamixer --list-sources | grep alsa | awk '{print $1}' | tr '\n' ' ')

function mute {
    for sink in ${SINKS}; do
        pamixer --sink ${sink} --mute
    done
    for source in ${SOURCES}; do
        pamixer --source ${source} --mute
    done
}

function unmute {
    for sink in ${SINKS}; do
        pamixer --sink ${sink} --unmute
    done
}

function mute_on_lock() {
    while IFS= read -r line; do
        if [[ "$line" == *"boolean true"* ]]; then
            mute
        fi
        if [[ "$line" == *"boolean false"* ]]; then
            unmute
        fi
    done
}

dbus-monitor --session "${DBUS_SESSION}" | mute_on_lock
