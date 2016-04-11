#!/bin/bash

ACTION=$1  # 'raise', 'lower', 'mute'
DEFAULT_CHANNELS=(Master)

# Ugly way to find out USB Headset sink number.
HEADSET_SINK=$(pacmd list-sinks | grep -B 1 -e alsa_output.*Wired_USB_Headset | grep index | grep -oe "[0-9]")

if [ "${ACTION}" = 'raise' ]; then
    for channel in "${DEFAULT_CHANNELS[@]}" ; do
        amixer -q set $channel 5%+
    done
    pactl set-sink-volume "${HEADSET_SINK}" +5%
fi

if [ "${ACTION}" = 'lower' ]; then
    for channel in "${DEFAULT_CHANNELS[@]}" ; do
        amixer -q set $channel 5%-
    done
    pactl set-sink-volume "${HEADSET_SINK}" -- -5%
fi

if [ "${ACTION}" = 'mute' ]; then
    for channel in "${DEFAULT_CHANNELS[@]}" ; do
        amixer -q set $channel toggle
    done
    pactl set-sink-mute "${HEADSET_SINK}" toggle
fi
