#!/bin/bash

ACTION=$1  # 'raise', 'lower', 'mute'
DEFAULT_CHANNEL=Master

# Ugly way to find out PS USB Wired Headset sink number.
HEADSET_SINK=$(pacmd list-sinks | grep -B 1 -e alsa_output.*Wired_USB_Headset | grep index | grep -oe "[0-9]")

if [ "${ACTION}" = 'raise' ]; then
    amixer -q set ${DEFAULT_CHANNEL} 5%+
    pactl set-sink-volume "${HEADSET_SINK}" +5%
fi

if [ "${ACTION}" = 'lower' ]; then
    amixer -q set ${DEFAULT_CHANNEL} 5%-
    pactl set-sink-volume "${HEADSET_SINK}" -- -5%
fi

if [ "${ACTION}" = 'mute' ]; then
    amixer -q set ${DEFAULT_CHANNEL} toggle
    amixer -q -c 2 set PCM toggle  # mute PS Wireless USB Headset.
    pactl set-sink-mute "${HEADSET_SINK}" toggle
fi
