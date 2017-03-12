#!/bin/bash

STEP=5
BRIGHTNESS_ICON="/usr/share/pixmaps/volnoti/display-brightness-symbolic.svg"

case $1 in
    "vol-inc" )
        pamixer -i ${STEP}
        volnoti-show $(pamixer --get-volume)
        ;;

    "vol-dec" )
        pamixer -d ${STEP}
        volnoti-show $(pamixer --get-volume)
        ;;

    "mute" )
        pamixer -t
        pamixer --get-mute && volnoti-show -m || volnoti-show $(pamixer --get-volume)
        ;;

    "mute-input" )
        inputs=$(pamixer --list-sources | grep input | grep -oE "^[0-9]+" | tr '\n' ' ')
        for input in ${inputs}; do
            pamixer --source ${input} --mute
        done
        notify-send "All inputs muted "
        ;;

    "unmute-input" )
        pamixer --default-source --unmute
        notify-send "Input unmuted "
        ;;

    "br-inc" )
        xbacklight -inc ${STEP}
        volnoti-show -s ${BRIGHTNESS_ICON} $(xbacklight -get)
        ;;

    "br-dec" )
        xbacklight -dec ${STEP}
        volnoti-show -s ${BRIGHTNESS_ICON} $(xbacklight -get)
        ;;
esac
