#!/bin/bash

STEP=5
ICON_HEADPHONES=""
ICON_MIC_MUTE=""
ICON_MIC_REC="<fc=#dc322f></fc>"

case $1 in
    "inc" )
        pamixer -i ${STEP}
        volnoti-show $(pamixer --get-volume)
        ;;

    "dec" )
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
        notify-send "Inputs muted "
        ;;

    "unmute-input" )
        inputs=$(pamixer --list-sources | grep input | grep -oE "^[0-9]+" | tr '\n' ' ')
        for input in ${inputs}; do
            pamixer --source ${input} --unmute
        done
        notify-send "Inputs unmuted "
        ;;

    "status" )
        if [[ $(pamixer --default-source --get-mute) == "true" ]]; then
            echo -n "${ICON_MIC_MUTE}"
        else
            echo -n "${ICON_MIC_REC}"
        fi
esac
