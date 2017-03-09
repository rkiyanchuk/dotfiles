#!/bin/bash

STEP=5
ICON_HEADPHONES=""
ICON_MIC_MUTE=""
ICON_MIC_REC="<fc=#dc322f></fc>"

case $1 in
    "inc" )
        sink=$(pamixer --list-sinks | grep -oE "^[0-9]+" | tail -n 1)
        pamixer --sink ${sink} -i ${STEP}
        volnoti-show $(pamixer --sink ${sink} --get-volume)
        ;;

    "dec" )
        sink=$(pamixer --list-sinks | grep -oE "^[0-9]+" | tail -n 1)
        pamixer --sink ${sink} -d ${STEP}
        volnoti-show $(pamixer --sink ${sink} --get-volume)
        ;;

    "mute" )
        sink=$(pamixer --list-sinks | grep -oE "^[0-9]+" | tail -n 1)
        pamixer --sink ${sink} -t
        pamixer --sink ${sink} --get-mute && volnoti-show -m || volnoti-show $(pamixer --get-volume)
        ;;

    "mute-input" )
        inputs=$(pamixer --list-sources | grep input | grep -oE "^[0-9]+" | tr '\n' ' ')
        for input in ${inputs}; do
            pamixer --source ${input} -t
        done

        if [[ $(pamixer --default-source --get-mute) == "true" ]]; then
            notify-send "Inputs muted "
        else
            notify-send "Inputs unmuted "

        fi
        ;;

    "status" )
        if [[ $(pamixer --default-source --get-mute) == "true" ]]; then
            echo -n "${ICON_MIC_MUTE}"
        else
            echo -n "${ICON_MIC_REC}"
        fi
esac
