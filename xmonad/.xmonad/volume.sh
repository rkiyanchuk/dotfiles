#!/usr/bin/env bash
# -*- coding: utf-8 -*-

ACCENT_COLOR="#cb4b16"
THRESHOLD="79"

if [[ $(hostname) == "zion" ]]; then
    CHANNEL="Master"
    DEV_NUM="1"
elif [[ $(hostname) == "rishon" ]]; then
    CHANNEL="Headphone"
    DEV_NUM="1"
fi

VOLUME=$(amixer get ${CHANNEL} -c ${DEV_NUM} | egrep -om 1 "\[[[:digit:]]+%\]")
VOLUME_VALUE=$(sed "s/[^0-9]//g" <<< ${VOLUME})
MUTED=$(amixer get ${CHANNEL} -c ${DEV_NUM} | egrep -om 1 '\[off\]|\[on\]')

if [[ ${MUTED} == "[off]" ]]; then
    echo "<fc=${ACCENT_COLOR}>--% </fc>"
else
    if (( ${VOLUME_VALUE} > ${THRESHOLD} )); then
        echo "<fc=${ACCENT_COLOR}>"${VOLUME_VALUE}"% </fc>"
    else
        # Extra whitespace fixes `%` symbol corruption
        echo "${VOLUME_VALUE}% "
    fi
fi
