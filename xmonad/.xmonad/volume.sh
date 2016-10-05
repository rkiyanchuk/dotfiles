#!/bin/bash

STATUS=($(pulseaudio-ctl full-status))

VOLUME=${STATUS[0]}
[[ ${STATUS[1]} = "no" ]] && OUTPUT_MUTED="%" || OUTPUT_MUTED="-"
[[ ${STATUS[2]} = "no" ]] && INPUT_MUTED=" " || INPUT_MUTED="!"

echo "${VOLUME}${OUTPUT_MUTED}${INPUT_MUTED}"
