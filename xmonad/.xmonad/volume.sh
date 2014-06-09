#!/usr/bin/env bash
# -*- coding: utf-8 -*-


mixer=$(amixer get Master | grep 'Mono:' -A 1 | cut -d ' ' -f 7 | cut -b 2,3,4)

muted=$(amixer get Master | grep 'Mono:' -A 1 | cut -d ' ' -f 8)
if [ $muted == "[off]" ]; then
    echo "--%"
else
    echo $mixer
fi
