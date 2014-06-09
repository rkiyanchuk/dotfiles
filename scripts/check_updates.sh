#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# Notifies about updates.
# Place into /etc/cron.hourly to run automatically every hour.

USER="zoresvit"

UPDATES=$(aptitude -y update | grep "Current status:" | cut -d ' ' -f 3)

if [ $UPDATES ]; then
    export DISPLAY=:0.0
    export XAUTHORITY=/home/${USER}/.Xauthority
    sudo -u ${USER} notify-send "System update" "${UPDATES} updates available." 
fi
