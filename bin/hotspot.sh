#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# Start with root privileges.

if [ $1 == "start" ]; then
    sudo ifconfig wlan0 up 10.10.0.1;
    sudo service isc-dhcp-server start;
    sudo service hostapd start
elif [ $1 == "stop" ]; then
    sudo ifconfig wlan0 down;
    sudo service isc-dhcp-server stop;
    sudo service hostapd stop
fi
