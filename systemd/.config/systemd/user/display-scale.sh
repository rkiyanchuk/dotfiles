#!/bin/bash

echo "start listening...."
dbus-monitor "type='signal',sender='org.gnome.Mutter.DisplayConfig',interface='org.gnome.Mutter.DisplayConfig'" |
while read -r line; do
    status=$(cat /sys/class/drm/card0-DP-1/status)

    if [[ $status == "disconnected" ]]; then
        echo "changing to builtin"
        gsettings set org.gnome.desktop.interface text-scaling-factor 1.2 
    elif [[ $status == "connected" ]]; then
        echo "changing to monitor"
        gsettings set org.gnome.desktop.interface text-scaling-factor 0.9 
    fi
done
