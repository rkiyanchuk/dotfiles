#!/usr/bin/env bash
# encoding: utf-8
# Activate primary screen (find first one connected).
MONITOR=$(xrandr | grep " connected" | cut -d' ' -f 1 | head -n 1)
xrandr --output "${MONITOR}" --auto --primary

# Activate secondary screens if connected.
xrandr | grep 'VGA1 connected' | ifne xrandr --output VGA1 --auto --right-of "${MONITOR}"
xrandr | grep 'VGA1 disconnected' | ifne xrandr --output VGA1 --off

xrandr | grep 'HDMI1 connected' | ifne xrandr --output HDMI1 --auto --left-of "${MONITOR}"
xrandr | grep 'HDMI1 disconnected' | ifne xrandr --output HDMI1 --off
