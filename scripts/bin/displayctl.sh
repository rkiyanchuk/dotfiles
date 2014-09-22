#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# Compute and adjust display parameters in XMonad.


usage() {
    echo "Usage: $0 [-h] [-f <fontsize>] [-i <iconsize>] \
[-t <trayicons>] [-w <width>]" 1>&2;
    echo "       -f Font size to set."
    echo "       -i Icon size to use in status bar."
    echo "       -t Number of tray icons to reserve space for."
    echo "       -w Width of XMobar."
    exit 1;
}


TRAYICONS=8
ICONSIZE=24
FONTSIZE=12
WIDTH=$(xrandr | grep primary | grep -o -e "[0-9]\{3,4\}" | sed -n 1p)
HEIGHT=$(xrandr | grep primary | grep -o -e "[0-9]\{3,4\}" | sed -n 2p)

while getopts "f:i:t:w:h" opt; do
    case $opt in
        f)
            FONTSIZE="$OPTARG"
            ;;
        i)
            ICONSIZE="$OPTARG"
            ;;
        t)
            TRAYICONS="$OPTARG"
            ;;
        w)
            WIDTH="$OPTARG"
            ;;
        h)
            usage
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
        :)
            echo "Option -$OPTARG requires an argument." >&2
            exit 1
            ;;
    esac
done

XMOBAR_WIDTH=$(($WIDTH - ($ICONSIZE * $TRAYICONS)))

echo "Set values:"
echo -e "\tFont size: $FONTSIZE"
echo -e "\tTray icons: $TRAYICONS"
echo -e "\tIcon size: $ICONSIZE"
echo -e "\tScreen dimensions: ${WIDTH}x${HEIGHT}"
echo -e "\tXMobar width: $XMOBAR_WIDTH"

# .Xdefaults
sed --follow-symlinks -i "s/\(\#define FONT.*\)[0-9]\{2\}/\1${FONTSIZE}/g" ~/.Xdefaults

# GTK
sed --follow-symlinks -i "s/\(gtk-font-name.*\)[0-9]\{2\}/\1${FONTSIZE}/g" ~/.gtkrc-2.0

# XMobar
sed --follow-symlinks -i "s/width=[0-9]\+/width=${XMOBAR_WIDTH}/g" ~/.xmobarrc

# stalonetray
sed --follow-symlinks -i "s/^geometry.*/geometry ${TRAYICONS}x1+${XMOBAR_WIDTH}+0/g" ~/.stalonetrayrc
sed --follow-symlinks -i "s/^icon_size.*/icon_size ${ICONSIZE}/g" ~/.stalonetrayrc

# XMonad
sed --follow-symlinks -i "s/\(.*dmenu_run.*\)-[0-9]\+/\1-${FONTSIZE}/g" ~/.xmonad/xmonad.hs
