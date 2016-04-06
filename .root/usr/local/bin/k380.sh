#! /bin/bash
line=$(dmesg | grep -i k380 | grep hidraw)
[[ $line =~ (.*)(hidraw+[^:])(.*) ]]
device=${BASH_REMATCH[2]}
/usr/local/bin/k380_conf -d "/dev/${device}" -f on


## Configure Bluetooth keyboard.
export DISPLAY=:0.0
export XAUTHORITY='/home/zoresvit/.Xauthority'

K380_KEYBOARD=$(xinput -list | grep "Keyboard K380" | sed 's/.*id=\([0-9]*\).*/\1/')
if [ ! -z "${K380_KEYBOARD}" ]; then
    setxkbmap -device "${K380_KEYBOARD}" \
    -layout 'us,ru,ua' \
    -variant 'altgr-intl,,' \
    -option 'grp:caps_toggle' \
    -option 'lv3:ralt_switch' \
    -option 'compose:rctrl-altgr'
fi

xset -b
