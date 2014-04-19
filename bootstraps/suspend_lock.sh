#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# Lock screen after suspend.
# NEEDS ROOT PERMISSIONS:
# sudo bash ./suspend_lock.sh
# Remember to modify `username` variable.

cat > /etc/pm/sleep.d/00screensaver-lock << 'EOF'
#!/bin/sh
#
# 00screensaver-lock: lock workstation on hibernate or suspend

dbus=$(ps aux | grep 'dbus-launch' | grep -v root)

username=$(echo $dbus | awk '{print $1}')
userhome=$(getent passwd $username | cut -d: -f6)
export XAUTHORITY="$userhome/.Xauthority"
for x in /tmp/.X11-unix/*; do
    displaynum=$(echo $x | sed s#/tmp/.X11-unix/X##)
    if [[ -f "$XAUTHORITY" ]]; then
        export DISPLAY=":$displaynum"
    fi
done

case "$1" in
    hibernate|suspend)
        su $USER -c "/usr/bin/gnome-screensaver-command -l" &
    ;;
    thaw|resume)
    ;;
    *) exit $NA
    ;;
esac
EOF

chmod 755 /etc/pm/sleep.d/00screensaver-lock
