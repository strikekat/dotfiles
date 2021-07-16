#!/bin/sh

# Gnome settings overrides X config additively upon login
# So this must be run _after_ Gnome applies display preferences
# XProfile is not getting executed and shell Profile seems to be too early
# ~/.config/autostart with included .desktop file seems to work

if [[ -z "${DISPLAY}" ]]; then
    echo "X11 is not running"
    exit 1
fi

if xrandr | grep -q "DP-0.3 connected"; then
    xrandr --output DP-0.3 --primary --pos 1920x0 --output DP-1 --scale 0.5x0.5 --pos 0x120
fi
