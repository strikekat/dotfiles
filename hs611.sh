#!/bin/bash

# Config script for drawing tablet

USB="HS611" # lsusb
EXT_SCREEN="DP-0.3" # xrandr
STYLUS="HUION Huion Tablet_HS611 stylus" # xsetwacom list devices
PAD="HUION Huion Tablet_HS611 Pad pad" # xsetwacom list devices
MONITOR="3840x2160+1920+0" # xrandr

if lsusb | grep -q $USB; then
    echo "$USB found"
else
    echo "$USB not found"
    exit 1
fi

# must map monitor to stylus, not pad
if xrandr | grep -q "$EXT_SCREEN connected"; then
    echo "External monitor found"
    xsetwacom set "$STYLUS" MapToOutput $MONITOR
else
    echo "External monitor not found"
fi

xsetwacom set "$PAD" button 1 key +Ctrl +Z
xsetwacom set "$PAD" button 2 key +Ctrl +Shift +Z
xsetwacom set "$PAD" button 3 key +K
xsetwacom set "$PAD" button 8 key +L
xsetwacom set "$PAD" button 9 key +ctrl
xsetwacom set "$PAD" button 10 key +T
xsetwacom set "$PAD" button 11 key +B
xsetwacom set "$PAD" button 12 key +E
#xsetwacom set "$PAD" button 13 "key +ISO_Level3_Shift +0xfe52"
#xsetwacom set "$PAD" button 14 "key +ISO_Level3_Shift +0x24"
