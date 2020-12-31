#!/bin/bash

USB="HS611"
EXT_SCREEN="DP-0.3"
STYLUS="HUION Huion Tablet_HS611 stylus"
PAD="HUION Huion Tablet_HS611 Pad pad"

if lsusb | grep -q $USB; then
    echo "$USB found"
else
    echo "$USB not found"
    exit 1
fi

if xrandr | grep -q "$EXT_SCREEN connected"; then
    echo "External monitor found"
    xsetwacom set "$STYLUS" MapToOutput 3840x2160+1920+0
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
