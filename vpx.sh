#!/usr/bin/env bash

VPX_TABLES=~/vpx/
VPX_EXEC=~/bin/VPinballX_GL.app/Contents/MacOS/VPinballX_GL

FILE="1"

while [ -n "$FILE" ]
do
    FILE=$(zenity --file-selection --title "Select Table" --file-filter='VPX Tables | *.vpx' --file-filter='All files | *' --filename "$VPX_TABLES")

    if [ -z "$FILE" ]; then
        exit
    fi

    $VPX_EXEC -play "$FILE"

    if [[ $(uname) == 'Darwin' ]]; then
        osascript <<EOS
        tell application "System Events"
            tell process "Terminal"
                set frontmost to true
            end tell
        end tell
EOS
    fi
done

exit
