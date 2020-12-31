#!/bin/sh

# generic proton start script I use for non-steam games

# proton distribution directory:
export W="/home/dhouchin/.local/share/Steam/steamapps/common/Proton 5.0/dist"

# create a prefix folder in game dir
# WINEPREFIX must be absolute:
export WINEPREFIX=/home/dhouchin/games/visualpinball/prefix

###
export WINEVERPATH=$W
export PATH=$W/bin:$PATH
export WINESERVER=$W/bin/wineserver
export WINELOADER=$W/bin/wine
export WINEDLLPATH=$W/lib/wine/fakedlls
export LD_LIBRARY_PATH="$W/lib:$LD_LIBRARY_PATH"
###


# required dependencies for VPX (Uncomment for first run):
#winetricks wsh57 ole32 oleaut32

gamemoderun wine VPinballX.exe
