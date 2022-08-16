#!/bin/sh

# rename the bundled (linux+mac ports only, Windows users need to obtain the files elsewhere)
# high-resolution textures to equivalent low-resolution texture names to trick the game into using them
# because Codemasters is absolutely ridiculous and you literally cannot "own"/"buy" the 
# delisted FREE high-resolution textures DLC from Steam whatsoever, even if you already own the game

# probably safest run at the lowest level possible
if [[ "$PWD" != */share/data/cars/models* ]]; then
    echo "Please place this script in the /share/data/cars/models/ directory"
    exit 1
fi

# rename old low texture directories
find . -depth -type d -name 'textures_low' -exec mv {} {}_bak \;
# rename texture files and directories
find . -type f -name '*.pssg' -path '*/textures_high/*' -exec prename -v 's/_high/_low/g' {} \;
# remove leftover high texture directories
find . -depth -type d -name 'textures_high' -exec rmdir {} \;


# reverse operations
#find . -type f -name '*.pssg' -path '*/textures_low/*' -exec prename -v 's/_low/_high/g' {} \;
# remove leftover low texture directories
#find . -depth -type d -name 'textures_low' -exec rmdir {} \;
# move back old low texture directories
#find . -depth -type d -name 'textures_low_bak' -execdir mv './textures_low_bak' './textures_low' \;


# removing high texture directories only (i.e. Steam Deck)
#find . -depth -type d -name 'textures_high' -exec rm -rf {} \;
