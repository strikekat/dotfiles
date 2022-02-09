#!/bin/bash

# Compress all folders in current directory to zstd archives

for d in */ ; do
    dir=$(basename "$d")
    echo "Processing $d"
    tar --zstd -cvf "$dir.tar.zst" "$d"
done

echo "Done"
