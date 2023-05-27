#!/bin/bash

# This only works in WSL

set -e

./build.sh

d=/mnt/c/Windows/Fonts/
#d=/mnt/c/Windows/Fonts/arial
#d=/usr/share/fonts/truetype/ubuntu/

nfiles=0
for f in $d/*.ttf ; do

	./build/cali "$f"
	ttf=$(basename "$f")
	ppm="./build/${ttf%.ttf}.ppm"
	png="./build/${ttf%.ttf}.png"
	magick.exe "$ppm" "$png"

	((nfiles=nfiles+1))

done

for f in $d/*.TTF ; do

	./build/cali "$f"
	ttf=$(basename "$f")
	ppm="./build/${ttf%.TTF}.ppm"
	png="./build/${ttf%.TTF}.png"
	magick.exe "$ppm" "$png"

	((nfiles=nfiles+1))

done

echo "Rendered $nfiles files"

##!/bin/bash
#set -xe
#for ppm in build/*.ppm; do
#	magick.exe "$ppm" "${ppm%.ppm}.png"
#done

