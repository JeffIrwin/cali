#!/bin/bash

# This only works in WSL

set -e

./build.sh

d=/mnt/c/Windows/Fonts/
#d=/usr/share/fonts/truetype/ubuntu/

nfiles=0
for f in $d/*.ttf ; do

	if [[ "$f" != "$d/corbeli.ttf" ]]; then
		./build/cali "$f"
	fi

	((nfiles=nfiles+1))

done

echo "Parsed $nfiles files"

