#!/bin/bash

# This only works in WSL

set -e

./build.sh

# TODO: where are Ubuntu fonts?  /usr/local/share/fonts?
d=/mnt/c/Windows/Fonts/

#for f in $d/a*.ttf ; do
for f in $d/*.ttf ; do
	if [[ "$f" != "$d/corbeli.ttf" ]]; then
		./build/cali "$f"
	fi
done

