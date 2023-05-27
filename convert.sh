#!/bin/bash

set -xe

for ppm in build/*.ppm; do
#for ppm in build/diff*.ppm; do
	magick.exe "$ppm" "${ppm%.ppm}.png"
done

