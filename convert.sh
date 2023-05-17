#!/bin/bash

set -xe

for ppm in build/*.ppm; do
	magick.exe "$ppm" "${ppm%.ppm}.png"
done

