#!/bin/bash

set -xe

./build.sh

#time ./build/cali $*
#time ./build/cali /mnt/c/Windows/Fonts/calibri.ttf
#time ./build/cali ./fonts/computer-modern/cmunrm.ttf
time ./build/cali ./fonts/noto-sans/NotoSans-Regular.ttf

