#!/bin/bash

set -xe

./build.sh

#./build/cali $*
./build/cali ./fonts/computer-modern/cmunrm.ttf
#./build/cali /mnt/c/Windows/Fonts/calibri.ttf

