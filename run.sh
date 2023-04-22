#!/bin/bash

set -xe

./build.sh
./build/cali $*

