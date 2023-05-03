#!/bin/bash

set -e

./build.sh
time ./build/test

echo "done test.sh"

