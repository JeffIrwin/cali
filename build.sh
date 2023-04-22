#!/bin/bash

# TODO: debug vs release arg

cmake -S . -B build -G "Unix Makefiles"
cmake --build build

