#!/bin/bash

#gfortran -o build/main src/main.f90

cmake -S . -B build
cmake --build build

