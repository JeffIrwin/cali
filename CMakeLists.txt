
cmake_minimum_required(VERSION 3.24)

#set(CMAKE_Fortran_COMPILER "gfortran")

set(PROJECT "cali")

project(${PROJECT} Fortran)

add_executable(${PROJECT} src/main.f90)

