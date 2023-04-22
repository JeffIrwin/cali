
cmake_minimum_required(VERSION 3.24)

#set(CMAKE_Fortran_COMPILER "gfortran")

set(PROJECT "cali")

project(${PROJECT} Fortran)

add_compile_options("-Wall")
add_compile_options("-Wextra")
add_compile_options("-fopenmp")
add_compile_options("-Wno-tabs")
add_compile_options("-Wno-maybe-uninitialized")

# TODO: debug-only.  For release use -O3
add_compile_options("-pedantic")
add_compile_options("-fbounds-check")
add_compile_options("-fbacktrace")

add_executable(${PROJECT} src/cali.f90 src/main.f90)

