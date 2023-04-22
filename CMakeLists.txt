
#=======================================================================

cmake_minimum_required(VERSION 3.24)

#set(CMAKE_Fortran_COMPILER "gfortran")

set(PROJECT "cali")
project(${PROJECT} Fortran)

#=======================================================================

add_compile_options("-Wall")
add_compile_options("-Wextra")
add_compile_options("-fopenmp")
add_compile_options("-Wno-tabs")
add_compile_options("-Wno-maybe-uninitialized")

# TODO: debug-only.  For release use -O3
add_compile_options("-pedantic")
add_compile_options("-fbounds-check")
add_compile_options("-fbacktrace")

#=======================================================================

set(SRC_DIR "src")

#********

set(LIB "${PROJECT}")
set(LIB_SRC
	${SRC_DIR}/cali.f90
	)
add_library(${LIB} STATIC ${LIB_SRC})
set(LINK_LIBS "${LIB}")

#********

set(EXE "${PROJECT}")
set(EXE_BIN "${EXE}-bin")
set(EXE_SRC
	${SRC_DIR}/main.f90
	)

#add_executable(${PROJECT} src/cali.f90 src/main.f90)
add_executable("${EXE_BIN}" ${EXE_SRC})
target_link_libraries(${EXE_BIN} ${LINK_LIBS})
set_target_properties("${EXE_BIN}" PROPERTIES OUTPUT_NAME "${EXE}")

#=======================================================================

