
#=======================================================================

cmake_minimum_required(VERSION 3.16)
#cmake_minimum_required(VERSION 3.24)

#set(CMAKE_Fortran_COMPILER "gfortran")

set(PROJECT "cali")
project(${PROJECT} Fortran CXX)

#=======================================================================

# TODO: release option

add_compile_options("-Wall")
add_compile_options("-Wextra")
add_compile_options("-Werror")

add_compile_options($<$<COMPILE_LANGUAGE:Fortran>:-Wno-tabs>)
add_compile_options($<$<COMPILE_LANGUAGE:Fortran>:-Wno-maybe-uninitialized>)

#add_compile_options("-fopenmp")
add_compile_options($<$<COMPILE_LANGUAGE:Fortran>:-fopenmp>)

#add_compile_options("-std=gnu")
#add_compile_options("-pedantic") # pedantic warns about convert='big_endian'

## TODO: debug-only.  For release use -O3
add_compile_options($<$<COMPILE_LANGUAGE:Fortran>:-fbacktrace>)
add_compile_options($<$<COMPILE_LANGUAGE:Fortran>:-fbounds-check>)

#add_compile_options("-O3")

#=======================================================================

set(SRC_DIR "src")
set(SUBMODULE_DIR "submodules")
set(PNG_DIR "${SUBMODULE_DIR}/lodepng")

#********

set(LIB "${PROJECT}")
set(LIB_SRC
	${SRC_DIR}/cali.f90
	${SRC_DIR}/utf.f90
	)

# TODO: add optional guard to compile w/ or w/o lodepng
list(APPEND LIB_SRC
	${PNG_DIR}/lodepng.cpp
	${SRC_DIR}/lodepng_wrapper.cpp
	)

#message("LIB_SRC = ${LIB_SRC}")

add_library(${LIB} STATIC ${LIB_SRC})
set(LINK_LIBS "${LIB}")

target_include_directories(${LIB} PUBLIC
	${PNG_DIR}
	)

#********

set(EXE "${PROJECT}")
set(EXE_BIN "${EXE}-bin")
set(EXE_SRC
	${SRC_DIR}/main.f90
	)

add_executable("${EXE_BIN}" ${EXE_SRC})
target_link_libraries(${EXE_BIN} ${LINK_LIBS})
set_target_properties("${EXE_BIN}" PROPERTIES OUTPUT_NAME "${EXE}")

#********

set(TEST_EXE "test")
set(TEST_SRC
	${SRC_DIR}/test.f90
	)

add_executable("${TEST_EXE}" ${TEST_SRC})
target_link_libraries(${TEST_EXE} ${LINK_LIBS})

#********

set(DEMO_EXE "demo")
set(DEMO_SRC
	${SRC_DIR}/demo.f90
	)

add_executable("${DEMO_EXE}" ${DEMO_SRC})
target_link_libraries(${DEMO_EXE} ${LINK_LIBS})

#=======================================================================

