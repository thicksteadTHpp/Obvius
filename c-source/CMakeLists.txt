cmake_minimum_required(VERSION 3.1.0)

project(obvlib VERSION 0.1
	       DESCRIPTION "The Obvius low-level c-code library"
	       LANGUAGES C)

SET(OBV_H obv.h convolve.h)
SET(CONVOLVE_SRC hex-convolve.c p-convolve.c convolve.c wrap.c edges.c )
SET(FORTRAN_H f2c.h)
SET(FORTRAN_SRC svd.c stepit.c)
SET(GL_SRC gl-obv-blt.c )
SET(TIFF_SRC tiff-access.c)
SET(ALL_SRC ${CONVOLVE_SRC} ${TIFF_SRC} ${GL_SRC}  matrix.c byteswap.c color.c dither.c  fft.c 	imops.c 3d-ops.c surface-plot.c zoom.c warper.c chartohex.c)



add_library(lib SHARED ${ALL_SRC} ${OBV_H})


find_library(MATH_LIBRARY m)

if(MATH_LIBRARY)
    target_link_libraries(lib PUBLIC ${MATH_LIBRARY})
endif()


#find_library(TIFF_LIBRARY tiff)
#if(TIFF_LIBRARY)
#    target_link_libraries(lib PUBLIC ${TIFF_LIBRARY})
#endif()

include(ExternalProject)

ExternalProject_Add(tifflib
  PREFIX ${CMAKE_BINARY_DIR}/tiff-lib
  INSTALL_DIR ${CMAKE_BINARY_DIR}/lib_ready
  URL http://download.osgeo.org/libtiff/tiff-4.1.0.zip
  URL_MD5 5d0347a6ac49ca20be01d6494c4df9cb
  CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_BINARY_DIR}/lib_ready)
#  INSTALL_COMMAND "")

#ExternalProject_Add_Step(tifflib copy
# DEPENDEES install
# COMMAND "cp ${CMAKE_BINARY_DIR}/tiff-lib/src/tifflib-build/libtiff/*.* ${CMAKE_BINARY_DIR}/lib_ready/")


add_dependencies(lib tifflib)


SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c89 -U__STRICT_ANSI__")


set_target_properties(lib PROPERTIES
  OUTPUT_NAME obvlib
  VERSION 0.1
  POSITION_INDEPENDENT_CODE ON)

if (WIN32)
   set_target_properties(lib PROPERTIES
     PREFIX lib
     SUFFIX .dll)
endif()
