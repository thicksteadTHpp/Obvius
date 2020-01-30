cmake_minimum_required(VERSION 3.1.0)

project(obvlib VERSION 0.1
	       DESCRIPTION "The Obvius low-level c-code library"
	       LANGUAGES C)

g
SET(CONVOLVE_SRC hex-convolve.c p-convolve.c convolve.c wrap.c edges.c )
SET(FORTRAN_SRC svd.c stepit.c)
SET(GL_SRC gl-obv-blt.c )
SET(TIFF_SRC tiff-access.c)
SET(ALL_SRC ${CONVOLVE_SRC} ${TIFF_SRC} ${GL_SRC}  matrix.c byteswap.c color.c dither.c  fft.c 	imops.c 3d-ops.c surface-plot.c zoom.c warper.c chartohex.c)


add_library(lib SHARED ${ALL_SRC})

set_target_properties(lib PROPERTIES
  OUTPUT_NAME obvlib
  VERSION 0.1
  SOVERSION tho
  POSITION_INDEPENDENT_CODE ON)

if (WIN32)
   set_target_properties(lib PROPERTIES
     PREFIX lib
     SUFFIX .dll)
endif()
