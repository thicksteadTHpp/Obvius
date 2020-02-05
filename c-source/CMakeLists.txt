cmake_minimum_required(VERSION 3.1.0)


project(obvlib VERSION 3.2
	       DESCRIPTION "The Obvius low-level c-code library"
	       LANGUAGES C)



# The obvius source files
SET(OBV_H obv.h convolve.h)
SET(CONVOLVE_SRC hex-convolve.c p-convolve.c convolve.c wrap.c edges.c )
SET(FORTRAN_H f2c.h)
SET(FORTRAN_SRC svd.c stepit.c)
SET(GL_SRC gl-obv-blt.c )
SET(TIFF_SRC tiff-access.c)
SET(ALL_SRC ${CONVOLVE_SRC} ${GL_SRC}  matrix.c byteswap.c color.c dither.c  fft.c imops.c 3d-ops.c surface-plot.c zoom.c warper.c chartohex.c)



# C standard to use for compiling the obvius lib
SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} $ENV{CFLAGS} -std=c89 -U__STRICT_ANSI__")

add_library(lib SHARED ${ALL_SRC} ${OBV_H})


message("Try to find system libtiff")
find_library(TIFF_LIBRARY tiff)
if(TIFF_LIBRARY)
       target_link_libraries(lib PUBLIC ${TIFF_LIBRARY})
       SET(ALL_SRC $ALL_SRC} ${TIFF_SRC})
       message("There is a TIFF library in the system")
       else()
       message("Compiling without TIFF support")
endif()


#env vars if not under travis
if (NOT DEFINED ENV{NAME})
   set(ENV{NAME} "obvius")
endif()

if (NOT DEFINED ENV{PREFIX})
   set(ENV{PREFIX} ${CMAKE_SHARED_LIBRARY_PREFIX})
endif()

if (NOT DEFINED ENV{EXTENSION})
   set(ENV{EXTENSION} ${CMAKE_SHARED_LIBRARY_SUFFIX})
endif()

if (NOT DEFINED ENV{CFLAGS})
   set(ENV{CFLAGS} -m64)
endif()


# the name of the library and the output dir

set_target_properties(lib PROPERTIES
   OUTPUT_NAME $ENV{NAME}
   PREFIX $ENV{PREFIX}
   SUFFIX $ENV{EXTENSION}
   LIBRARY_OUTPUT_DIRECTORY ${obvlib_SOURCE_DIR}/libs/lib
   VERSION 3.2
   POSITION_INDEPENDENT_CODE ON	
   )


if (WIN32)
   set_target_properties(lib PROPERTIES
     PREFIX $ENV{PREFIX}
     SUFFIX $ENV{EXTENSION})
endif()


find_library(MATH_LIBRARY m)

if(MATH_LIBRARY)
    target_link_libraries(lib PUBLIC ${MATH_LIBRARY})
endif()













