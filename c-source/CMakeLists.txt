cmake_minimum_required(VERSION 3.1.0)

set(OBV_VERSION 3.2)

project(obvlib VERSION ${OBV_VERSION}
	       DESCRIPTION "The Obvius low-level c-code library"
	       LANGUAGES C)

# The obvius source files
SET(OBV_H obv.h convolve.h)
SET(CONVOLVE_SRC hex-convolve.c p-convolve.c convolve.c wrap.c edges.c )
SET(FORTRAN_H f2c.h)
SET(FORTRAN_SRC svd.c stepit.c)
SET(GL_SRC gl-obv-blt.c )
SET(TIFF_SRC tiff-access.c)

# on windows ad  mpw hacks for srand48 and drand48
if (WIN32)
SET(ALL_SRC ${CONVOLVE_SRC} ${GL_SRC}  mpw-hacks.c matrix.c byteswap.c color.c dither.c  fft.c imops.c 3d-ops.c surface-plot.c zoom.c warper.c chartohex.c)
else()
SET(ALL_SRC ${CONVOLVE_SRC} ${GL_SRC}  matrix.c byteswap.c color.c dither.c  fft.c imops.c 3d-ops.c surface-plot.c zoom.c warper.c chartohex.c)
endif()


if (DEFINED ENV{CFLAGS})
   set(OBV_C_FLAGS $ENV{CFLAGS})
endif()

# C standard to use for compiling the obvius lib
SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OBV_C_FLAGS} -std=c89 -U__STRICT_ANSI__")

# link to tiff library is ON for APPLE 64 bit LINUX 32Bit and 64Bit 
set(LINK_TO_TIFF ON)

# do not link to tiff on APPLE 32bit
if (DEFINED ENV{CFLAGS})
   if (APPLE AND $ENV{CFLAGS} STREQUAL "-m32")
    set(LINK_TO_TIFF OFF)
    message("MACOSX does not like 32bit linking anymore")
   endif()
endif()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${obvlib_SOURCE_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${obvlib_SOURCE_DIR}/lib)


message("Try to find system libtiff")
find_library(TIFF_LIBRARY tiff)
if(TIFF_LIBRARY AND LINK_TO_TIFF)
       add_library(lib SHARED ${ALL_SRC} ${TIFF_SRC} ${OBV_H})
       target_link_libraries(lib PUBLIC ${TIFF_LIBRARY})
       message("There is a TIFF library in the system")
 else()
       message("Compiling without TIFF support")
       add_library(lib SHARED ${ALL_SRC} ${OBV_H})	
endif()


#env vars if not under travis
#must be after defining the library

if (DEFINED ENV{TRAVIS})
   set(OBV_LIB_NAME $ENV{NAME})
   set(OBV_LIB_PREFIX $ENV{PREFIX})
   set(OBV_LIB_SUFFIX $ENV{EXTENSION})
else()
   set(OBV_LIB_NAME "obvius")
   set(OBV_LIB_PREFIX "lib")   #${CMAKE_SHARED_LIBRARY_PREFIX})
   set(OBV_LIB_SUFFIX ${CMAKE_SHARED_LIBRARY_SUFFIX})
endif()

message("obv-lib name: ${OBV_LIB_PREFIX}${OBV_LIB_NAME}${OBV_LIB_SUFFIX}")

# the name of the library and the output dir

set_target_properties(lib PROPERTIES
   OUTPUT_NAME ${OBV_LIB_NAME}
   PREFIX ${OBV_LIB_PREFIX}
   SUFFIX ${OBV_LIB_SUFFIX}
#   LIBRARY_OUTPUT_DIRECTORY ${obvlib_SOURCE_DIR}
   VERSION ${OBV_VERSION}
   POSITION_INDEPENDENT_CODE ON
   NO_SONAME ON    # no version string 
   )

get_target_property(OUT_DIR lib LIBRARY_OUTPUT_DIRECTORY)
message("output dir: ${OUT_DIR}")

#if (WIN32)
#   set_target_properties(lib PROPERTIES
#     OUTPUT_NAME ${OBV_LIB_NAME}
#     PREFIX ${OBV_LIB_PREFIX}
#     SUFFIX ${OBV_LIBSUFFIX})
#endif()


# msys and mingw automatically link math lib 
if (NOT WIN32)
find_library(MATH_LIBRARY m)

if(MATH_LIBRARY)
    target_link_libraries(lib PUBLIC ${MATH_LIBRARY})
    message("found math library")
endif()

# don't install under Win32 because admin priviliges are needed

install(TARGETS lib
        LIBRARY DESTINATION ${obvlib_SOURCE_DIR}/../bin/
	NAMELINK_SKIP
	RENAME ${OBV_LIB_PREFIX}${OBV_LIB_NAME}${OBV_LIB_SUFFIX})
endif()









