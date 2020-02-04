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

SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} $ENV{CFLAGS} -std=c89 -U__STRICT_ANSI__")

add_library(lib SHARED ${ALL_SRC} ${OBV_H})

set_target_properties(lib PROPERTIES
   OUTPUT_NAME obv
   LIBRARY_OUTPUT_DIRECTORY ${obvlib_SOURCE_DIR}/libs/lib
   )

if ( $ENV{CFLAGS})
   if ( $ENV{CFLAGS} STREQUAL "-m32" )
      set_target_properties(lib PROPERTIES
      OUTPUT_NAME obv32)
   endif()
endif()


set_target_properties(lib PROPERTIES  
  VERSION 0.1
  POSITION_INDEPENDENT_CODE ON)

if (WIN32)
   set_target_properties(lib PROPERTIES
     PREFIX lib
     SUFFIX .dll)
endif()

if ( $ENV{CFLAGS})
   if ( $ENV{CFLAGS} STREQUAL "-m32" )
      set_target_properties(lib PROPERTIES
      OUTPUT_NAME obv32)
   endif()
endif()



include(ExternalProject)







set(ENV_FLAGS "$ENV{CFLAGS}")
message("Building OSX?: ${APPLE} $ENV{CFLAGS}  ")
if ( $ENV{CFLAGS} STREQUAL "-m32")
  message("STREQUAL works")
endif()


if (DEFINED ENV{CFLAGS})
   if (APPLE AND $ENV{CFLAGS} STREQUAL "-m32")
        message("32bit compile on APPLE ") 
	ExternalProject_Add(tiffsource
	PREFIX ${CMAKE_BINARY_DIR}/tiff-source
  	INSTALL_DIR ${CMAKE_BINARY_DIR}/lib_ready
  	URL http://download.osgeo.org/libtiff/tiff-4.1.0.zip
  	URL_MD5 5d0347a6ac49ca20be01d6494c4df9cb
  	CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=${obvlib_SOURCE_DIR}/libs
        	   -DCMAKE_C_FLAGS=$ENV{CFLAGS}
	           -DCMAKE_CXX_FLAGS=$ENV{CFLAGS}
	           -Dwebp:BOOL=OFF
		   -Djpeg:BOOL=OFF
   )
   else()
   ExternalProject_Add(tiffsource
	PREFIX ${CMAKE_BINARY_DIR}/tiff-source
  	INSTALL_DIR ${CMAKE_BINARY_DIR}/lib_ready
  	URL http://download.osgeo.org/libtiff/tiff-4.1.0.zip
  	URL_MD5 5d0347a6ac49ca20be01d6494c4df9cb
  	CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=${obvlib_SOURCE_DIR}/libs
        	   -DCMAKE_C_FLAGS=$ENV{CFLAGS}
	           -DCMAKE_CXX_FLAGS=$ENV{CFLAGS}
	     	    -Dwebp:BOOL=OFF
   )
   endif()
endif()


find_library(MATH_LIBRARY m)

if(MATH_LIBRARY)
    target_link_libraries(lib PUBLIC ${MATH_LIBRARY})
endif()



add_dependencies(lib tiffsource)


add_library(tifflib SHARED IMPORTED)
set_target_properties(tifflib PROPERTIES
   IMPORTED_LOCATION ${obvlib_SOURCE_DIR}/libs/lib/${CMAKE_SHARED_LIBRARY_PREFIX}tiff${CMAKE_SHARED_LIBRARY_SUFFIX})

target_link_libraries(lib PUBLIC tifflib)

add_dependencies(lib tifflib)


#if(TIFF_LIBRARY)
#    target_link_libraries(lib PUBLIC ${TIFF_LIBRARY})
#endif()









