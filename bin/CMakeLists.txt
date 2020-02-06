cmake_minimum_required(VERSION 3.0.0)

project(obvlib)

add_subdirectory(libs)

set_target_properties(obvlib PROPERTIES
  OUTPUT_NAME obvlib
  VERSION 0.1
  SOVERSION tho
  POSITION_INDEPENDENT_CODE ON)

if (WIN32)
   set_target_properties(obvlib PROPERTIES
     PREFIX lib
     SUFFIX .dll)
endif()
