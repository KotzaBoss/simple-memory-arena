cmake_minimum_required(VERSION 3.25)

macro (BUILD_TYPE_WARNING WARNING)
  message (CHECK_FAIL "Skipping...")
  message (WARNING ${WARNING})
  set (CCACHE "SKIPPED")
endmacro()

###

message (CHECK_START "Enabling ccache")

if (NOT CMAKE_BUILD_TYPE)
  BUILD_TYPE_WARNING("CMAKE_BUILD_TYPE not set, expected 'Debug' to enable ccache")
  return()
elseif(NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
  BUILD_TYPE_WARNING("Build type is '${CMAKE_BUILD_TYPE}', should be 'Debug' to enable ccache")
  return()
endif()

find_program (CCACHE ccache)

if (${CCACHE} MATCHES ".*-NOTFOUND")
  message (CHECK_FAIL "Not found")
  return()
else()
  message (CHECK_PASS ${CCACHE})
endif()

set (CMAKE_CXX_COMPILER_LAUNCHER ${CCACHE})
set (CMAKE_C_COMPILER_LAUNCHER ${CCACHE})
message (STATUS "C/CXX compiler launchers set to ${CCACHE}")

