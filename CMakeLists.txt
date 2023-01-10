cmake_minimum_required(VERSION 3.25)

project (simple-memory-arena)

message ("=============================================================")
message ("= ${PROJECT_NAME}")
message ("=============================================================")

if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Debug)
endif()

set(CMAKE_MODULE_PATH cmake)

include(UseCcache)

# ADA ##############################################

find_program(GPRBUILD gprbuild REQUIRED)

add_custom_target(ADA
  ALL
  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
  COMMAND ${GPRBUILD} -P arena.gpr
  )

# CPP ##############################################

find_library (BENCHMARK benchmark REQUIRED)

function (BUILD_EXECUTABLE TARGET MAX_OBJS_PREPROC)
  set(EXECUTABLE ${TARGET}.${MAX_OBJS_PREPROC}.out)

  add_executable (${EXECUTABLE}
    cpp/${TARGET}.cpp
    )
  
  set_target_properties (${EXECUTABLE}
    PROPERTIES
      CXX_STANDARD              20
      CXX_STANDARD_REQUIRED     TRUE
      CXX_EXTENSIONS            TRUE
      RUNTIME_OUTPUT_DIRECTORY  cpp
    )
  
  target_compile_options (${EXECUTABLE}
    PRIVATE
      -Wall
      -Wextra
      -Wpedantic
      -O2
      -g
    )

  target_compile_definitions (${EXECUTABLE}
    PRIVATE
      MAX_OBJS_PREPROC=${MAX_OBJS_PREPROC}
    )

  if (${TARGET} STREQUAL "bench")
    target_link_libraries (${EXECUTABLE}
      PRIVATE
        ${BENCHMARK}
      )
  endif()

endfunction()

foreach (TARGET "bench" "main")
  foreach (ORDER_MAGNITUDE RANGE 5)
    BUILD_EXECUTABLE (${TARGET} 1e${ORDER_MAGNITUDE})
  endforeach()
endforeach()

# RUN #################################################

add_custom_target(RUN_ADA
  COMMAND
    for OBJS in {1..10000}\; do ./build/ada/main $OBJS\; done\;
  USES_SHELL
  )

