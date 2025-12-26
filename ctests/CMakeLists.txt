cmake_minimum_required(VERSION 3.20)

project(mquickjs_ctests LANGUAGES C CXX)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)

FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG v3.9.1
)
FetchContent_MakeAvailable(Catch2)

set(MQUICKJS_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../mquickjs-c)
set(GENERATED_DIR ${CMAKE_CURRENT_BINARY_DIR}/generated)
set(GEN_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/gen_stdlib.cmake)

file(MAKE_DIRECTORY ${GENERATED_DIR})

add_library(cutils STATIC
  ${MQUICKJS_SOURCE_DIR}/cutils.c
)
target_include_directories(cutils PUBLIC
  ${MQUICKJS_SOURCE_DIR}
)

add_executable(mqjs_stdlib_host
  ${MQUICKJS_SOURCE_DIR}/mqjs_stdlib.c
  ${MQUICKJS_SOURCE_DIR}/mquickjs_build.c
)
target_include_directories(mqjs_stdlib_host PRIVATE
  ${MQUICKJS_SOURCE_DIR}
)
target_link_libraries(mqjs_stdlib_host PRIVATE
  cutils
  m
)

add_custom_command(
  OUTPUT ${GENERATED_DIR}/mqjs_stdlib.h
  COMMAND ${CMAKE_COMMAND}
    -DMQJS_STDLIB_TOOL=$<TARGET_FILE:mqjs_stdlib_host>
    -DOUTPUT=${GENERATED_DIR}/mqjs_stdlib.h
    -P ${GEN_SCRIPT}
  DEPENDS mqjs_stdlib_host
  VERBATIM
)

add_custom_command(
  OUTPUT ${GENERATED_DIR}/mquickjs_atom.h
  COMMAND ${CMAKE_COMMAND}
    -DMQJS_STDLIB_TOOL=$<TARGET_FILE:mqjs_stdlib_host>
    -DOUTPUT=${GENERATED_DIR}/mquickjs_atom.h
    -DARGS=-a
    -P ${GEN_SCRIPT}
  DEPENDS mqjs_stdlib_host
  VERBATIM
)

add_custom_target(mqjs_stdlib_headers
  DEPENDS
    ${GENERATED_DIR}/mqjs_stdlib.h
    ${GENERATED_DIR}/mquickjs_atom.h
)

add_library(mqjs_stdlib_data STATIC
  mqjs_stdlib_wrapper.c
  mqjs_stdlib_stubs.c
)
target_include_directories(mqjs_stdlib_data PRIVATE
  ${MQUICKJS_SOURCE_DIR}
  ${GENERATED_DIR}
)
add_dependencies(mqjs_stdlib_data mqjs_stdlib_headers)

add_library(mquickjs_core STATIC
  ${MQUICKJS_SOURCE_DIR}/mquickjs.c
  ${MQUICKJS_SOURCE_DIR}/dtoa.c
  ${MQUICKJS_SOURCE_DIR}/libm.c
)
target_include_directories(mquickjs_core PRIVATE
  ${MQUICKJS_SOURCE_DIR}
  ${GENERATED_DIR}
)
target_link_libraries(mquickjs_core PRIVATE
  cutils
  m
)
add_dependencies(mquickjs_core mqjs_stdlib_headers)

add_library(dtoa_udiv1norm_helpers STATIC
  dtoa_udiv1norm_helpers.c
)
target_include_directories(dtoa_udiv1norm_helpers PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${MQUICKJS_SOURCE_DIR}
)
target_link_libraries(dtoa_udiv1norm_helpers PUBLIC
  cutils
)
if (CMAKE_C_COMPILER_ID MATCHES "Clang|GNU")
  target_compile_options(dtoa_udiv1norm_helpers PRIVATE
    -Wno-sign-compare
    -Wno-unused-parameter
  )
endif()

add_executable(dtoa_udiv1norm_catch2_test
  dtoa_udiv1norm_catch2_test.cpp
)
target_include_directories(dtoa_udiv1norm_catch2_test PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
)
target_link_libraries(dtoa_udiv1norm_catch2_test PRIVATE
  dtoa_udiv1norm_helpers
  Catch2::Catch2WithMain
  m
)

include(CTest)
include(Catch)
catch_discover_tests(dtoa_udiv1norm_catch2_test)

add_executable(cutils_unicode_from_utf8_catch2_test
  cutils_unicode_from_utf8_catch2_test.cpp
)
target_include_directories(cutils_unicode_from_utf8_catch2_test PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
)
target_link_libraries(cutils_unicode_from_utf8_catch2_test PRIVATE
  cutils
  Catch2::Catch2WithMain
)

catch_discover_tests(cutils_unicode_from_utf8_catch2_test)

add_executable(parser_bytecode_catch2_test
  parser_bytecode_catch2_test.cpp
)
target_include_directories(parser_bytecode_catch2_test PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${MQUICKJS_SOURCE_DIR}
)
target_link_libraries(parser_bytecode_catch2_test PRIVATE
  mquickjs_core
  mqjs_stdlib_data
  Catch2::Catch2WithMain
  m
)
add_dependencies(parser_bytecode_catch2_test mqjs_stdlib_headers)

catch_discover_tests(parser_bytecode_catch2_test)

add_executable(js_call_catch2_test
  js_call_catch2_test.cpp
)
target_include_directories(js_call_catch2_test PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${MQUICKJS_SOURCE_DIR}
)
target_link_libraries(js_call_catch2_test PRIVATE
  mquickjs_core
  mqjs_stdlib_data
  Catch2::Catch2WithMain
  m
)
add_dependencies(js_call_catch2_test mqjs_stdlib_headers)

catch_discover_tests(js_call_catch2_test)
