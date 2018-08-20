cmake_minimum_required (VERSION 3.4.3)
project(OctoSQL)
set(OCTO_SQL_MAJOR_VERSION 0)
set(OCTO_SQL_MINOR_VERSION 1)
set(CMAKE_BUILD_TYPE Debug)

find_package(BISON)
find_package(FLEX)

configure_file (
  "${PROJECT_SOURCE_DIR}/octo.h.in"
  "${PROJECT_BINARY_DIR}/octo.h"
)

# Combine the parser grammars into a single file
add_custom_command(OUTPUT ${PROJECT_BINARY_DIR}/parser.y
  DEPENDS parser.y
          parser/select.y
          parser/insert.y
          parser/update.y
  COMMAND awk -v path=${PROJECT_SOURCE_DIR}/
    -f ${PROJECT_SOURCE_DIR}/../tools/combine-grammar.awk
    ${PROJECT_SOURCE_DIR}/parser.y > ${PROJECT_BINARY_DIR}/parser.y
)

BISON_TARGET(OctoParser ${PROJECT_BINARY_DIR}/parser.y
  ${PROJECT_BINARY_DIR}/parser.c)
FLEX_TARGET(OctoLexer lexer.l ${CMAKE_CURRENT_BINARY_DIR}/lexer.c
  COMPILE_FLAGS --header-file=${PROJECT_BINARY_DIR}/lexer.h)

include_directories("${PROJECT_BINARY_DIR}")

add_executable(octo
  octo.c
  ${BISON_OctoParser_OUTPUTS}
  ${FLEX_OctoLexer_OUTPUTS}
)

include(cmake/tests.cmake)
