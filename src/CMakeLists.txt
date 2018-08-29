find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
find_package(YOTTADB REQUIRED)
find_package(CMOCKA)

configure_file (
  "${PROJECT_SOURCE_DIR}/src/octo.h.in"
  "${PROJECT_BINARY_DIR}/octo.h"
)

configure_file (
  "${PROJECT_SOURCE_DIR}/tools/activate.in"
  "${PROJECT_BINARY_DIR}/activate"
)

configure_file (
  "${PROJECT_SOURCE_DIR}/tools/calltab.ci"
  "${PROJECT_BINARY_DIR}/calltab.ci"
)

configure_file (
  "${PROJECT_SOURCE_DIR}/tools/select.m"
  "${PROJECT_BINARY_DIR}/select.m"
)

# Combine the parser grammars into a single file
add_custom_command(OUTPUT ${PROJECT_BINARY_DIR}/parser.y
  DEPENDS parser.y
          parser/select.y
          parser/insert.y
          parser/update.y
          parser/drop.y
  COMMAND awk -v path=${PROJECT_SOURCE_DIR}/src/
    -f ${PROJECT_SOURCE_DIR}/tools/combine-grammar.awk
    ${PROJECT_SOURCE_DIR}/src/parser.y > ${PROJECT_BINARY_DIR}/parser.y
)

BISON_TARGET(OctoParser ${PROJECT_BINARY_DIR}/parser.y
  ${PROJECT_BINARY_DIR}/parser.c)
FLEX_TARGET(OctoLexer lexer.l ${CMAKE_CURRENT_BINARY_DIR}/lexer.c
  COMPILE_FLAGS --header-file=${PROJECT_BINARY_DIR}/lexer.h)

include_directories("${PROJECT_SOURCE_DIR}/src")
include_directories("${PROJECT_BINARY_DIR}")
include_directories("${YOTTADB_INCLUDE_DIRS}")
link_directories("${YOTTADB_INCLUDE_DIRS}")

add_library(libocto OBJECT
  yyerror.c
  yywrap.c
  emit_create_table.c
  emit_select_statement.c
  cleanup_sql_statement.c
  fetch_primary_key_column.c
  quoting.c
  create_table_defaults.c
  m_templates/emit_simple_select.c
  ${BISON_OctoParser_OUTPUTS}
  ${FLEX_OctoLexer_OUTPUTS}
)

add_executable(octo
  octo.c
  $<TARGET_OBJECTS:libocto>
)

target_link_libraries(octo ${YOTTADB_LIBRARIES})

enable_testing()
include(${PROJECT_SOURCE_DIR}/cmake/tests.cmake)

if(CMOCKA_FOUND)
  include(${PROJECT_SOURCE_DIR}/cmake/cmocka-tests.cmake)
endif()
