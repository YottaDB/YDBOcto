find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
find_package(YOTTADB REQUIRED)
find_package(Readline REQUIRED)
find_package(CMOCKA)
find_package(BATS)

configure_file (
  "${PROJECT_SOURCE_DIR}/src/octo.h.in"
  "${PROJECT_BINARY_DIR}/octo.h"
)

configure_file (
  "${PROJECT_SOURCE_DIR}/tools/activate.in"
  "${PROJECT_BINARY_DIR}/activate"
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
include_directories("${Readline_INCLUDE_DIR}")
link_directories("${YOTTADB_INCLUDE_DIRS}")

add_library(libocto OBJECT
  yyerror.c
  yywrap.c
  errors.c
  extract_expression.c
  parse_line.c
  emit_create_table.c
  emit_select_statement.c
  emit_column_specification.c
  emit_temporary_join_table.c
  cleanup_sql_statement.c
  fetch_primary_key_column.c
  quoting.c
  create_table_defaults.c
  print_temporary_table.c
  readline_getc.c
  populate_data_type.c
  find_table.c
  find_column.c
  find_qualified_column.c
  generate_end.c
  qualify_column_list.c
  qualify_column_name.c
  qualify_statement.c
  octo_init.c
  get_keyword.c
  get_table_parts.c
  get_advance.c
  emit_insert_statement.c
  emit_xref_table.c
  generate_cursor.c
  generate_null_check.c
  generate_key_name.c
  get_key_columns.c
  optimize_where.c
  optimization_transforms/constant_equal_column.c
  m_templates/emit_simple_select.c
  ${BISON_OctoParser_OUTPUTS}
  ${FLEX_OctoLexer_OUTPUTS}
)

add_executable(octo
  octo.c
  $<TARGET_OBJECTS:libocto>
)

set(aux_files
  ${PROJECT_BINARY_DIR}/calltab.ci
  ${PROJECT_BINARY_DIR}/select.m
  ${PROJECT_BINARY_DIR}/insert.m
  ${PROJECT_BINARY_DIR}/pack.m
)
add_custom_target(place_aux_files ALL DEPENDS ${aux_files})
foreach (auxFile ${aux_files})
  get_filename_component(fileName ${auxFile} NAME)
  add_custom_command(OUTPUT ${auxFile}
    DEPENDS "${PROJECT_SOURCE_DIR}/src/aux/${fileName}"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/aux/${fileName}" "${auxFile}")
endforeach()

set_property(TARGET libocto PROPERTY C_STANDARD 11)

target_link_libraries(octo ${YOTTADB_LIBRARIES} ${Readline_LIBRARY})

enable_testing()
include(${PROJECT_SOURCE_DIR}/cmake/tests.cmake)

if(CMOCKA_FOUND)
  include(${PROJECT_SOURCE_DIR}/cmake/cmocka-tests.cmake)
endif()
if(BATS_FOUND)
  include(${PROJECT_SOURCE_DIR}/cmake/bats-tests.cmake)
endif()
