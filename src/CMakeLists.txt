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

# Generate the physical-parser
BISON_TARGET(PhysicalParser ${PROJECT_SOURCE_DIR}/src/physical/physical-parser.y
  ${PROJECT_BINARY_DIR}/physical-parser.c)
FLEX_TARGET(PhysicalLexer ${PROJECT_SOURCE_DIR}/src/physical/physical-lexer.l
  ${PROJECT_BINARY_DIR}/physical-lexer.c)

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
include_directories("${PROJECT_SOURCE_DIR}/src/optimization_transforms")
include_directories("${PROJECT_SOURCE_DIR}/src/physical")
include_directories("${PROJECT_SOURCE_DIR}/src/m_templates")
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
  copy_sql_statement.c
  copy_sql_table.c
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
  qualify_column_list_alias.c
  qualify_column_name.c
  qualify_statement.c
  qualify_join_conditions.c
  octo_init.c
  get_keyword.c
  get_keyword_from_keywords.c
  get_table_parts.c
  get_advance.c
  emit_insert_statement.c
  emit_xref_table.c
  generate_cursor.c
  generate_null_check.c
  generate_key_name.c
  get_key_columns.c
  optimize_where.c
  assign_table_to_columns.c
  columns_equal.c
  tables_equal.c
  values_equal.c
  column_list_alias_to_columns.c
  get_column_piece_number.c
  optimization_transforms/constant_equal_column.c
  optimization_transforms/optimize_logical_plan.c
  optimization_transforms/lp_get_select.c
  optimization_transforms/lp_get_select_where.c
  optimization_transforms/lp_get_select_keywords.c
  optimization_transforms/lp_verify_structure.c
  optimization_transforms/lp_emit_plan.c
  optimization_transforms/lp_get_keys.c
  optimization_transforms/lp_get_projection_columns.c
  optimization_transforms/get_plan_unique_number.c
  optimization_transforms/lp_insert_key.c
  optimization_transforms/lp_copy_plan.c
  optimization_transforms/lp_join_where.c
  optimization_transforms/lp_generate_where.c
  optimization_transforms/lp_column_list_to_lp.c
  optimization_transforms/lp_columns_to_column_list.c
  optimization_transforms/lp_table_join_to_column_list.c
  optimization_transforms/lp_replace_derived_table_references.c
  optimization_transforms/lp_get_output_key.c
  optimization_transforms/lp_get_project.c
  optimization_transforms/lp_opt_fix_key_to_const.c
  optimization_transforms/lp_get_key.c
  optimization_transforms/lp_get_key_index.c
  m_templates/emit_simple_select.c
  optimization_transforms/generate_logical_plan.c
  physical/generate_physical_plan.c
  physical/emit_physical_plan.c
  m_templates/print_dots.c
  m_escape_string2.c
  ${BISON_OctoParser_OUTPUTS}
  ${FLEX_OctoLexer_OUTPUTS}
)

add_executable(octo
  octo.c
  $<TARGET_OBJECTS:libocto>
)

add_executable(pparser
  physical/pparser.c
  errors.c
  octo_init.c
  ${BISON_PhysicalParser_OUTPUTS}
  ${FLEX_PhysicalLexer_OUTPUTS}
)

# Generate C files from templates
set(template_dir ${PROJECT_SOURCE_DIR}/src/m_templates)
set(template_files
  ${template_dir}/simpleFor.ctemplate
  ${template_dir}/tmpl_physical_plan.ctemplate
  ${template_dir}/tmpl_key_start.ctemplate
  ${template_dir}/tmpl_key_end.ctemplate
  ${template_dir}/tmpl_key.ctemplate
  ${template_dir}/tmpl_key_advance.ctemplate
  ${template_dir}/tmpl_temp_key_advance.ctemplate
  ${template_dir}/tmpl_print_expression.ctemplate
  ${template_dir}/tmpl_column_reference.ctemplate
  ${template_dir}/tmpl_column_list_combine.ctemplate
  ${template_dir}/tmpl_emit_source.ctemplate
  ${template_dir}/tmpl_table_set.ctemplate
  )
foreach(template ${template_files})
  get_filename_component(fileName ${template} NAME)
  add_custom_command(OUTPUT ${fileName}.c
    DEPENDS "${template}" pparser
    COMMAND ${PROJECT_BINARY_DIR}/src/pparser < ${template} > ${fileName}.c
    )
  target_sources(libocto PUBLIC ${fileName}.c)
endforeach()


# Place aux files

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
target_link_libraries(pparser ${YOTTADB_LIBRARIES} ${Readline_LIBRARY})

enable_testing()
include(${PROJECT_SOURCE_DIR}/cmake/tests.cmake)

if(CMOCKA_FOUND)
  # include(${PROJECT_SOURCE_DIR}/cmake/cmocka-tests.cmake)
endif()
if(BATS_FOUND)
  include(${PROJECT_SOURCE_DIR}/cmake/bats-tests.cmake)
endif()
