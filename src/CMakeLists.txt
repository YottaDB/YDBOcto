find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
find_package(YOTTADB REQUIRED)
find_package(Readline REQUIRED)
find_package(Config REQUIRED)
find_package(OpenSSL REQUIRED)
find_package(CMOCKA)
find_package(BATS)

configure_file (
  "${PROJECT_SOURCE_DIR}/src/constants.h.in"
  "${PROJECT_BINARY_DIR}/constants.h"
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
          parser/set.y
  COMMAND awk -v path=${PROJECT_SOURCE_DIR}/src/
    -f ${PROJECT_SOURCE_DIR}/tools/combine-grammar.awk
    ${PROJECT_SOURCE_DIR}/src/parser.y > ${PROJECT_BINARY_DIR}/parser.y
  COMMENT "Combining grammar files"
)

BISON_TARGET(OctoParser ${PROJECT_BINARY_DIR}/parser.y
  ${PROJECT_BINARY_DIR}/parser.c)
FLEX_TARGET(OctoLexer lexer.l ${CMAKE_CURRENT_BINARY_DIR}/lexer.c
  COMPILE_FLAGS --header-file=${PROJECT_BINARY_DIR}/lexer.h)

include_directories("${PROJECT_SOURCE_DIR}/src")
include_directories("${PROJECT_SOURCE_DIR}/src/optimization_transforms")
include_directories("${PROJECT_SOURCE_DIR}/src/physical")
include_directories("${PROJECT_SOURCE_DIR}/src/m_templates")
include_directories("${PROJECT_SOURCE_DIR}/src/helpers")
include_directories("${PROJECT_BINARY_DIR}")
include_directories("${YOTTADB_INCLUDE_DIRS}")
include_directories("${Readline_INCLUDE_DIR}")
include_directories("${OPENSSL_INCLUDE_DIR}")
include_directories("${CONFIG_INCLUDE_DIR}")
link_directories("${YOTTADB_INCLUDE_DIRS}")

add_library(libhelpers OBJECT
  helpers/set.c
  helpers/get.c
  helpers/make_buffers.c
)

# Load the default config from src/aux/octo.conf
ADD_CUSTOM_COMMAND(
  OUTPUT default_octo_conf.h
  DEPENDS "${PROJECT_SOURCE_DIR}/src/aux/octo.conf.default"
  COMMAND xxd -i "octo.conf.default" "${PROJECT_BINARY_DIR}/default_octo_conf.h"
  WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/src/aux/"
  COMMENT "Converting octo_conf.c to binary form for defaults"
)

add_library(libocto OBJECT
  default_octo_conf.h
  yyerror.c
  yywrap.c
  errors.c
  parse_line.c
  emit_create_table.c
  emit_select_statement.c
  emit_column_specification.c
  quoting.c
  create_table_defaults.c
  print_temporary_table.c
  readline_get_more.c
  populate_data_type.c
  find_table.c
  find_column.c
  qualify_column_list.c
  qualify_column_list_alias.c
  qualify_column_name.c
  qualify_statement.c
  qualify_join_conditions.c
  qualify_function_name.c
  octo_init.c
  get_keyword.c
  get_keyword_from_keywords.c
  generate_key_name.c
  get_key_columns.c
  assign_table_to_columns.c
  columns_equal.c
  tables_equal.c
  values_equal.c
  column_list_alias_to_columns.c
  get_column_piece_number.c
  run_query.c
  parse_startup_flags.c
  memory_chunk.c
  match_column_in_table.c
  natural_join_condition.c
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
  optimization_transforms/lp_get_criteria.c
  optimization_transforms/lp_get_key_index.c
  optimization_transforms/lp_generate_set_logical_plan.c
  optimization_transforms/lp_generate_xref_plan.c
  optimization_transforms/lp_generate_xref_keys.c
  optimization_transforms/lp_get_table_join.c
  optimization_transforms/lp_make_key.c
  optimization_transforms/lp_optimize_where_multi_equals_ands.c
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
  $<TARGET_OBJECTS:libhelpers>
)

add_library(librocto OBJECT
  default_octo_conf.h
  rocto/read_bind.c
  rocto/read_startup_message.c
  rocto/read_query.c
  rocto/read_parse.c
  rocto/read_execute.c
  rocto/read_sync.c
  rocto/read_describe.c
  rocto/handle_bind.c
  rocto/handle_query.c
  rocto/handle_parse.c
  rocto/handle_execute.c
  rocto/handle_describe.c
  rocto/make_error_response.c
  rocto/make_bind_complete.c
  rocto/make_ready_for_query.c
  rocto/make_empty_query_response.c
  rocto/make_row_description.c
  rocto/make_data_row.c
  rocto/make_command_complete.c
  rocto/make_authentication_md5_password.c
  rocto/make_authentication_ok.c
  rocto/make_parse_complete.c
  rocto/make_parameter_status.c
  rocto/make_no_data.c
  rocto/send_message.c
  rocto/read_message.c
  rocto/read_bytes.c
  rocto/rocto_main_loop.c
  rocto/free_error_response.c
  rocto/get_plan_row_description.c
  rocto/handle_query_response.c
  #  rocto/read_authentication_md5_password.c
  rocto/read_authentication_ok.c
  rocto/read_bind_complete.c
  rocto/read_command_complete.c
  #  rocto/read_data_row.c
  #  rocto/read_empty_query_response.c
  #  rocto/read_error_response.c
  #  rocto/read_parameter_status.c
  #  rocto/read_parse_complete.c
  #  rocto/read_ready_for_query.c
  #  rocto/read_row_description.c
)

add_executable(rocto
  rocto.c
  $<TARGET_OBJECTS:librocto>
  $<TARGET_OBJECTS:libocto>
  $<TARGET_OBJECTS:libhelpers>
)

add_executable(pparser
  default_octo_conf.h
  physical/pparser.c
  errors.c
  #octo_init.c
  ${BISON_PhysicalParser_OUTPUTS}
  ${FLEX_PhysicalLexer_OUTPUTS}
)

# Generate C files from templates
set(template_dir ${PROJECT_SOURCE_DIR}/src/m_templates)
set(template_files
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
  ${template_dir}/tmpl_column_reference_trigger.ctemplate
  )
foreach(template ${template_files})
  get_filename_component(fileName ${template} NAME)
  add_custom_command(OUTPUT ${fileName}.c
    DEPENDS "${template}" pparser
    COMMAND ${PROJECT_BINARY_DIR}/src/pparser < ${template} > ${fileName}.c
    )
  target_sources(libocto PUBLIC ${PROJECT_BINARY_DIR}/src/${fileName}.c)
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

target_link_libraries(octo ${YOTTADB_LIBRARIES} ${Readline_LIBRARY} ${CONFIG_LIBRARY} ${OPENSSL_LIBRARIES})
target_link_libraries(rocto ${YOTTADB_LIBRARIES} ${Readline_LIBRARY} ${CONFIG_LIBRARY} ${OPENSSL_LIBRARIES})
target_link_libraries(pparser ${YOTTADB_LIBRARIES} ${Readline_LIBRARY} ${CONFIG_LIBRARY} ${OPENSSL_LIBRARIES})

enable_testing()
include(${PROJECT_SOURCE_DIR}/cmake/tests.cmake)

if(CMOCKA_FOUND)
  include(${PROJECT_SOURCE_DIR}/cmake/cmocka-tests.cmake)
endif()
if(BATS_FOUND)
  include(${PROJECT_SOURCE_DIR}/cmake/bats-tests.cmake)
endif()
