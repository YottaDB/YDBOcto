#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
find_package(YOTTADB REQUIRED)
find_package(Readline REQUIRED)
find_package(Config REQUIRED)
find_package(OpenSSL REQUIRED)
find_package(CMOCKA)
find_package(BATS)

set(PRE_CONFIGURE_FILE "${PROJECT_SOURCE_DIR}/src/git_hashes.h.in")
set(POST_CONFIGURE_FILE "${PROJECT_BINARY_DIR}/git_hashes.h")
include(${PROJECT_SOURCE_DIR}/cmake/git-watcher.cmake)
set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${PROJECT_BINARY_DIR}/git-state")

if(DEFINED YOTTADB_INCLUDE_DIRS AND EXISTS "${YOTTADB_INCLUDE_DIRS}/plugin/libgtmtls.so")
  message(STATUS "Found YottaDB TLS plugin: ${YOTTADB_INCLUDE_DIRS}")
    set(YDB_TLS_AVAILABLE 1)
else()
  message(STATUS "Could NOT find YottaDB TLS plugin in ${YOTTADB_INCLUDE_DIRS}")
    set(YDB_TLS_AVAILABLE 0)
endif()

configure_file (
  "${PROJECT_SOURCE_DIR}/src/constants.h.in"
  "${PROJECT_BINARY_DIR}/constants.h")

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
          parser/select.y parser/insert.y parser/update.y
          parser/drop.y
          parser/set.y
  COMMAND awk -v path=${PROJECT_SOURCE_DIR}/src/
    -f ${PROJECT_SOURCE_DIR}/tools/combine-grammar.awk
    ${PROJECT_SOURCE_DIR}/src/parser.y > ${PROJECT_BINARY_DIR}/parser.y
  COMMENT "Combining grammar files"
)

BISON_TARGET(OctoParser ${PROJECT_BINARY_DIR}/parser.y
  ${PROJECT_BINARY_DIR}/parser.c)
FLEX_TARGET(OctoLexer lexer.l ${CMAKE_CURRENT_BINARY_DIR}/lexer.c COMPILE_FLAGS --header-file=${PROJECT_BINARY_DIR}/lexer.h)

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
  ${POST_CONFIGURE_FILE}
)
add_dependencies(libhelpers check_git_repository)

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
  ydb_hash_to_string.c
  yyerror.c
  yywrap.c
  format_error_string.c
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
  find_column_alias_name.c
  qualify_column_list.c
  qualify_column_list_alias.c
  qualify_column_name.c
  qualify_statement.c
  qualify_function_name.c
  qualify_query.c
  octo_init.c
  get_keyword.c
  get_keyword_from_keywords.c
  generate_routine_name.c
  generate_key_name.c
  get_key_columns.c
  hash_canonical_query.c
  assign_table_to_columns.c
  add_optional_piece_keyword_to_sql_column.c
  columns_equal.c
  tables_equal.c
  values_equal.c
  column_list_alias_to_columns.c
  columns_to_column_list_alias.c
  get_column_piece_number.c
  drill_to_table_alias.c
  run_query.c
  parse_startup_flags.c
  memory_chunk.c
  match_column_in_table.c
  natural_join_condition.c
  like_to_regex.c
  similar_to_regex.c
  compress_statement.c
  decompress_statement.c
  store_table_in_pg_class.c
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
  optimization_transforms/lp_copy_key.c
  optimization_transforms/lp_join_where.c
  optimization_transforms/lp_generate_where.c
  optimization_transforms/lp_column_list_to_lp.c
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
  optimization_transforms/lp_drill_to_insert.c
  optimization_transforms/lp_make_normal_disjunctive_form.c
  optimization_transforms/lp_join_plans.c
  optimization_transforms/generate_logical_plan.c
  optimization_transforms/logical_plan.c
  physical/generate_physical_plan.c
  physical/emit_physical_plan.c
  physical/is_prev_plan_in_same_dnf.c
  m_templates/tmpl_print_dots.c
  m_templates/resize_tmpl_buffer.c
  m_escape_string2.c
  copy_sql_statement.c
  cleanup_tables.c
  parser/query_specification.c
  parser/sort_specification.c
  parser/trim_dot_star.c
  parser/regex_specification.c
  ${BISON_OctoParser_OUTPUTS}
  ${FLEX_OctoLexer_OUTPUTS}
  ${POST_CONFIGURE_FILE}
)
add_dependencies(libocto check_git_repository)

add_executable(octo
  octo.c
  errors.c
  $<TARGET_OBJECTS:libocto>
  $<TARGET_OBJECTS:libhelpers>
  ${POST_CONFIGURE_FILE}
)
add_dependencies(octo check_git_repository _ydbocto)

if(${YDB_TLS_AVAILABLE})
  add_library(ydbtls SHARED IMPORTED GLOBAL)
  set_target_properties(ydbtls PROPERTIES
    IMPORTED_LOCATION "${YOTTADB_INCLUDE_DIRS}/plugin/libgtmtls.so"
  )
endif()


add_library(librocto OBJECT
  default_octo_conf.h
  rocto/copy_bind_parameter.c
  rocto/deserialize.c
  rocto/get_pid_start_time.c
  rocto/read_bind.c
  rocto/read_cancel_request.c
  rocto/read_startup_message.c
  rocto/read_ssl_request.c
  rocto/read_query.c
  rocto/read_parse.c
  rocto/read_execute.c
  rocto/read_flush.c
  rocto/read_sync.c
  rocto/read_terminate.c
  rocto/read_describe.c
  rocto/handle_bind.c
  rocto/handle_cancel_request.c
  rocto/handle_query.c
  rocto/handle_parse.c
  rocto/handle_execute.c
  rocto/handle_describe.c
  rocto/handle_password_message.c
  rocto/make_error_response.c
  rocto/make_bind_complete.c
  rocto/make_backend_key_data.c
  rocto/make_close_complete.c
  rocto/make_ready_for_query.c
  rocto/make_empty_query_response.c
  rocto/make_row_description.c
  rocto/make_data_row.c
  rocto/make_command_complete.c
  rocto/make_authentication_md5_password.c
  rocto/make_authentication_ok.c
  rocto/make_parse_complete.c
  rocto/make_parameter_status.c
  rocto/make_portal_suspended.c
  rocto/make_no_data.c
  rocto/make_parameter_description.c
  rocto/make_password_message.c
  rocto/make_startup_message.c
  rocto/send_message.c
  rocto/read_message.c
  rocto/read_bytes.c
  rocto/send_bytes.c
  rocto/rocto_main_loop.c
  rocto/free_error_response.c
  rocto/free_data_row.c
  rocto/free_row_description.c
  rocto/get_plan_row_description.c
  rocto/get_user_column_value.c
  rocto/handle_query_response.c
  #  rocto/read_authentication_md5_password.c
  rocto/read_authentication_ok.c
  rocto/read_bind_complete.c
  rocto/read_close.c
  rocto/read_close_complete.c
  rocto/read_command_complete.c
  rocto/read_data_row.c
  rocto/read_empty_query_response.c
  rocto/read_error_response.c
  rocto/read_parameter_status.c
  rocto/read_parse_complete.c
  rocto/read_password_message.c
  rocto/read_portal_suspended.c
  rocto/read_ready_for_query.c
  rocto/read_row_description.c
  rocto_helper_waitpid.c
  ${POST_CONFIGURE_FILE}
)
add_dependencies(librocto check_git_repository)

add_executable(rocto
  rocto.c
  rocto_errors.c
  $<TARGET_OBJECTS:librocto>
  $<TARGET_OBJECTS:libocto>
  $<TARGET_OBJECTS:libhelpers>
  ${POST_CONFIGURE_FILE}
)
add_dependencies(rocto check_git_repository)

add_executable(pparser
  default_octo_conf.h
  physical/pparser.c
  errors.c
  #octo_init.c
  ${BISON_PhysicalParser_OUTPUTS}
  ${FLEX_PhysicalLexer_OUTPUTS}
  ${POST_CONFIGURE_FILE}
)
add_dependencies(pparser check_git_repository)

# Generate C files from templates
set(template_dir ${PROJECT_SOURCE_DIR}/src/m_templates)
set(template_files
  ${template_dir}/tmpl_physical_plan.ctemplate
  ${template_dir}/tmpl_tablejoin.ctemplate
  ${template_dir}/tmpl_tablejoin_innerjoin.ctemplate
  ${template_dir}/tmpl_tablejoin_leftjoin.ctemplate
  ${template_dir}/tmpl_tablejoin_rightjoin.ctemplate
  ${template_dir}/tmpl_rightjoin_key.ctemplate
  ${template_dir}/tmpl_tablejoin_body.ctemplate
  ${template_dir}/tmpl_key_start.ctemplate
  ${template_dir}/tmpl_key_end.ctemplate
  ${template_dir}/tmpl_key.ctemplate
  ${template_dir}/tmpl_key_advance.ctemplate
  ${template_dir}/tmpl_key_source.ctemplate
  ${template_dir}/tmpl_temp_key_advance.ctemplate
  ${template_dir}/tmpl_print_expression.ctemplate
  ${template_dir}/tmpl_column_reference.ctemplate
  ${template_dir}/tmpl_column_list_combine.ctemplate
  ${template_dir}/tmpl_emit_source.ctemplate
  ${template_dir}/tmpl_column_reference_trigger.ctemplate
  ${template_dir}/tmpl_duplication_check.ctemplate
  ${template_dir}/tmpl_order_by_key.ctemplate
  )
foreach(template ${template_files})
  get_filename_component(fileName ${template} NAME)
  add_custom_command(OUTPUT ${fileName}.c
    DEPENDS "${template}" pparser
    COMMAND ${PROJECT_BINARY_DIR}/src/pparser < ${template} > ${fileName}.c
    )
  target_sources(libocto PUBLIC ${PROJECT_BINARY_DIR}/src/${fileName}.c)
endforeach()

# Create a shared library of M routines
set(aux_files
  aux/_ydboctoAdmin.m
  aux/_ydboctoCleanup.m
  aux/_ydboctoNullSubsCheck.m
  aux/_ydboctocurrentSchema.m
  aux/_ydboctopgCurrentSchemas.m
  aux/_ydboctopgGetExpr.m
  aux/_ydboctopgObjDescription.m
  aux/_ydboctopgRowNumber.m
  aux/_ydboctoplanhelpers.m
  aux/_ydboctoreplace.m
  aux/_ydboctoselect.m
  aux/_ydboctoversion.m
)
add_library(_ydbocto SHARED ${aux_files})
set_target_properties(_ydbocto PROPERTIES PREFIX "")

# manually place Call-In file
configure_file("${PROJECT_SOURCE_DIR}/src/aux/ydbocto.ci" ${PROJECT_BINARY_DIR}/ydbocto.ci COPYONLY)

set_property(TARGET libocto PROPERTY C_STANDARD 11)
set_property(TARGET librocto PROPERTY C_STANDARD 11)
set_property(TARGET pparser PROPERTY C_STANDARD 11)
set_property(TARGET octo PROPERTY C_STANDARD 11)
set_property(TARGET rocto PROPERTY C_STANDARD 11)

target_compile_options(libocto PRIVATE -Wall -Wextra -pedantic )
target_compile_options(librocto PRIVATE -Wall -Wextra -pedantic )

target_link_libraries(octo ${YOTTADB_LIBRARIES} ${Readline_LIBRARY} ${CONFIG_LIBRARY} ${OPENSSL_LIBRARIES})
target_link_libraries(rocto
  ${YOTTADB_LIBRARIES}
  ${Readline_LIBRARY}
  ${CONFIG_LIBRARY}
  ${OPENSSL_LIBRARIES}
  pthread)
if(${YDB_TLS_AVAILABLE})
    target_link_libraries(rocto ${YOTTADB_LIBRARIES} ${Readline_LIBRARY} ${CONFIG_LIBRARY} ${OPENSSL_LIBRARIES} ydbtls)
else()
    target_link_libraries(rocto ${YOTTADB_LIBRARIES} ${Readline_LIBRARY} ${CONFIG_LIBRARY} ${OPENSSL_LIBRARIES})
endif()
target_link_libraries(pparser ${YOTTADB_LIBRARIES} ${Readline_LIBRARY} ${CONFIG_LIBRARY} ${OPENSSL_LIBRARIES})

enable_testing()
include(${PROJECT_SOURCE_DIR}/cmake/tests.cmake)

if(CMOCKA_FOUND)
  include(${PROJECT_SOURCE_DIR}/cmake/cmocka-tests.cmake)
endif()
if(BATS_FOUND)
  include(${PROJECT_SOURCE_DIR}/cmake/bats-tests.cmake)
endif()

# Files to be installed

# Binaries
install(TARGETS octo
        DESTINATION bin)
install(TARGETS rocto
        DESTINATION bin)
install(PROGRAMS aux/install.sh
        DESTINATION .)

# M Routines
# Source Code
install(FILES ${aux_files}
        DESTINATION r)

# Shared Library
if(MUMPS_UTF8_MODE)
  install(TARGETS _ydbocto
          DESTINATION o/utf8)
else()
  install(TARGETS _ydbocto
          DESTINATION o)
endif()

# Configuration file
install(FILES aux/octo.conf.default
        DESTINATION etc
        RENAME octo.conf)
install(FILES ${PROJECT_SOURCE_DIR}/src/aux/ydbocto.ci
        DESTINATION .)

# Seed Data
install(FILES ${PROJECT_SOURCE_DIR}/tests/fixtures/postgres-seed.zwr
        DESTINATION etc)
install(FILES ${PROJECT_SOURCE_DIR}/tests/fixtures/postgres-seed.sql
        DESTINATION etc)

# CPack required variables
include (InstallRequiredSystemLibraries)
set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGE_NAME "Octo")
set(CPACK_PACKAGE_VENDOR "YottaDB LLC")
set(CPACK_PACKAGE_VERSION_MAJOR "${OCTO_SQL_MAJOR_VERSION}")
set(CPACK_PACKAGE_VERSION_MINOR "${OCTO_SQL_MINOR_VERSION}")
set(CPACK_PACKAGE_VERSION_PATCH "${OCTO_SQL_PATCH_VERSION}")

set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")
set(CPACK_RESOURCE_FILE_README "${CMAKE_SOURCE_DIR}/README.md")

# Enable cpack
include(CPack)
