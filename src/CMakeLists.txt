#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
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

# Detect if YDB is also installed in UTF-8 mode
if($ENV{ydb_dist} MATCHES "utf8" OR EXISTS $ENV{ydb_dist}/utf8)
	set(ydb_chsets "" "UTF-8")
	set(utf8_installed 1)
else()
	set(ydb_chsets "")
endif()

if(DEFINED YOTTADB_INCLUDE_DIRS AND EXISTS "${YOTTADB_INCLUDE_DIRS}/plugin/o/_ydbposix.so")
	message(STATUS "Found YDBPosix plugin: ${YOTTADB_INCLUDE_DIRS}/plugin/o/_ydbposix.so")
	if(utf8_installed)
		if(EXISTS "${YOTTADB_INCLUDE_DIRS}/plugin/o/utf8/_ydbposix.so")
			message(STATUS "Found YDBPosix plugin with UTF-8 support: ${YOTTADB_INCLUDE_DIRS}/plugin/o/utf8/_ydbposix.so")
		else()
			message(FATAL_ERROR "YottaDB supports UTF-8, but YDBPosix does not. Please install YDBPosix with UTF-8 support.")
		endif()
	endif()
else()
	message(FATAL_ERROR "Could NOT find YDBPosix plugin in ${YOTTADB_INCLUDE_DIRS}/o. Please install YDBPosix and rebuild Octo.")
endif()

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
  add_optional_piece_keyword_to_sql_column.c
  assign_table_to_columns.c
  cleanup_tables.c
  columns_equal.c
  columns_to_column_list_alias.c
  compress_statement.c
  copy_sql_statement.c
  create_cursor.c
  create_table_defaults.c
  decompress_statement.c
  default_octo_conf.h
  delete_table_from_pg_class.c
  drill_to_table_alias.c
  drop_table_from_local_cache.c
  emit_column_specification.c
  emit_create_table.c
  emit_select_statement.c
  find_column.c
  find_column_alias_name.c
  find_table.c
  format_error_string.c
  generate_key_name.c
  generate_routine_name.c
  get_aggregate_func_name.c
  get_column_alias_for_column_list_alias.c
  get_column_list_alias_n_from_table_alias.c
  get_column_number_from_column_list_alias.c
  get_column_piece_number.c
  get_key_columns.c
  get_keyword.c
  get_keyword_from_keywords.c
  get_mem_usage.c
  get_mval_len.c
  get_set_operation_string.c
  get_user_visible_type_string.c
  hash_canonical_query.c
  is_query_canceled.c
  m_escape_string2.c
  m_templates/resize_tmpl_buffer.c
  match_column_in_table.c
  match_column_list_alias_in_select_column_list.c
  match_sql_statement.c
  memory_chunk.c
  octo_init.c
  optimization_transforms/generate_logical_plan.c
  optimization_transforms/get_plan_unique_number.c
  optimization_transforms/logical_plan.c
  optimization_transforms/lp_column_list_to_lp.c
  optimization_transforms/lp_copy_key.c
  optimization_transforms/lp_copy_plan.c
  optimization_transforms/lp_drill_to_insert.c
  optimization_transforms/lp_emit_plan.c
  optimization_transforms/lp_generate_set_logical_plan.c
  optimization_transforms/lp_generate_where.c
  optimization_transforms/lp_generate_xref_keys.c
  optimization_transforms/lp_generate_xref_plan.c
  optimization_transforms/lp_get_aggregate_plan_helper_func_name.c
  optimization_transforms/lp_get_col_num_n_in_select_column_list.c
  optimization_transforms/lp_get_criteria.c
  optimization_transforms/lp_get_key.c
  optimization_transforms/lp_get_keys.c
  optimization_transforms/lp_get_num_cols_in_select_column_list.c
  optimization_transforms/lp_get_output_key.c
  optimization_transforms/lp_get_project.c
  optimization_transforms/lp_get_projection_columns.c
  optimization_transforms/lp_get_select.c
  optimization_transforms/lp_get_select_keywords.c
  optimization_transforms/lp_get_select_where.c
  optimization_transforms/lp_get_table_join.c
  optimization_transforms/lp_insert_key.c
  optimization_transforms/lp_is_bool_operand_type_string.c
  optimization_transforms/lp_join_where.c
  optimization_transforms/lp_make_key.c
  optimization_transforms/lp_make_normal_disjunctive_form.c
  optimization_transforms/lp_optimize_where_multi_equals_ands.c
  optimization_transforms/lp_replace_derived_table_references.c
  optimization_transforms/lp_verify_structure.c
  optimization_transforms/optimize_logical_plan.c
  parse_line.c
  parse_startup_flags.c
  parser/aggregate_function.c
  parser/as_name.c
  parser/between_predicate.c
  parser/cast_specification.c
  parser/derived_column.c
  parser/derived_table.c
  parser/grouping_column_reference.c
  parser/natural_join_condition.c
  parser/parse_literal_to_parameter.c
  parser/query_specification.c
  parser/regex_specification.c
  parser/set_operation.c
  parser/sort_specification.c
  parser/table_reference.c
  physical/emit_physical_plan.c
  physical/generate_physical_plan.c
  physical/get_physical_plan_from_unique_id.c
  populate_data_type.c
  print_result_row.c
  print_temporary_table.c
  qualify_column_name.c
  qualify_function_name.c
  qualify_query.c
  qualify_statement.c
  quoting.c
  readline_get_more.c
  run_query.c
  store_table_in_pg_class.c
  tables_equal.c
  values_equal.c
  ydb_error_check.c
  ydb_hash_to_string.c
  yyerror.c
  yywrap.c
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
  rocto/send_result_rows.c
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
  ${template_dir}/tmpl_column_list_combine.ctemplate
  ${template_dir}/tmpl_column_reference.ctemplate
  ${template_dir}/tmpl_duplication_check.ctemplate
  ${template_dir}/tmpl_emit_source.ctemplate
  ${template_dir}/tmpl_group_by.ctemplate
  ${template_dir}/tmpl_key.ctemplate
  ${template_dir}/tmpl_key_advance.ctemplate
  ${template_dir}/tmpl_key_end.ctemplate
  ${template_dir}/tmpl_key_source.ctemplate
  ${template_dir}/tmpl_key_start.ctemplate
  ${template_dir}/tmpl_limit_check.ctemplate
  ${template_dir}/tmpl_order_by_key.ctemplate
  ${template_dir}/tmpl_physical_plan.ctemplate
  ${template_dir}/tmpl_populate_output_key.ctemplate
  ${template_dir}/tmpl_print_dots.ctemplate
  ${template_dir}/tmpl_print_expression.ctemplate
  ${template_dir}/tmpl_rightjoin_key.ctemplate
  ${template_dir}/tmpl_tablejoin.ctemplate
  ${template_dir}/tmpl_tablejoin_body.ctemplate
  ${template_dir}/tmpl_tablejoin_body_group_by.ctemplate
  ${template_dir}/tmpl_tablejoin_deferred_plans.ctemplate
  ${template_dir}/tmpl_tablejoin_on_condition.ctemplate
  ${template_dir}/tmpl_temp_key_advance.ctemplate
  ${template_dir}/tmpl_where_or_having_or_on.ctemplate
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
  aux/_ydboctoInit.m
  aux/_ydboctoUtils.m
  aux/_ydboctocurrentSchema.m
  aux/_ydboctopgCurrentSchemas.m
  aux/_ydboctopgGetExpr.m
  aux/_ydboctopgObjDescription.m
  aux/_ydboctopgRowNumber.m
  aux/_ydboctoplanhelpers.m
  aux/_ydboctoreplace.m
  aux/_ydboctoselect.m
  aux/_ydboctosqlfunctions.m
  aux/_ydboctoversion.m
)
add_library(_ydbocto SHARED ${aux_files})
set_target_properties(_ydbocto PROPERTIES PREFIX "")

# manually place Call-In file
configure_file("${PROJECT_SOURCE_DIR}/src/aux/ydbocto.ci" ${PROJECT_BINARY_DIR}/src/ydbocto.ci COPYONLY)

set_property(TARGET libocto PROPERTY C_STANDARD 11)
set_property(TARGET librocto PROPERTY C_STANDARD 11)
set_property(TARGET pparser PROPERTY C_STANDARD 11)
set_property(TARGET octo PROPERTY C_STANDARD 11)
set_property(TARGET rocto PROPERTY C_STANDARD 11)

# Set C compiler flags for all Octo targets
set(CC_COMPILE_FLAGS "-Wall -Wextra -pedantic")
# Enable GCC/Clang specific flags for cmake
if (CMAKE_COMPILER_IS_GNUCC)
	# gcc 6.3.0 is known to have -Wmisleading-indentation. And gcc 4.8.5 is known to not have that.
	# Not sure what the intermediate versions support so we add this warning flag only for versions >= 6.3.0
	if(${CMAKE_C_COMPILER_VERSION} VERSION_GREATER "6.3.0")
		set(CC_COMPILE_FLAGS "${CC_COMPILE_FLAGS} -Wmisleading-indentation")
	endif()
	# gcc 10.1.0 issued a superfluous -Wrestrict warning in tmpl_column_list_combine.c (see commit message for details)
	# so disable it for now. May re-enable this in the future if later versions of gcc do not issue this warning.
	# gcc 7.5.0 is known to support -Wno-restrict. And gcc 4.8.5 is known to not support it.
	# Not sure what the intermediate versions support so we add this warning disable flag only for versions >= 7.5.0
	if(${CMAKE_C_COMPILER_VERSION} VERSION_GREATER "7.5.0")
		set(CC_COMPILE_FLAGS "${CC_COMPILE_FLAGS} -Wno-restrict")
	endif()
else()
	set(CC_COMPILE_FLAGS "${CC_COMPILE_FLAGS} -Wno-gnu-zero-variadic-macro-arguments")
endif()
set_target_properties(libocto librocto pparser octo rocto PROPERTIES COMPILE_FLAGS "${CC_COMPILE_FLAGS}")

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

if(CMOCKA_FOUND)
  include(${PROJECT_SOURCE_DIR}/cmake/cmocka-tests.cmake)
endif()
if(BATS_FOUND)
  include(${PROJECT_SOURCE_DIR}/cmake/bats-tests.cmake)
endif()

# Create separate object files and libraries for M and UTF-8 mode, if supported
set(LC_ALL_TEMP ${LC_ALL})
set(ICU_TEMP ${ydb_icu_version})
foreach(ydb_chset ${ydb_chsets})
	list(FIND ydb_chset "UTF-8" utf8_chset)
	if(0 GREATER_EQUAL ${utf8_chset})
		find_program(PKGCONFIG NAMES pkg-config)
		if(PKGCONFIG)
			execute_process(
				COMMAND ${PKGCONFIG} --modversion icu-io
				OUTPUT_VARIABLE icu_version
				RESULT_VARIABLE icu_failed
				OUTPUT_STRIP_TRAILING_WHITESPACE
				)
			if(icu_failed)
				message(FATAL_ERROR "Command\n ${PKGCONFIG} --modversion icu-io\nfailed (${icu_failed}).")
			elseif("${icu_version}" MATCHES "([0-9]+\\.[0-9]+)")
				set(ydb_icu_version "${icu_version}")
			else()
				message(FATAL_ERROR "Command\n ${PKGCONFIG} --modversion icu-io\nproduced unrecognized output:\n ${icu_version}")
			endif()
		else()
			message(FATAL_ERROR "Unable to find 'pkg-config'.  Set PKGCONFIG in CMake cache.")
		endif()

		find_program(LOCALECFG NAMES locale)
		if(LOCALECFG)
			execute_process(
				COMMAND ${LOCALECFG} -a
				OUTPUT_VARIABLE locale_list
				RESULT_VARIABLE locale_failed
				OUTPUT_STRIP_TRAILING_WHITESPACE
				)
			if(locale_failed)
				message(FATAL_ERROR "Command\n ${LOCALECFG} -a\nfailed (${locale_failed}).")
			endif()
			STRING(REGEX REPLACE "\n" ";" locale_list "${locale_list}")
			foreach(lc ${locale_list})
				string(TOLOWER "${lc}" lc_lower)
				if("x${lc_lower}" MATCHES "^x[a-zA-Z_]+\\.?utf-?8")
					set(LC_ALL ${lc})
					message("-- Setting locale to ${LC_ALL}")
					break()
				endif()
			endforeach(lc)
			if("${LC_ALL}" STREQUAL "")
				message("Locale undefined. Expect to see NONUTF8LOCALE during MUMPS routine compilation: ${locale_list}\n")
			endif()
		else()
			message(FATAL_ERROR "Unable to find 'locale'.  Set LOCALECFG in CMake cache.")
		endif()
		if (NOT EXISTS "${PROJECT_BINARY_DIR}/src/utf8")
			file(MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/src/utf8")
		endif()
		set(WORKING_DIR "${PROJECT_BINARY_DIR}/src/utf8")
	else()
		set(LC_ALL ${LC_ALL_TEMP})
		set(ydb_icu_version ${ICU_TEMP})
		set(WORKING_DIR "${PROJECT_BINARY_DIR}/src")
	endif()

	foreach(in ${aux_files})
		set(infile ${PROJECT_SOURCE_DIR}/src/${in})
		string(REPLACE "aux/" "" mfiles "${in}")
		string(REPLACE ".m" ".o" out "${mfiles}")
		add_custom_command(TARGET octo rocto POST_BUILD
			COMMAND ${CMAKE_COMMAND}
			  -D ydb_routines=.
			  -D ydb_chset=${ydb_chset}
			  -D LC_ALL=${LC_ALL}
			  -D mumps=$ENV{ydb_dist}/yottadb
			  -D ydb_icu_version=${ydb_icu_version}
			  -D args=${infile}
			  -P ${CMAKE_SOURCE_DIR}/cmake/mumps.cmake
			  WORKING_DIRECTORY ${WORKING_DIR}
			VERBATIM
			)
		if(ydb_chset MATCHES "UTF-8")
			list(APPEND utf8_objects ${WORKING_DIR}/${out})
		else()
			list(APPEND m_mode_objects ${WORKING_DIR}/${out})
		endif()
	endforeach()
endforeach()
if(utf8_installed)
	add_custom_command(TARGET octo rocto POST_BUILD
		COMMAND /usr/bin/cc -shared -o _ydbocto.so ${utf8_objects}
		DEPENDS ${utf8_objects}
		WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/src/utf8)
endif()
set(LC_ALL ${LC_ALL_TEMP})

# Files to be installed
#
# NOTE: Any files installed here will need to be added to tools/ci/install.sh,
# as `make install` cannot be straightforwardly used during Docker image creation
# in the build pipeline.

if (DISABLE_INSTALL MATCHES "OFF")
	# Binaries
	install(TARGETS octo
			DESTINATION octo/bin)
	install(TARGETS rocto
			DESTINATION octo/bin)

	if (NOT EXISTS ${CMAKE_INSTALL_PREFIX}/bin/)
	  install(DIRECTORY DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
	endif()

	# Create symbolic (soft) links to binaries from $ydb_dist/plugin/bin
	INSTALL(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_INSTALL_PREFIX}/octo/bin/octo ${CMAKE_INSTALL_PREFIX}/bin/octo)")
	INSTALL(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_INSTALL_PREFIX}/octo/bin/rocto ${CMAKE_INSTALL_PREFIX}/bin/rocto)")

	# M Routines
	# Source Code
	install(FILES ${aux_files}
			DESTINATION r)


	if(utf8_installed)
		install(FILES ${PROJECT_BINARY_DIR}/src/utf8/_ydbocto.so
			DESTINATION ${CMAKE_INSTALL_PREFIX}/o/utf8)
	endif()

	# Configuration file
	if(NOT EXISTS ${CMAKE_INSTALL_PREFIX}/octo/octo.conf)
		install(FILES aux/octo.conf.default
				DESTINATION octo
				RENAME octo.conf)
	endif()
	install(FILES ${PROJECT_SOURCE_DIR}/src/aux/ydbocto.ci
			DESTINATION octo)

	install(TARGETS _ydbocto
		DESTINATION ${CMAKE_INSTALL_PREFIX}/o)

	install(FILES ${PROJECT_BINARY_DIR}/postgres-seed.sql
		DESTINATION ${CMAKE_INSTALL_PREFIX}/octo)
	install(FILES ${PROJECT_BINARY_DIR}/postgres-seed.zwr
		DESTINATION ${CMAKE_INSTALL_PREFIX}/octo)

	# Add tests to make install target, if tests are supported. Only the limited test suite should be run at
	# install time so skip this rule for when the full test suite is requested.
	if(BATS_FOUND AND "${FULL_TEST_SUITE}" MATCHES "OFF")
		install(CODE "execute_process(COMMAND make test WORKING_DIRECTORY ${PROJECT_BINARY_DIR})")
	endif()

	# This line can be used when all supported platforms use CMake version 3.12 or above
	# add_compile_definitions(DISABLE_INSTALL=0)
	add_definitions(-DDISABLE_INSTALL=0)
else()
	install(CODE "MESSAGE(STATUS \"Installation disabled. No files were installed.\")")
	# This line can be used when all supported platforms use CMake version 3.12 or above
	# add_compile_definitions(DISABLE_INSTALL=1)
	add_definitions(-DDISABLE_INSTALL=1)
endif()

# Seed Data
# Copy CREATE TABLE statements for PostgreSQL catalog tables without "pg_catalog" or "information_schema" prefixes
add_custom_command(OUTPUT ${PROJECT_BINARY_DIR}/postgres-seed.sql
	COMMAND ${PROJECT_SOURCE_DIR}/tests/fixtures/convert_catalog_sql.sh ${PROJECT_BINARY_DIR}
	WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/tests/fixtures
)

# Copy PostgreSQL catalog functions without "pg_catalog" prefix
add_custom_command(OUTPUT ${PROJECT_BINARY_DIR}/postgres-seed.zwr
	COMMAND ${PROJECT_SOURCE_DIR}/tests/fixtures/convert_catalog_zwr.sh ${PROJECT_BINARY_DIR}
	WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/tests/fixtures
)

add_custom_target(
	convert_catalog_sql ALL
	DEPENDS ${PROJECT_BINARY_DIR}/postgres-seed.sql
)
add_custom_target(
	convert_catalog_zwr ALL
	DEPENDS ${PROJECT_BINARY_DIR}/postgres-seed.zwr
)

# Strip debug symbols from files
if("${CMAKE_BUILD_TYPE}" MATCHES "RelWithDebInfo")
	list(APPEND executable_target_list
		octo
		rocto
		)

	foreach(e ${executable_target_list})
		add_custom_command(TARGET ${e} POST_BUILD
			COMMAND objcopy --only-keep-debug $<TARGET_FILE:${e}> ${e}.dbg
			COMMAND objcopy --strip-debug $<TARGET_FILE:${e}>
			COMMAND objcopy --add-gnu-debuglink=${e}.dbg $<TARGET_FILE:${e}>
			WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/src"
			)
	endforeach()
endif()

# CPack required variables
include (InstallRequiredSystemLibraries)
set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGE_NAME "YDBOcto")
set(CPACK_PACKAGE_VENDOR "YottaDB LLC")
set(CPACK_PACKAGE_VERSION_MAJOR "${YDBOCTO_MAJOR_VERSION}")
set(CPACK_PACKAGE_VERSION_MINOR "${YDBOCTO_MINOR_VERSION}")
set(CPACK_PACKAGE_VERSION_PATCH "${YDBOCTO_PATCH_VERSION}")

set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")
set(CPACK_RESOURCE_FILE_README "${CMAKE_SOURCE_DIR}/README.md")

# Enable cpack
include(CPack)
