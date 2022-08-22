#################################################################
#								#
# Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

macro(CONFIGURE_BATS_TEST TEST_NAME)
  configure_file (
    "${PROJECT_SOURCE_DIR}/tests/${TEST_NAME}.bats.in"
    "${PROJECT_BINARY_DIR}/bats_tests/${TEST_NAME}.bats"
    @ONLY
  )
endmacro()

# Add a core test that does not rely on any external database, e.g. PostgreSQL, being loaded
macro(ADD_BATS_TEST TEST_NAME)
  CONFIGURE_BATS_TEST(${TEST_NAME})
  add_test(${TEST_NAME} ${BATS} --tap ${PROJECT_BINARY_DIR}/bats_tests/${TEST_NAME}.bats)
endmacro(ADD_BATS_TEST)

macro(ADD_BATS_TEST_DML TEST_NAME)
	ADD_BATS_TEST(${TEST_NAME})
	# Set hello_db as a dependency for the given test to ensure that external databases, e.g. PostgreSQL,
	# are properly loaded with test data before running the test.
	set_tests_properties(${TEST_NAME} PROPERTIES DEPENDS "hello_db")
endmacro(ADD_BATS_TEST_DML)

# Note: Any newly defined macro in this file that is used to add tests which get run in the "test-auto-upgrade" job
# would need special handling in "tools/ci/testAutoUpgrade.m".
# The "ADD_BATS_TEST_WITH_TIME" macro does not need such special handling as tests
# defined using it do not get run in the "test-auto-upgrade" job.

macro(ADD_BATS_TEST_WITH_TIME TEST_NAME)
  CONFIGURE_BATS_TEST(${TEST_NAME})
  add_test(${TEST_NAME} ${BATS} -T --tap ${PROJECT_BINARY_DIR}/bats_tests/${TEST_NAME}.bats)
endmacro(ADD_BATS_TEST_WITH_TIME)

# Copy over the setup script
configure_file (
  "${PROJECT_SOURCE_DIR}/tests/test_helpers.bash.in"
  "${PROJECT_BINARY_DIR}/bats_tests/test_helpers.bash"
)

# Core tests to always run
ADD_BATS_TEST(test_basic_parsing)
ADD_BATS_TEST(hello_bats)
ADD_BATS_TEST(test_select_columns)

find_program(psql NAMES psql)
if(psql)
	ADD_BATS_TEST(test_psql_authentication)
endif()

# These tests are only run in the full test suite, but omitted during installation testing
if("${FULL_TEST_SUITE}")
	# Configure hello_db directly instead of calling `ADD_BATS_TEST` to prevent a dependency cycle
	# where hello_db depends on hello_db. See `ADD_BATS_TEST` definition above for more info.
	CONFIGURE_BATS_TEST(hello_db) # Requires MySQL and PostgreSQL
	add_test(hello_db ${BATS} --tap ${PROJECT_BINARY_DIR}/bats_tests/hello_db.bats)

	ADD_BATS_TEST_DML(test_where)
	ADD_BATS_TEST_DML(test_order_by)
	ADD_BATS_TEST_DML(test_order_by_where)
	ADD_BATS_TEST_DML(test_inner_join)
	ADD_BATS_TEST_DML(test_createtable)
	ADD_BATS_TEST_DML(test_create_function)
	ADD_BATS_TEST_DML(test_index_statement)
	ADD_BATS_TEST_DML(test_natural_join)
	ADD_BATS_TEST_DML(test_outer_join)
	ADD_BATS_TEST_DML(test_errors)
	ADD_BATS_TEST_DML(test_adduser)
	ADD_BATS_TEST_DML(test_deleteuser)
	ADD_BATS_TEST_DML(test_showusers)
	ADD_BATS_TEST_DML(test_exit)
	ADD_BATS_TEST_DML(test_help_option)
	ADD_BATS_TEST_DML(test_port_option)
	ADD_BATS_TEST_DML(test_verbosity)
	ADD_BATS_TEST_DML(test_inner_join_where)
	ADD_BATS_TEST_DML(test_inner_join_order_by)
	ADD_BATS_TEST_DML(test_inner_join_where_order_by)
	ADD_BATS_TEST_DML(test_select_distinct)
	ADD_BATS_TEST_DML(test_where_optimizations)
	ADD_BATS_TEST_DML(test_select_subquery_union)
	ADD_BATS_TEST_DML(test_unknown_column)
	ADD_BATS_TEST_DML(test_subquery_invalid_table)
	ADD_BATS_TEST_DML(test_drop_table)
	ADD_BATS_TEST_DML(test_xref)
	ADD_BATS_TEST_DML(test_hash_query)
	ADD_BATS_TEST_DML(test_computed_columns)
	ADD_BATS_TEST_DML(test_boolean_expression_expansion)
	ADD_BATS_TEST_DML(test_composite_key)
	ADD_BATS_TEST_DML(test_case_statement)
	ADD_BATS_TEST_DML(test_prep_statements)
	ADD_BATS_TEST_DML(test_select_subquery)
	ADD_BATS_TEST_DML(test_exists_operator)
	ADD_BATS_TEST_DML(test_between_operator)
	ADD_BATS_TEST_DML(test_anyallsome_operator)
	ADD_BATS_TEST_DML(test_readline)
	ADD_BATS_TEST_DML(test_unique_filenames)
	ADD_BATS_TEST_DML(test_where_in)
	ADD_BATS_TEST_DML(test_insert_into)
	ADD_BATS_TEST_DML(test_delete_from_table)
	ADD_BATS_TEST_DML(test_update_table)
	ADD_BATS_TEST_DML(test_cross_join)
	ADD_BATS_TEST_DML(test_limit)
	ADD_BATS_TEST_DML(test_set_operations)
	ADD_BATS_TEST_DML(test_optional_create_table_settings)
	ADD_BATS_TEST_DML(test_cross_index_optimization)
	ADD_BATS_TEST_DML(test_seed_queries)
	ADD_BATS_TEST_DML(test_squirrel_sql_connect_queries)
	ADD_BATS_TEST_DML(test_squirrel_sql_list_all_columns)
	ADD_BATS_TEST_DML(test_long_lines)
	ADD_BATS_TEST_DML(test_multicmd_line)
	ADD_BATS_TEST_DML(test_customers_database)
	ADD_BATS_TEST_DML(test_sqllogic)
	ADD_BATS_TEST_DML(test_null_subs_check)
	ADD_BATS_TEST_DML(test_join_on_columns)
	ADD_BATS_TEST_DML(test_log_split)
	ADD_BATS_TEST_DML(test_coerce_type)
	ADD_BATS_TEST_DML(test_octo_zroutines)
	ADD_BATS_TEST_DML(test_memory_usage)
	ADD_BATS_TEST_DML(test_large_queries)
	ADD_BATS_TEST_DML(test_regex_type_expressions)
	ADD_BATS_TEST_DML(test_query_generator)
	ADD_BATS_TEST_DML(test_group_by)
	ADD_BATS_TEST_DML(test_boolean_type)
	ADD_BATS_TEST_DML(test_null_keyword)
	ADD_BATS_TEST_DML(test_conditional_expression_functions)
	ADD_BATS_TEST_DML(test_aggregate_functions)
	ADD_BATS_TEST_DML(test_as_keyword)
	ADD_BATS_TEST_DML(test_drop_function)
	ADD_BATS_TEST_DML(test_full_join)
	ADD_BATS_TEST_DML(test_physical_plans)
	ADD_BATS_TEST_DML(test_random_octo_client)
	ADD_BATS_TEST_DML(test_math_functions)
	ADD_BATS_TEST_DML(test_discard_all)
	ADD_BATS_TEST_DML(test_primary_key)
	ADD_BATS_TEST_DML(test_not_operator)
	ADD_BATS_TEST_DML(test_values_clause)
	ADD_BATS_TEST_DML(test_buffer_resize)
	ADD_BATS_TEST_DML(test_framework)
	ADD_BATS_TEST_DML(test_pgadmin)
	ADD_BATS_TEST_DML(test_array_syntax)
	ADD_BATS_TEST_DML(test_constraint_table_column)
	ADD_BATS_TEST_DML(test_tablename_asterisk)
	ADD_BATS_TEST_DML(test_display_relation_commands)
	ADD_BATS_TEST_DML(test_truncate)

	# The following tests require PostgreSQL
	ADD_BATS_TEST_DML(test_psql_connection)
	ADD_BATS_TEST_DML(test_cancel_request)
	ADD_BATS_TEST_DML(test_select_columns_psql)
	ADD_BATS_TEST_DML(test_permissions)
	# The following test requires MySQL and PostgreSQL
	ADD_BATS_TEST_DML(test_date_time_functions)

	find_program(go NAMES go)
	if(go)
		ADD_BATS_TEST_DML(test_psql_go_connection)
	endif()

	find_program(java NAMES java)
	if(java)
		ADD_BATS_TEST_DML(test_jdbc_connection)
	endif()

	find_program(isql NAMES isql)
	if(isql)
		ADD_BATS_TEST_DML(test_odbc_connection)
	endif()

	ADD_BATS_TEST_DML(test_octo_conf)
endif()
if(${TEST_SPEED})
	ADD_BATS_TEST_WITH_TIME(test_speed)
endif()
if(${TEST_VISTA})
	set(TEST_VISTA_ENV_FILE "" CACHE FILEPATH "Path to VistA Environment File")
	set(TEST_VISTA_INPUT_SQL "" CACHE FILEPATH "Path to a VistA DDL SQL file (prevents having to recreate it)")
	set(TEST_VISTA_INPUT_M "" CACHE FILEPATH "Local copy of a _YDBOCTOVISTAM.m file")
	set(TEST_VISTA_INPUT_F "" CACHE FILEPATH "Local copy of a _YDBOCTOVISTAF.m file")
	set(TEST_VISTA_INPUT_F_SQL "" CACHE FILEPATH "Local copy of _YDBOCTOVISTAF.sql")
	ADD_BATS_TEST(test_vista_database)
endif()
