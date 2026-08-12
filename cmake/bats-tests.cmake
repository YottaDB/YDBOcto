#################################################################
#								#
# Copyright (c) 2019-2026 YottaDB LLC and/or its subsidiaries.	#
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

# Add a core test that runs serially and cannot be parallelized
macro(ADD_BATS_TEST_SERIAL TEST_NAME)
	ADD_BATS_TEST(${TEST_NAME})
	set_tests_properties(${TEST_NAME} PROPERTIES RUN_SERIAL TRUE)
endmacro(ADD_BATS_TEST_SERIAL)

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
configure_file(
  "${PROJECT_SOURCE_DIR}/tests/test_helpers.bash.in"
  "${PROJECT_BINARY_DIR}/bats_tests/test_helpers.bash"
  @ONLY
)

configure_file(
  "${PROJECT_SOURCE_DIR}/tests/fixtures/QueryGenerator.m.in"
  "${PROJECT_BINARY_DIR}/configured_fixtures/QueryGenerator.m"
)

# Core tests to always run
ADD_BATS_TEST(basic_parsing)
ADD_BATS_TEST(hello_bats)
ADD_BATS_TEST(select_columns)

find_program(psql NAMES psql)
if(psql)
	ADD_BATS_TEST(psql_authentication)
endif()

# These tests are only run in the full test suite, but omitted during installation testing
if("${FULL_TEST_SUITE}")
	# Java is needed for many tests, so we need to find it and compile the code we need
	find_package(Java COMPONENTS Development REQUIRED)
	if(Java_Development_FOUND)
		include(UseJava)
		set(JAVA_SOURCE_FILES
			${PROJECT_SOURCE_DIR}/tests/fixtures/TDTT098.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TDTT062.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TJC002.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TJC008.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TJC010.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TJC011.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TJC015.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TJC016.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TJC017.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TJC018.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TJC019.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TJC020.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TJC022.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TJC023.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TII12.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/TBCR002.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/run_multi_query.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/run_multiple_query_files.java
			${PROJECT_SOURCE_DIR}/tests/fixtures/run_query.java
		)
		add_jar(jocto ${JAVA_SOURCE_FILES})

		# Get the Postgres JDBC file to use against Octo
		set(JDBC_VERSION "42.7.4") # this is the latest driver as of August 2024
		file(DOWNLOAD "https://jdbc.postgresql.org/download/postgresql-${JDBC_VERSION}.jar" ${PROJECT_BINARY_DIR}/postgresql.jar
		EXPECTED_HASH SHA1=264310fd7b2cd76738787dc0b9f7ea2e3b11adc1)
	endif()

	# These tests do things that influence the behavior of other tests, and thus
	# they are run alone.
	ADD_BATS_TEST_SERIAL(no_parallel_suite)

	# hello_db is a pre-requisite for the _DML tests
	ADD_BATS_TEST(hello_db)

	# These tests don't need Postgres
	ADD_BATS_TEST(verbosity)
	ADD_BATS_TEST(readline)
	ADD_BATS_TEST(help_option)
	ADD_BATS_TEST(port_option)
	ADD_BATS_TEST(adduser)
	ADD_BATS_TEST(deleteuser)
	ADD_BATS_TEST(showusers)
	ADD_BATS_TEST(order_by_where)
	ADD_BATS_TEST(inner_join)
	ADD_BATS_TEST(index_statement)
	ADD_BATS_TEST(exit)
	ADD_BATS_TEST(inner_join_where)
	ADD_BATS_TEST(inner_join_where_order_by)
	ADD_BATS_TEST(unknown_column)
	ADD_BATS_TEST(subquery_invalid_table)
	ADD_BATS_TEST(select_subquery_union)
	ADD_BATS_TEST(drop_table)
	ADD_BATS_TEST(computed_columns)
	ADD_BATS_TEST(prep_statements)
	ADD_BATS_TEST(unique_filenames)
	ADD_BATS_TEST(insert_into)
	ADD_BATS_TEST(optional_create_table_settings)
	ADD_BATS_TEST(cross_index_optimization)
	ADD_BATS_TEST(seed_queries)
	ADD_BATS_TEST(squirrel_sql_connect_queries)
	ADD_BATS_TEST(squirrel_sql_list_all_columns)
	ADD_BATS_TEST(long_lines)
	ADD_BATS_TEST(multicmd_line)
	ADD_BATS_TEST(customers_database)
	ADD_BATS_TEST(null_subs_check)
	ADD_BATS_TEST(log_split)
	ADD_BATS_TEST(octo_zroutines)
	ADD_BATS_TEST(memory_usage)
	ADD_BATS_TEST(large_queries)
	ADD_BATS_TEST(drop_function)
	ADD_BATS_TEST(full_join)
	ADD_BATS_TEST(physical_plans)
	ADD_BATS_TEST(discard_all)
	ADD_BATS_TEST(primary_key)
	ADD_BATS_TEST(buffer_resize)
	ADD_BATS_TEST(pgadmin)
	ADD_BATS_TEST(display_relation_commands)
	ADD_BATS_TEST(truncate)
	ADD_BATS_TEST(ydbjnlf_in_octo)
	ADD_BATS_TEST(fuzzing_queries)
	ADD_BATS_TEST(iterator)

	# The following tests only require the psql command line client, not Postgres
	# (Therefore, we don't need databases set-up for these tests)
	if(psql)
		ADD_BATS_TEST(psql_connection)
		ADD_BATS_TEST(cancel_request)
		ADD_BATS_TEST(permissions)
	endif()

	# These tests require Postgres running
	ADD_BATS_TEST_DML(errors)
	ADD_BATS_TEST_DML(where)
	ADD_BATS_TEST_DML(order_by)
	ADD_BATS_TEST_DML(createtable1)
	ADD_BATS_TEST_DML(createtable2)
	ADD_BATS_TEST_DML(createtable3)
	ADD_BATS_TEST_DML(create_function)
	ADD_BATS_TEST_DML(create_view1_1)
	ADD_BATS_TEST_DML(create_view1_2)
	ADD_BATS_TEST_DML(create_view1_3)
	ADD_BATS_TEST_DML(create_view1_4)
	ADD_BATS_TEST_DML(create_view2_1)
	ADD_BATS_TEST_DML(create_view2_2)
	ADD_BATS_TEST_DML(create_view2_3)
	ADD_BATS_TEST_DML(create_view3)
	ADD_BATS_TEST_DML(natural_join)
	ADD_BATS_TEST_DML(outer_join)
	ADD_BATS_TEST_DML(inner_join_order_by)
	ADD_BATS_TEST_DML(select_distinct)
	ADD_BATS_TEST_DML(where_optimizations)
	ADD_BATS_TEST_DML(xref)
	ADD_BATS_TEST_DML(hash_query)
	ADD_BATS_TEST_DML(boolean_expression_expansion)
	ADD_BATS_TEST_DML(composite_key)
	ADD_BATS_TEST_DML(case_statement)
	ADD_BATS_TEST_DML(select_subquery)
	ADD_BATS_TEST_DML(exists_operator)
	ADD_BATS_TEST_DML(between_operator)
	ADD_BATS_TEST_DML(anyallsome_operator)
	ADD_BATS_TEST_DML(qualified_operator)
	ADD_BATS_TEST_DML(where_in)
	ADD_BATS_TEST_DML(delete_from_table)
	ADD_BATS_TEST_DML(update_table)
	ADD_BATS_TEST_DML(cross_join)
	ADD_BATS_TEST_DML(limit)
	ADD_BATS_TEST_DML(set_operations)
	ADD_BATS_TEST_DML(sqllogic)
	ADD_BATS_TEST_DML(join_on_columns)
	ADD_BATS_TEST_DML(coerce_type)
	ADD_BATS_TEST_DML(regex_type_expressions)
	ADD_BATS_TEST_DML(query_generator)
	ADD_BATS_TEST_DML(group_by)
	ADD_BATS_TEST_DML(boolean_type)
	ADD_BATS_TEST_DML(null_keyword)
	ADD_BATS_TEST_DML(conditional_expression_functions)
	ADD_BATS_TEST_DML(aggregate_functions)
	ADD_BATS_TEST_DML(as_keyword)
	ADD_BATS_TEST_DML(random_octo_client)
	ADD_BATS_TEST_DML(math_functions)
	ADD_BATS_TEST_DML(not_operator)
	ADD_BATS_TEST_DML(values_clause)
	ADD_BATS_TEST_DML(framework)
	ADD_BATS_TEST_DML(array_syntax)
	ADD_BATS_TEST_DML(constraint_table_column)
	ADD_BATS_TEST_DML(tablename_asterisk)
	ADD_BATS_TEST_DML(powerbi)
	ADD_BATS_TEST_DML(select_columns_psql)
	ADD_BATS_TEST_DML(auto_upgrade)
	ADD_BATS_TEST_DML(jdbc_connection)
	ADD_BATS_TEST_DML(pg_functions)
	ADD_BATS_TEST_DML(dbeaver_connect_queries)
	ADD_BATS_TEST_DML(type_cast)
	ADD_BATS_TEST_DML(begin_commit_rollback)
	ADD_BATS_TEST_DML(date_time_type)
	ADD_BATS_TEST_DML(date_time_type2)
	ADD_BATS_TEST_DML(date_time_type3)
	ADD_BATS_TEST_DML(date_time_type4)
	ADD_BATS_TEST_DML(date_time_type5)
	ADD_BATS_TEST_DML(date_time_type6)
	ADD_BATS_TEST_DML(date_time_functions)

	find_program(go NAMES go)
	if(go)
		ADD_BATS_TEST_DML(psql_go_connection)
	endif()

	find_program(isql NAMES isql)
	if(isql)
		ADD_BATS_TEST_DML(odbc_connection)
	endif()

	find_program(Rscript NAMES Rscript)
	if(Rscript)
		ADD_BATS_TEST(r_connection)
	endif()

	ADD_BATS_TEST_DML(octo_conf)
endif()
if(${TEST_SPEED})
	ADD_BATS_TEST_WITH_TIME(speed)
endif()
if(${TEST_VISTA})
	set(TEST_VISTA_ENV_FILE "" CACHE FILEPATH "Path to VistA Environment File")
	set(TEST_VISTA_INPUT_SQL "" CACHE FILEPATH "Path to a VistA DDL SQL file (prevents having to recreate it)")
	set(TEST_VISTA_INPUT_M "" CACHE FILEPATH "Local copy of a _YDBOCTOVISTAM.m file")
	set(TEST_VISTA_INPUT_F "" CACHE FILEPATH "Local copy of a _YDBOCTOVISTAF.m file")
	set(TEST_VISTA_INPUT_F_SQL "" CACHE FILEPATH "Local copy of _YDBOCTOVISTAF.sql")
	ADD_BATS_TEST(vista_database)
endif()
