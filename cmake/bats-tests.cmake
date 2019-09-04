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

macro(ADD_BATS_TEST TEST_NAME)
  configure_file (
    "${PROJECT_SOURCE_DIR}/tests/${TEST_NAME}.bats.in"
    "${PROJECT_BINARY_DIR}/bats_tests/${TEST_NAME}.bats"
    @ONLY
  )
  add_test(${TEST_NAME} ${BATS} --tap ${PROJECT_BINARY_DIR}/bats_tests/${TEST_NAME}.bats)
endmacro(ADD_BATS_TEST)

# Copy over the setup script
configure_file (
  "${PROJECT_SOURCE_DIR}/tests/test_helpers.bash.in"
  "${PROJECT_BINARY_DIR}/bats_tests/test_helpers.bash"
)

ADD_BATS_TEST(hello_bats)
ADD_BATS_TEST(test_unique_filenames)
ADD_BATS_TEST(test_where)
ADD_BATS_TEST(test_where_in)
ADD_BATS_TEST(test_select_columns)
# This test can be added when Octo supports INSERT statements
#ADD_BATS_TEST(test_insert_from_table)
ADD_BATS_TEST(test_cross_join)
ADD_BATS_TEST(test_order_by)
ADD_BATS_TEST(test_order_by_where)
ADD_BATS_TEST(test_limit)
ADD_BATS_TEST(test_inner_join)
ADD_BATS_TEST(test_inner_join_where)
ADD_BATS_TEST(test_inner_join_order_by)
ADD_BATS_TEST(test_inner_join_where_order_by)
ADD_BATS_TEST(test_where_optimizations)
ADD_BATS_TEST(test_select_distinct)
ADD_BATS_TEST(test_select_subquery_union)
ADD_BATS_TEST(test_set_operations)
ADD_BATS_TEST(test_optional_create_table_settings)
ADD_BATS_TEST(test_cross_index_optimization)
ADD_BATS_TEST(test_create_table)
ADD_BATS_TEST(test_natural_join)
ADD_BATS_TEST(test_outer_join)
ADD_BATS_TEST(test_errors)
ADD_BATS_TEST(test_seed_queries)
ADD_BATS_TEST(test_squirrel_sql_connect_queries)
ADD_BATS_TEST(test_squirrel_sql_list_all_columns)
ADD_BATS_TEST(test_adduser)
ADD_BATS_TEST(test_deleteuser)
ADD_BATS_TEST(test_showusers)
ADD_BATS_TEST(test_exit)
ADD_BATS_TEST(test_help_option)
ADD_BATS_TEST(test_port_option)
ADD_BATS_TEST(test_long_lines)
ADD_BATS_TEST(test_multicmd_line)
ADD_BATS_TEST(test_unknown_column)
ADD_BATS_TEST(test_customers_database)
ADD_BATS_TEST(test_subquery_invalid_table)
ADD_BATS_TEST(test_drop_table)
ADD_BATS_TEST(test_xref)
ADD_BATS_TEST(test_sqllogic_select1)
ADD_BATS_TEST(test_null_subs_check)
ADD_BATS_TEST(test_hash_query)
ADD_BATS_TEST(test_computed_columns)
ADD_BATS_TEST(test_join_on_columns)
ADD_BATS_TEST(test_boolean_expression_expansion)
ADD_BATS_TEST(test_log_split)
ADD_BATS_TEST(test_coerce_type)
ADD_BATS_TEST(test_composite_key)
ADD_BATS_TEST(test_case_statement)
ADD_BATS_TEST(test_octo_zroutines)
ADD_BATS_TEST(test_memory_usage)

find_program(psql NAMES psql)
if(psql)
  ADD_BATS_TEST(test_psql_connection)
  ADD_BATS_TEST(test_psql_authentication)
  ADD_BATS_TEST(test_cancel_request)
endif()

find_program(go NAMES go)
if(go)
  ADD_BATS_TEST(test_psql_go_connection)
endif()
