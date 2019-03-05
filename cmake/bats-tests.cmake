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
ADD_BATS_TEST(test_where)
ADD_BATS_TEST(test_where_in)
ADD_BATS_TEST(test_select_columns)
#ADD_BATS_TEST(test_insert_from_table)
ADD_BATS_TEST(test_cross_join)
ADD_BATS_TEST(test_order_by)
ADD_BATS_TEST(test_limit)
ADD_BATS_TEST(test_inner_join)
ADD_BATS_TEST(test_where_optimizations)
ADD_BATS_TEST(test_select_distinct)
ADD_BATS_TEST(test_set_operations)
ADD_BATS_TEST(test_optional_create_table_settings)
