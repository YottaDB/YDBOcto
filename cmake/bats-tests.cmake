macro(ADD_BATS_TEST TEST_NAME)
  configure_file (
    "${PROJECT_SOURCE_DIR}/tests/${TEST_NAME}.bats.in"
    "${PROJECT_BINARY_DIR}/bats_tests/${TEST_NAME}.bats"
  )
  add_test(${TEST_NAME} ${BATS} ${PROJECT_BINARY_DIR}/bats_tests/${TEST_NAME}.bats)
endmacro(ADD_BATS_TEST)

# Copy over the setup script
configure_file (
  "${PROJECT_SOURCE_DIR}/tests/test_helpers.bash.in"
  "${PROJECT_BINARY_DIR}/bats_tests/test_helpers.bash"
)

ADD_BATS_TEST(hello_bats)
ADD_BATS_TEST(test_where)
ADD_BATS_TEST(test_select_columns)
ADD_BATS_TEST(test_insert_from_table)
