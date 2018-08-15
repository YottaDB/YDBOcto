enable_testing()

add_test(testCreateTables octo --dry-run --input-file ${PROJECT_SOURCE_DIR}/samples/create_table.sql)
