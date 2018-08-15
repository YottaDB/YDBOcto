enable_testing()

add_test(testCreateTables octo --input-file ${PROJECT_SOURCE_DIR}/samples/create_table.sql)
