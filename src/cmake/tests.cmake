enable_testing()

add_test(testCreateTables octo --dry-run --input-file ${PROJECT_SOURCE_DIR}/samples/create_table.sql)
add_test(testDeleteValues octo --dry-run --input-file ${PROJECT_SOURCE_DIR}/samples/delete.sql)
add_test(testSelectValues octo --dry-run --input-file ${PROJECT_SOURCE_DIR}/samples/select.sql)
add_test(testInsertValues octo --dry-run --input-file ${PROJECT_SOURCE_DIR}/samples/insert.sql)
add_test(testUpdateValues octo --dry-run --input-file ${PROJECT_SOURCE_DIR}/samples/update.sql)
