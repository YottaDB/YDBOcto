macro(ADD_UNIT_TEST_WITH_OPTIONS TEST_NAME WRAP_FUNCTION)
  set(test_link_flags "")
  if(NOT "${WRAP_FUNCTION}" STREQUAL "")
    set(test_link_flags "--wrap,${WRAP_FUNCTION}")
  endif()
  add_executable(${TEST_NAME} ${PROJECT_SOURCE_DIR}/src/tests/${TEST_NAME}.c $<TARGET_OBJECTS:libocto>)

  target_link_libraries(${TEST_NAME} ${CMOCKA_LIBRARIES} ${test_link_flags})
  add_test(${TEST_NAME} ${TEST_NAME})
endmacro(ADD_UNIT_TEST_WITH_OPTIONS)

ADD_UNIT_TEST_WITH_OPTIONS(test_emit_create_table "")
ADD_UNIT_TEST_WITH_OPTIONS(test_parser_negatives "")
ADD_UNIT_TEST_WITH_OPTIONS(test_emit_select_statement "")