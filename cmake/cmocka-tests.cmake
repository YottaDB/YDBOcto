macro(ADD_UNIT_TEST_WITH_OPTIONS TEST_NAME TEST_FILE WRAP_FUNCTION)
  set(test_link_flags "")
  if(NOT "${WRAP_FUNCTION}" STREQUAL "")
    set(test_link_flags "-Wl,--wrap=${WRAP_FUNCTION}")
  endif()
  add_executable(${TEST_NAME} ${PROJECT_SOURCE_DIR}/${TEST_FILE}.c
    $<TARGET_OBJECTS:liboctod> $<TARGET_OBJECTS:libocto> $<TARGET_OBJECTS:libhelpers>)

  target_link_libraries(${TEST_NAME}
    ${test_link_flags}
    ${CMOCKA_LIBRARIES}
    ${Readline_LIBRARY}
    ${YOTTADB_LIBRARIES}
  )
  add_test(${TEST_NAME} ${TEST_NAME})
endmacro(ADD_UNIT_TEST_WITH_OPTIONS)

#ADD_UNIT_TEST_WITH_OPTIONS(test_emit_create_table "")
#ADD_UNIT_TEST_WITH_OPTIONS(test_parser_negatives "")
#ADD_UNIT_TEST_WITH_OPTIONS(test_emit_select_statement "")
#ADD_UNIT_TEST_WITH_OPTIONS(test_generate_cursor "")

ADD_UNIT_TEST_WITH_OPTIONS(test_read_bind src/octod/test_read_bind "")
ADD_UNIT_TEST_WITH_OPTIONS(test_read_startup_message src/octod/test_read_startup_message "")
ADD_UNIT_TEST_WITH_OPTIONS(test_read_query src/octod/test_read_query "")
ADD_UNIT_TEST_WITH_OPTIONS(test_make_error_response src/octod/test_make_error_response "")
ADD_UNIT_TEST_WITH_OPTIONS(test_make_bind_complete src/octod/test_make_bind_complete "")
ADD_UNIT_TEST_WITH_OPTIONS(test_make_ready_for_query src/octod/test_make_ready_for_query "")
ADD_UNIT_TEST_WITH_OPTIONS(test_handle_bind src/octod/test_handle_bind "send_message")
ADD_UNIT_TEST_WITH_OPTIONS(test_make_row_description src/octod/test_make_row_description "")
ADD_UNIT_TEST_WITH_OPTIONS(test_make_data_row src/octod/test_make_data_row "")
ADD_UNIT_TEST_WITH_OPTIONS(test_make_command_complete src/octod/test_make_command_complete "")
