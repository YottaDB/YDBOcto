#################################################################
#								#
# Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

macro(ADD_UNIT_TEST_WITH_OPTIONS TEST_NAME TEST_FILE WRAP_FUNCTION)
  set(test_link_flags "")
  foreach(func ${WRAP_FUNCTION})
    if ("${test_link_flags}" STREQUAL "")
      set(test_link_flags "-Wl")
    endif()
    set(test_link_flags "${test_link_flags},--wrap=${func}")
  endforeach(func)
  add_executable(${TEST_NAME} ${PROJECT_SOURCE_DIR}/${TEST_FILE}.c rocto_errors.c rocto_gbldefs.c
    $<TARGET_OBJECTS:librocto> $<TARGET_OBJECTS:libocto> $<TARGET_OBJECTS:libhelpers>)

  set_property(TARGET ${TEST_NAME} PROPERTY C_STANDARD 11)
  target_link_libraries(${TEST_NAME}
    ${test_link_flags}
    ${CMOCKA_LIBRARIES}
    ${Readline_LIBRARY}
    ${YOTTADB_LIBRARIES}
    ${CONFIG_LIBRARY}
    ${OPENSSL_LIBRARIES}
    pthread
  )
  if(${YDB_TLS_AVAILABLE})
    target_link_libraries(${TEST_NAME}
      ydbtls
    )
  endif()
  add_test(${TEST_NAME} ${TEST_NAME})
endmacro(ADD_UNIT_TEST_WITH_OPTIONS)

if("${FULL_TEST_SUITE}" MATCHES "ON")
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_bind src/rocto/test_read_bind "")
	set(functions_to_wrap read_bytes)
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_startup_message src/rocto/test_read_startup_message "${functions_to_wrap}")
	set(functions_to_wrap read_bytes)
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_message src/rocto/test_read_message "${functions_to_wrap}")
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_query src/rocto/test_read_query "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_describe src/rocto/test_read_describe "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_error_response src/rocto/test_make_error_response "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_bind_complete src/rocto/test_make_bind_complete "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_ready_for_query src/rocto/test_make_ready_for_query "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_row_description src/rocto/test_make_row_description "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_data_row src/rocto/test_make_data_row "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_command_complete src/rocto/test_make_command_complete "")
	set(functions_to_wrap send_message run_query)
	ADD_UNIT_TEST_WITH_OPTIONS(test_handle_query src/rocto/test_handle_query "${functions_to_wrap}")
	set(functions_to_wrap syscall)
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_authentication_md5_password src/rocto/test_make_authentication_md5_password "${functions_to_wrap}")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_authentication_ok src/rocto/test_make_authentication_ok "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_parse src/rocto/test_read_parse "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_sync src/rocto/test_read_sync "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_execute src/rocto/test_read_execute "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_parse_complete src/rocto/test_make_parse_complete "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_empty_query_response src/rocto/test_make_empty_query_response "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_parameter_status src/rocto/test_make_parameter_status "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_close src/rocto/test_read_close "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_close_complete src/rocto/test_make_close_complete "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_password_message src/rocto/test_read_password_message "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_portal_suspended src/rocto/test_make_portal_suspended "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_flush src/rocto/test_read_flush "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_generate_routine_name src/test_generate_routine_name "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_deserialize src/rocto/test_deserialize "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_ssl_request src/rocto/test_read_ssl_request "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_get_user_column_value src/rocto/test_get_user_column_value "")
	# This unit test is disabled due to its reliance on a now-fixed bug in octo_init.c.
	# Specifically, this test relied on merge_config_file being unable to find a configuration file in $ydb_dist/plugin/etc,
	# as this location is incorrect. Now it finds a config file in the correct location $ydb_dist/plugin/octo, which causes
	# to go down a code path that relies on octo_log, which is wrapped for the subtest cases, but cannot be wrapped prior to them,
	# leading to a segmentation fault.
	set(functions_to_wrap ydb_get_s get_user_column_value populate_global_names octo_log)
	ADD_UNIT_TEST_WITH_OPTIONS(test_handle_password_message src/rocto/test_handle_password_message "${functions_to_wrap}")
	set(functions_to_wrap read_bytes)
	ADD_UNIT_TEST_WITH_OPTIONS(test_read_cancel_request src/rocto/test_read_cancel_request "${functions_to_wrap}")
	set(functions_to_wrap kill get_pid_start_time)
	ADD_UNIT_TEST_WITH_OPTIONS(test_handle_cancel_request src/rocto/test_handle_cancel_request "${functions_to_wrap}")
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_backend_key_data src/rocto/test_make_backend_key_data "")
	ADD_UNIT_TEST_WITH_OPTIONS(test_get_pid_start_time src/rocto/test_get_pid_start_time "")
	set(functions_to_wrap ydb_subscript_next_s ydb_get_s)
	ADD_UNIT_TEST_WITH_OPTIONS(test_make_parameter_description src/rocto/test_make_parameter_description "${functions_to_wrap}")
	if(${CMAKE_BUILD_TYPE} MATCHES "RelWithDebInfo")
		ADD_UNIT_TEST_WITH_OPTIONS(test_no_asserts src/test_no_asserts "")
	endif()
endif()
