/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

static ErrorResponse *read_error_response(BaseMessage *message) {
	ErrorResponse *ret;
	char *	       cur_pointer, *last_byte;
	uint32_t       remaining_length, num_args, i;

	remaining_length = ntohl(message->length);
	ret = (ErrorResponse *)malloc(remaining_length + sizeof(ErrorResponse) - sizeof(uint32_t));

	ret->type = message->type;
	ret->length = remaining_length;
	remaining_length -= sizeof(uint32_t);
	memcpy(ret->data, message->data, remaining_length);
	cur_pointer = ret->data;
	last_byte = ret->data + remaining_length;

	// Count number of arguments
	num_args = 0;
	while (cur_pointer < last_byte) {
		cur_pointer++; // skip type indicator
		while (cur_pointer != last_byte && '\0' != *cur_pointer) {
			cur_pointer++;
		}
		cur_pointer++; // skip null terminator
		num_args++;
	}

	if (0 == num_args) {
		ret->args = NULL;
		return ret;
	}
	ret->args = (ErrorResponseArg *)malloc(num_args * sizeof(ErrorResponseArg));

	// Populate args with type info and pointers into data section
	cur_pointer = ret->data;
	for (i = 0; i < num_args; i++) {
		ret->args[i].type = *cur_pointer;
		cur_pointer++;
		ret->args[i].value = cur_pointer;
		while (cur_pointer != last_byte && '\0' != *cur_pointer) {
			cur_pointer++;
		}
		cur_pointer++; // skip null terminator
	}

	return ret;
}

static void test_error_with_one_parm(void **state) {
	ErrorResponse *	 received_response = NULL;
	ErrorResponse *	 response = NULL;
	char *		 message = "Seems OK to me man";
	char *		 detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};

	response = make_error_response(PSQL_Error_ERROR, PSQL_Code_Success, message, 1, &a);

	// Expected length is each string + null terminating bytes + response arg type + length + final null byte
	int32_t expected_length = strlen(psql_error_severity_str[PSQL_Error_ERROR])
				  + strlen(psql_sqlstate_codes_str[PSQL_Code_Success]) + strlen(message) + strlen(detail)
				  + (4 * sizeof(char)) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(char);

	received_response = read_error_response((BaseMessage *)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_ErrorResponse);
	assert_int_equal(received_response->length, expected_length);

	free_error_response(response);
	free_error_response(received_response);
}

static void test_error_with_no_additional_parms(void **state) {
	ErrorResponse *	 received_response = NULL;
	ErrorResponse *	 response = NULL;
	char *		 message = "Seems OK to me man";
	char *		 detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};

	response = make_error_response(PSQL_Error_ERROR, PSQL_Code_Success, message, 0);

	// Expected length is each string + null terminating bytes + response arg type + length + final null byte
	int32_t expected_length = strlen(psql_error_severity_str[PSQL_Error_ERROR])
				  + strlen(psql_sqlstate_codes_str[PSQL_Code_Success]) + strlen(message) + (3 * sizeof(char))
				  + (3 * sizeof(char)) + sizeof(uint32_t) + sizeof(char);

	received_response = read_error_response((BaseMessage *)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_ErrorResponse);
	assert_int_equal(received_response->length, expected_length);

	free_error_response(response);
	free_error_response(received_response);
}

static void test_error_with_additional_parms(void **state) {
	ErrorResponse *	 received_response = NULL;
	ErrorResponse *	 response = NULL;
	char *		 message = "Seems OK to me man";
	char *		 detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};

	response = make_error_response(PSQL_Error_ERROR, PSQL_Code_Success, message, 10, &a, &a, &a, &a, &a, &a, &a, &a, &a, &a);

	// Total of 13 items: 3 required, 10 optional. Each includes a null terminator and type code. Include length and type also.
	int32_t expected_length = strlen(psql_error_severity_str[PSQL_Error_ERROR])
				  + strlen(psql_sqlstate_codes_str[PSQL_Code_Success]) + strlen(message) + (10 * strlen(detail))
				  + (13 * sizeof(char)) + (13 * sizeof(char)) + sizeof(uint32_t) + sizeof(char);

	received_response = read_error_response((BaseMessage *)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_ErrorResponse);
	assert_int_equal(received_response->length, expected_length);

	free_error_response(response);
	free_error_response(received_response);
}

static void test_error_verify_args_pointers_correct(void **state) {
	ErrorResponse *	 received_response = NULL;
	ErrorResponse *	 response = NULL;
	char *		 message = "Seems OK to me man";
	char *		 detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};

	response = make_error_response(PSQL_Error_ERROR, PSQL_Code_Success, message, 10, &a, &a, &a, &a, &a, &a, &a, &a, &a, &a);

	// Total of 13 args: 3 required, 10 optional.
	int32_t num_args = 3 + 10;
	// Each arg includes a null terminator and type code. Count length and type of ErrorResponse as usual.
	int32_t expected_length = strlen(psql_error_severity_str[PSQL_Error_ERROR])
				  + strlen(psql_sqlstate_codes_str[PSQL_Code_Success]) + strlen(message) + (10 * strlen(detail))
				  + (num_args * sizeof(char)) + (num_args * sizeof(char)) + sizeof(uint32_t) + sizeof(char);

	received_response = read_error_response((BaseMessage *)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_ErrorResponse);
	assert_int_equal(received_response->length, expected_length);

	int32_t i = 0;
	for (i = 0; i < num_args; i++) {
		assert_non_null(received_response->args[i].value);
		switch (i) {
		case 0:
			assert_int_equal(received_response->args[0].type, PSQL_Error_SEVERITY);
			assert_string_equal(psql_error_severity_str[PSQL_Error_ERROR], received_response->args[0].value);
			break;
		case 1:
			assert_int_equal(received_response->args[1].type, PSQL_Error_Code);
			assert_string_equal(psql_sqlstate_codes_str[PSQL_Code_Success], received_response->args[1].value);
			break;
		case 2:
			assert_int_equal(received_response->args[2].type, PSQL_Error_Message);
			assert_string_equal(message, received_response->args[2].value);
			break;
		default:
			assert_int_equal(received_response->args[i].type, PSQL_Error_Detail);
			assert_string_equal(detail, received_response->args[i].value);
			break;
		}
	}

	free_error_response(response);
	free_error_response(received_response);
}

static void test_notice_with_one_parm(void **state) {
	ErrorResponse *	 received_response = NULL;
	ErrorResponse *	 response = NULL;
	char *		 message = "Seems OK to me man";
	char *		 detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};

	response = make_error_response(PSQL_Error_WARNING, PSQL_Code_Protocol_Violation, message, 1, &a);

	// Expected length is each string + null terminating bytes + response arg type + length + final null byte
	int32_t expected_length = strlen(psql_error_severity_str[PSQL_Error_WARNING])
				  + strlen(psql_sqlstate_codes_str[PSQL_Code_Protocol_Violation]) + strlen(message) + strlen(detail)
				  + (4 * sizeof(char)) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(char);

	received_response = read_error_response((BaseMessage *)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_NoticeResponse);
	assert_int_equal(received_response->length, expected_length);

	free_error_response(response);
	free_error_response(received_response);
}

static void test_notice_with_no_additional_parms(void **state) {
	ErrorResponse *	 received_response = NULL;
	ErrorResponse *	 response = NULL;
	char *		 message = "Seems OK to me man";
	char *		 detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};

	response = make_error_response(PSQL_Error_WARNING, PSQL_Code_Protocol_Violation, message, 0);

	// Expected length is each string + null terminating bytes + response arg type + length + final null byte
	int32_t expected_length = strlen(psql_error_severity_str[PSQL_Error_WARNING])
				  + strlen(psql_sqlstate_codes_str[PSQL_Code_Protocol_Violation]) + strlen(message)
				  + (3 * sizeof(char)) + (3 * sizeof(char)) + sizeof(uint32_t) + sizeof(char);

	received_response = read_error_response((BaseMessage *)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_NoticeResponse);
	assert_int_equal(received_response->length, expected_length);

	free_error_response(response);
	free_error_response(received_response);
}

static void test_notice_with_additional_parms(void **state) {
	ErrorResponse *	 received_response = NULL;
	ErrorResponse *	 response = NULL;
	char *		 message = "Seems OK to me man";
	char *		 detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};

	response = make_error_response(PSQL_Error_WARNING, PSQL_Code_Protocol_Violation, message, 10, &a, &a, &a, &a, &a, &a, &a,
				       &a, &a, &a);

	// Total of 13 items: 3 required, 10 optional. Each includes a null terminator and type code. Include length and type also.
	int32_t expected_length = strlen(psql_error_severity_str[PSQL_Error_WARNING])
				  + strlen(psql_sqlstate_codes_str[PSQL_Code_Protocol_Violation]) + strlen(message)
				  + (10 * strlen(detail)) + (13 * sizeof(char)) + (13 * sizeof(char)) + sizeof(uint32_t)
				  + sizeof(char);

	received_response = read_error_response((BaseMessage *)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_NoticeResponse);
	assert_int_equal(received_response->length, expected_length);

	free_error_response(response);
	free_error_response(received_response);
}

static void test_notice_verify_args_pointers_correct(void **state) {
	ErrorResponse *	 received_response = NULL;
	ErrorResponse *	 response = NULL;
	char *		 message = "Seems OK to me man";
	char *		 detail = "This is a more complicated message";
	ErrorResponseArg a = {PSQL_Error_Detail, detail};

	response = make_error_response(PSQL_Error_WARNING, PSQL_Code_Protocol_Violation, message, 10, &a, &a, &a, &a, &a, &a, &a,
				       &a, &a, &a);

	// Total of 13 args: 3 required, 10 optional.
	int32_t num_args = 3 + 10;
	// Each arg includes a null terminator and type code. Count length and type of ErrorResponse as usual.
	int32_t expected_length = strlen(psql_error_severity_str[PSQL_Error_WARNING])
				  + strlen(psql_sqlstate_codes_str[PSQL_Code_Protocol_Violation]) + strlen(message)
				  + (10 * strlen(detail)) + (num_args * sizeof(char)) + (num_args * sizeof(char)) + sizeof(uint32_t)
				  + sizeof(char);

	received_response = read_error_response((BaseMessage *)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_int_equal(received_response->type, PSQL_NoticeResponse);
	assert_int_equal(received_response->length, expected_length);

	int32_t i = 0;
	for (i = 0; i < num_args; i++) {
		assert_non_null(received_response->args[i].value);
		switch (i) {
		case 0:
			assert_int_equal(received_response->args[0].type, PSQL_Error_SEVERITY);
			assert_string_equal(psql_error_severity_str[PSQL_Error_WARNING], received_response->args[0].value);
			break;
		case 1:
			assert_int_equal(received_response->args[1].type, PSQL_Error_Code);
			assert_string_equal(psql_sqlstate_codes_str[PSQL_Code_Protocol_Violation],
					    received_response->args[1].value);
			break;
		case 2:
			assert_int_equal(received_response->args[2].type, PSQL_Error_Message);
			assert_string_equal(message, received_response->args[2].value);
			break;
		default:
			assert_int_equal(received_response->args[i].type, PSQL_Error_Detail);
			assert_string_equal(detail, received_response->args[i].value);
			break;
		}
	}

	free_error_response(response);
	free_error_response(received_response);
}

int main(void) {
	const struct CMUnitTest tests[]
	    = {cmocka_unit_test(test_error_with_one_parm),	    cmocka_unit_test(test_error_with_no_additional_parms),
	       cmocka_unit_test(test_error_with_additional_parms),  cmocka_unit_test(test_error_verify_args_pointers_correct),
	       cmocka_unit_test(test_notice_with_one_parm),	    cmocka_unit_test(test_notice_with_no_additional_parms),
	       cmocka_unit_test(test_notice_with_additional_parms), cmocka_unit_test(test_notice_verify_args_pointers_correct)};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
