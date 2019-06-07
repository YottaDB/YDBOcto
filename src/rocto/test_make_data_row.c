/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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

static void test_null_input(void **state) {
	DataRow *response = NULL;
	DataRow *received_response = NULL;
	ErrorResponse *err = NULL;

	int expected_length = sizeof(unsigned int) + sizeof(short int);
	response = make_data_row(NULL, 0);
	received_response = read_data_row((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_non_null(received_response);
	assert_null(received_response->parms);
	assert_int_equal(received_response->length, expected_length);
	assert_int_equal(received_response->num_columns, 0);

	free(response);
	free_data_row(received_response);
}

static void test_one_parms(void **state) {
	DataRow *response = NULL;
	DataRow *received_response = NULL;
	ErrorResponse *err = NULL;
	int num_parms = 1, i = 0;
	DataRowParm parms[num_parms];

	memset(parms, 0, sizeof(DataRowParm) * num_parms);
	char *parm_value = "helloWorld";
	int parm_length = strlen(parm_value);
	parms[0].length = parm_length;
	parms[0].value = (char*)malloc(parm_length * sizeof(char));
	strncpy(parms[0].value, parm_value, parm_length);
	// DataRow.length + DataRow.num_columns + DataRowParms (each parm: length + string value)
	int expected_length = sizeof(unsigned int) + sizeof(short int) + (sizeof(unsigned int)) * num_parms + parm_length;

	response = make_data_row(parms, num_parms);
	received_response = read_data_row((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_non_null(received_response);
	assert_non_null(received_response->parms);
	assert_int_equal(received_response->length, expected_length);
	assert_int_equal(received_response->num_columns, num_parms);
	assert_int_equal(received_response->parms[0].length, parm_length);
	assert_memory_equal(received_response->parms[0].value, parm_value, parm_length);

	free(response);
	free_data_row(received_response);
	free(parms[0].value);
}

static void test_multi_parms(void **state) {
	DataRow *response = NULL;
	DataRow *received_response = NULL;
	ErrorResponse *err = NULL;
	int num_parms = 2, i = 0;
	DataRowParm parms[num_parms];
	memset(parms, 0, sizeof(DataRowParm) * num_parms);

	char *parm1_value = "helloWorld";
	int parm1_length = strlen(parm1_value);
	parms[0].length = parm1_length;
	parms[0].value = (char*)malloc(parm1_length * sizeof(char));
	strncpy(parms[0].value, parm1_value, parm1_length);

	char *parm2_value = "helloUniverse";
	int parm2_length = strlen(parm2_value);
	parms[1].length = parm2_length;
	parms[1].value = (char*)malloc(parm2_length * sizeof(char));
	strncpy(parms[1].value, parm2_value, parm2_length);

	// DataRow.length + DataRow.num_columns + DataRowParms (each parm: length + string value)
	int expected_length = sizeof(unsigned int) + sizeof(short int)
		+ (sizeof(unsigned int)) * num_parms + parm1_length + parm2_length;

	response = make_data_row(parms, num_parms);
	received_response = read_data_row((BaseMessage*)&response->type, &err);

	// Standard checks
	assert_non_null(received_response);
	assert_non_null(received_response->parms);
	assert_int_equal(received_response->length, expected_length);
	assert_int_equal(received_response->num_columns, num_parms);
	assert_int_equal(received_response->parms[0].length, parm1_length);
	assert_memory_equal(received_response->parms[0].value, parm1_value, parm1_length);
	assert_int_equal(received_response->parms[1].length, parm2_length);
	assert_memory_equal(received_response->parms[1].value, parm2_value, parm2_length);

	free(response);
	free_data_row(received_response);
	free(parms[0].value);
	free(parms[1].value);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_null_input),
		cmocka_unit_test(test_one_parms),
		cmocka_unit_test(test_multi_parms),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
