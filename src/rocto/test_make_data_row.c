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
#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"

static void test_null_input(void **state) {
	DataRow *response = NULL;
	DataRow *received_response = NULL;

	int32_t expected_length = sizeof(uint32_t) + sizeof(int16_t);
	response = make_data_row(NULL, 0, NULL);
	received_response = read_data_row((BaseMessage*)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_null(received_response->parms);
	assert_int_equal(received_response->length, expected_length);
	assert_int_equal(received_response->num_columns, 0);

	free(response);
	free_data_row(received_response);
}

static void test_zero_parms(void **state) {
	DataRow *response = NULL;
	DataRow *received_response = NULL;
	int32_t num_parms = 0;
	DataRowParm parms[num_parms];

	// DataRow.length + DataRow.num_columns + DataRowParms (each parm: length + string value)
	int32_t expected_length = sizeof(uint32_t) + sizeof(int16_t) + (sizeof(uint32_t)) * num_parms;

	response = make_data_row(parms, num_parms, NULL);
	received_response = read_data_row((BaseMessage*)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_null(received_response->parms);
	assert_int_equal(received_response->length, expected_length);
	assert_int_equal(received_response->num_columns, 0);

	free(response);
	free_data_row(received_response);
}

static void test_one_text_parm(void **state) {
	DataRow *response = NULL;
	DataRow *received_response = NULL;
	int32_t num_parms = 1, i = 0;
	DataRowParm parms[num_parms];

	memset(parms, 0, sizeof(DataRowParm) * num_parms);
	char *parm_value = "helloWorld";
	int32_t parm_length = strlen(parm_value);
	parms[0].length = parm_length;
	parms[0].value = (char*)malloc(parm_length * sizeof(char));
	strncpy(parms[0].value, parm_value, parm_length);
	// DataRow.length + DataRow.num_columns + DataRowParms (each parm: length + string value)
	int32_t expected_length = sizeof(uint32_t) + sizeof(int16_t) + (sizeof(uint32_t)) * num_parms + parm_length;

	response = make_data_row(parms, num_parms, NULL);
	received_response = read_data_row((BaseMessage*)&response->type);

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

static void test_two_text_parms(void **state) {
	DataRow *response = NULL;
	DataRow *received_response = NULL;
	int32_t num_parms = 2;
	DataRowParm parms[num_parms];
	memset(parms, 0, sizeof(DataRowParm) * num_parms);

	char *parm1_value = "helloWorld";
	int32_t parm1_length = strlen(parm1_value);
	parms[0].length = parm1_length;
	parms[0].value = (char*)malloc(parm1_length * sizeof(char));
	strncpy(parms[0].value, parm1_value, parm1_length);

	char *parm2_value = "helloUniverse";
	int32_t parm2_length = strlen(parm2_value);
	parms[1].length = parm2_length;
	parms[1].value = (char*)malloc(parm2_length * sizeof(char));
	strncpy(parms[1].value, parm2_value, parm2_length);

	// DataRow.length + DataRow.num_columns + DataRowParms (each parm: length + string value)
	int32_t expected_length = sizeof(uint32_t) + sizeof(int16_t)
		+ (sizeof(uint32_t)) * num_parms + parm1_length + parm2_length;

	response = make_data_row(parms, num_parms, NULL);
	received_response = read_data_row((BaseMessage*)&response->type);

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

static void test_three_text_parms(void **state) {
	DataRow *response = NULL;
	DataRow *received_response = NULL;
	int32_t num_parms = 3;
	DataRowParm parms[num_parms];
	memset(parms, 0, sizeof(DataRowParm) * num_parms);

	char *parm1_value = "helloWorld";
	int32_t parm1_length = strlen(parm1_value);
	parms[0].length = parm1_length;
	parms[0].value = (char*)malloc(parm1_length * sizeof(char));
	strncpy(parms[0].value, parm1_value, parm1_length);

	char *parm2_value = "helloUniverse";
	int32_t parm2_length = strlen(parm2_value);
	parms[1].length = parm2_length;
	parms[1].value = (char*)malloc(parm2_length * sizeof(char));
	strncpy(parms[1].value, parm2_value, parm2_length);

	char *parm3_value = "helloThingBiggerThanUniverse";
	int32_t parm3_length = strlen(parm3_value);
	parms[2].length = parm3_length;
	parms[2].value = (char*)malloc(parm3_length * sizeof(char));
	strncpy(parms[2].value, parm3_value, parm3_length);

	// DataRow.length + DataRow.num_columns + DataRowParms (each parm: length + string value)
	int32_t expected_length = sizeof(uint32_t) + sizeof(int16_t)
		+ (sizeof(uint32_t)) * num_parms + parm1_length + parm2_length + parm3_length;

	response = make_data_row(parms, num_parms, NULL);
	received_response = read_data_row((BaseMessage*)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_non_null(received_response->parms);
	assert_int_equal(received_response->length, expected_length);
	assert_int_equal(received_response->num_columns, num_parms);
	assert_int_equal(received_response->parms[0].length, parm1_length);
	assert_memory_equal(received_response->parms[0].value, parm1_value, parm1_length);
	assert_int_equal(received_response->parms[1].length, parm2_length);
	assert_memory_equal(received_response->parms[1].value, parm2_value, parm2_length);
	assert_int_equal(received_response->parms[2].length, parm3_length);
	assert_memory_equal(received_response->parms[2].value, parm3_value, parm3_length);

	free(response);
	free_data_row(received_response);
	free(parms[0].value);
	free(parms[1].value);
	free(parms[2].value);
}

static void test_one_binary_parm(void **state) {
	DataRow *response = NULL;
	DataRow *received_response = NULL;
	int32_t num_parms = 1, i = 0;
	DataRowParm parms[num_parms];

	memset(parms, 0, sizeof(DataRowParm) * num_parms);
	char *parm_value = "12";
	int32_t parm_length = strlen(parm_value);
	parms[0].length = PSQL_TypeSize_int4;
	parms[0].value = (char*)malloc(parms[0].length);
	strncpy(parms[0].value, parm_value, parm_length);
	// DataRow.length + DataRow.num_columns + DataRowParms (each parm: length + string value)
	int32_t expected_length = sizeof(uint32_t) + sizeof(int16_t) + (sizeof(uint32_t)) * num_parms + parms[0].length;

	response = make_data_row(parms, num_parms, NULL);
	received_response = read_data_row((BaseMessage*)&response->type);

	// Standard checks
	assert_non_null(received_response);
	assert_non_null(received_response->parms);
	assert_int_equal(received_response->length, expected_length);
	assert_int_equal(received_response->num_columns, num_parms);
	assert_int_equal(received_response->parms[0].length, parm_length);
	assert_int_equal(ntohl(*(int32_t *)received_response->parms[0].value), 12);

	free(response);
	free_data_row(received_response);
	free(parms[0].value);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_null_input),
		cmocka_unit_test(test_zero_parms),
		cmocka_unit_test(test_one_text_parm),
		cmocka_unit_test(test_two_text_parms),
		cmocka_unit_test(test_three_text_parms)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
