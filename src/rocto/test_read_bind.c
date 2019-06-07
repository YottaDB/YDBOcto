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

static void test_valid_input_no_parms(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(0);
	int message_length = sizeof(unsigned int) + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 3;
	ErrorResponse *err = NULL;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(bind);
	assert_string_equal(bind->dest, dest);
	assert_string_equal(bind->source, source);
	assert_int_equal(bind->num_parm_format_codes, ntohs(num_parm_format_codes));
	assert_int_equal(bind->num_parms, ntohs(num_parms));
	assert_int_equal(bind->num_result_col_format_codes, ntohs(num_result_col_format_codes));

	// Checks for this case, since we don't have some values
	assert_null(bind->parm_format_codes);
	assert_null(bind->parms);
	assert_null(bind->result_col_format_codes);
	assert_null(err);

        free(test_data);
        free(bind);
}

static void test_valid_input_one_parm_no_parm_code(void **state) {
	// Test a valid, simple, Bind command
	int message_length = 0;
	message_length += sizeof(unsigned int);		// length

	// Strings
	char *dest = "Hello";
	message_length += strlen(dest) + 1;		// count null
	char *source = "SELECT * FROM names;";
	message_length += strlen(source) + 1;		// count null

	// Format codes
	short int num_parm_format_codes = htons(0);
	message_length += sizeof(short int);

	// Parameters
	short int num_parms = htons(1);
	message_length += sizeof(short int);
	char *parm_value = "parameter";
	BindParm *parm = (BindParm*)malloc(sizeof(BindParm) + strlen(parm_value) + 1);	// count null
	parm->length = htonl(strlen(parm_value) + 1);
	memcpy(&parm->value, parm_value, strlen(parm_value) + 1);
	message_length += sizeof(int) + strlen(parm_value) + 1;

	// Column format codes
	short int num_result_col_format_codes = htons(1);
	message_length += sizeof(short int);
	short int col_formats[1] = {0};			// arbitrary col format code
	message_length += sizeof(col_formats);

	ErrorResponse *err = NULL;
	char *c = NULL;

	// Create and initialize base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	// Copy strings into message
	c = test_data->data;
	memcpy(c, dest, strlen(dest) + 1);		// include null
	c += strlen(dest) + 1;
	memcpy(c, source, strlen(source) + 1);		// include null
	c += strlen(source) + 1;

	// Copy parameter codes into message
	*((short int*)c) = num_parm_format_codes;
	c += sizeof(short int);
	// Copy parameters into message
	*((short int*)c) = num_parms;
	c += sizeof(short int);
	memcpy(c, parm, sizeof(int) + strlen(parm_value) + 1);
	c += sizeof(int) + strlen(parm_value) + 1;
	// Copy column format codes into message
	*((short int*)c) = num_result_col_format_codes;
	c += sizeof(short int);
	memcpy(c, col_formats, sizeof(col_formats));
	c += sizeof(col_formats);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(bind);
	assert_string_equal(bind->dest, dest);
	assert_string_equal(bind->source, source);
	assert_int_equal(bind->num_parm_format_codes, ntohs(num_parm_format_codes));
	assert_int_equal(bind->num_parms, ntohs(num_parms));
	assert_int_equal(bind->num_result_col_format_codes, ntohs(num_result_col_format_codes));

	// Checks for this case, since we don't have some values
	assert_null(bind->parm_format_codes);
	assert_non_null(bind->parms);
	assert_non_null(bind->result_col_format_codes);
	assert_null(err);

        free(test_data);
	free(parm);
        free(bind->parms);
	free(bind);
}

static void test_valid_input_one_parm_one_parm_code(void **state) {
	// Test a valid, simple, Bind command
	int message_length = 0;
	message_length += sizeof(unsigned int);		// length

	// Strings
	char *dest = "Hello";
	message_length += strlen(dest) + 1;		// count null
	char *source = "SELECT * FROM names;";
	message_length += strlen(source) + 1;		// count null

	// Format codes
	short int num_parm_format_codes = htons(1);
	message_length += sizeof(short int);
	short int formats[1] = {0};			// arbitrary format code
	message_length += sizeof(formats);

	// Parameters
	short int num_parms = htons(1);
	message_length += sizeof(short int);
	char *parm_value = "parameter";
	BindParm *parm = (BindParm*)malloc(sizeof(BindParm) + strlen(parm_value) + 1);	// count null
	parm->length = htonl(strlen(parm_value) + 1);
	memcpy(&parm->value, parm_value, strlen(parm_value) + 1);
	message_length += sizeof(int) + strlen(parm_value) + 1;

	// Column format codes
	short int num_result_col_format_codes = htons(1);
	message_length += sizeof(short int);
	short int col_formats[1] = {0};			// arbitrary col format code
	message_length += sizeof(col_formats);

	ErrorResponse *err = NULL;
	char *c = NULL;

	// Create and initialize base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	// Copy strings into message
	c = test_data->data;
	memcpy(c, dest, strlen(dest) + 1);		// include null
	c += strlen(dest) + 1;
	memcpy(c, source, strlen(source) + 1);		// include null
	c += strlen(source) + 1;

	// Copy parameter codes into message
	*((short int*)c) = num_parm_format_codes;
	c += sizeof(short int);
	memcpy(c, formats, sizeof(formats));
	c += sizeof(formats);
	// Copy parameters into message
	*((short int*)c) = num_parms;
	c += sizeof(short int);
	memcpy(c, parm, sizeof(int) + strlen(parm_value) + 1);
	c += sizeof(int) + strlen(parm_value) + 1;
	// Copy column format codes into message
	*((short int*)c) = num_result_col_format_codes;
	c += sizeof(short int);
	memcpy(c, col_formats, sizeof(col_formats));
	c += sizeof(col_formats);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(bind);
	assert_string_equal(bind->dest, dest);
	assert_string_equal(bind->source, source);
	assert_int_equal(bind->num_parm_format_codes, ntohs(num_parm_format_codes));
	assert_int_equal(bind->num_parms, ntohs(num_parms));
	assert_int_equal(bind->num_result_col_format_codes, ntohs(num_result_col_format_codes));

	// Checks for this case, since we don't have some values
	assert_non_null(bind->parm_format_codes);
	assert_non_null(bind->parms);
	assert_non_null(bind->result_col_format_codes);
	assert_null(err);

        free(test_data);
	free(parm);
        free(bind->parms);
	free(bind);
}


static void test_valid_input_multi_parms_default_parm_code(void **state) {
	// Test a valid, simple, Bind command
	int message_length = 0;
	message_length += sizeof(unsigned int);		// length

	// Strings
	char *dest = "Hello";
	message_length += strlen(dest) + 1;		// count null
	char *source = "SELECT * FROM names;";
	message_length += strlen(source) + 1;		// count null

	// Format codes
	short int num_parm_format_codes = htons(0);
	message_length += sizeof(short int);

	// Parameters
	short int num_parms = htons(2);
	message_length += sizeof(short int);
	// First parameter
	char *parm1_value = "parameter1";
	BindParm *parm1 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm1_value) + 1);	// count null
	parm1->length = htonl(strlen(parm1_value) + 1);
	memcpy(&parm1->value, parm1_value, strlen(parm1_value) + 1);
	message_length += sizeof(int) + strlen(parm1_value) + 1;
	// Second parameter
	char *parm2_value = "parameter2";
	BindParm *parm2 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm2_value) + 1);	// count null
	parm2->length = htonl(strlen(parm2_value) + 1);
	memcpy(&parm2->value, parm2_value, strlen(parm2_value) + 1);
	message_length += sizeof(int) + strlen(parm2_value) + 1;

	// Column format codes
	short int num_result_col_format_codes = htons(2);
	message_length += sizeof(short int);
	short int col_formats[2] = {0, 0};			// arbitrary col format code
	message_length += sizeof(col_formats);

	ErrorResponse *err = NULL;
	char *c = NULL;

	// Create and initialize base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	// Copy strings into message
	c = test_data->data;
	memcpy(c, dest, strlen(dest) + 1);		// include null
	c += strlen(dest) + 1;
	memcpy(c, source, strlen(source) + 1);		// include null
	c += strlen(source) + 1;

	// Copy parameter codes into message
	*((short int*)c) = num_parm_format_codes;
	c += sizeof(short int);
	// Copy parameters into message
	*((short int*)c) = num_parms;
	c += sizeof(short int);
	memcpy(c, parm1, sizeof(int) + strlen(parm1_value) + 1);
	c += sizeof(int) + strlen(parm1_value) + 1;
	memcpy(c, parm2, sizeof(int) + strlen(parm2_value) + 1);
	c += sizeof(int) + strlen(parm2_value) + 1;
	// Copy column format codes into message
	*((short int*)c) = num_result_col_format_codes;
	c += sizeof(short int);
	memcpy(c, col_formats, sizeof(col_formats));
	c += sizeof(col_formats);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(bind);
	assert_string_equal(bind->dest, dest);
	assert_string_equal(bind->source, source);
	assert_int_equal(bind->num_parm_format_codes, ntohs(num_parm_format_codes));
	assert_int_equal(bind->num_parms, ntohs(num_parms));
	assert_int_equal(bind->num_result_col_format_codes, ntohs(num_result_col_format_codes));

	// Checks for this case, since we don't have some values
	assert_null(bind->parm_format_codes);
	assert_non_null(bind->parms);
	assert_non_null(bind->result_col_format_codes);
	assert_null(err);

        free(test_data);
	free(parm1);
	free(parm2);
        free(bind->parms);
	free(bind);
}


static void test_valid_input_multi_parms_one_parm_code(void **state) {
	// Test a valid, simple, Bind command
	int message_length = 0;
	message_length += sizeof(unsigned int);		// length

	// Strings
	char *dest = "Hello";
	message_length += strlen(dest) + 1;		// count null
	char *source = "SELECT * FROM names;";
	message_length += strlen(source) + 1;		// count null

	// Format codes
	short int num_parm_format_codes = htons(1);
	message_length += sizeof(short int);
	short int formats[1] = {0};			// arbitrary format code
	message_length += sizeof(formats);

	// Parameters
	short int num_parms = htons(2);
	message_length += sizeof(short int);
	// First parameter
	char *parm1_value = "parameter1";
	BindParm *parm1 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm1_value) + 1);	// count null
	parm1->length = htonl(strlen(parm1_value) + 1);
	memcpy(&parm1->value, parm1_value, strlen(parm1_value) + 1);
	message_length += sizeof(int) + strlen(parm1_value) + 1;
	// Second parameter
	char *parm2_value = "parameter2";
	BindParm *parm2 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm2_value) + 1);	// count null
	parm2->length = htonl(strlen(parm2_value) + 1);
	memcpy(&parm2->value, parm2_value, strlen(parm2_value) + 1);
	message_length += sizeof(int) + strlen(parm2_value) + 1;

	// Column format codes
	short int num_result_col_format_codes = htons(2);
	message_length += sizeof(short int);
	short int col_formats[2] = {0, 0};			// arbitrary col format code
	message_length += sizeof(col_formats);

	ErrorResponse *err = NULL;
	char *c = NULL;

	// Create and initialize base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	// Copy strings into message
	c = test_data->data;
	memcpy(c, dest, strlen(dest) + 1);		// include null
	c += strlen(dest) + 1;
	memcpy(c, source, strlen(source) + 1);		// include null
	c += strlen(source) + 1;

	// Copy parameter codes into message
	*((short int*)c) = num_parm_format_codes;
	c += sizeof(short int);
	memcpy(c, formats, sizeof(formats));
	c += sizeof(formats);
	// Copy parameters into message
	*((short int*)c) = num_parms;
	c += sizeof(short int);
	memcpy(c, parm1, sizeof(int) + strlen(parm1_value) + 1);
	c += sizeof(int) + strlen(parm1_value) + 1;
	memcpy(c, parm2, sizeof(int) + strlen(parm2_value) + 1);
	c += sizeof(int) + strlen(parm2_value) + 1;
	// Copy column format codes into message
	*((short int*)c) = num_result_col_format_codes;
	c += sizeof(short int);
	memcpy(c, col_formats, sizeof(col_formats));
	c += sizeof(col_formats);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(bind);
	assert_string_equal(bind->dest, dest);
	assert_string_equal(bind->source, source);
	assert_int_equal(bind->num_parm_format_codes, ntohs(num_parm_format_codes));
	assert_int_equal(bind->num_parms, ntohs(num_parms));
	assert_int_equal(bind->num_result_col_format_codes, ntohs(num_result_col_format_codes));

	// Checks for this case, since we don't have some values
	assert_non_null(bind->parm_format_codes);
	assert_non_null(bind->parms);
	assert_non_null(bind->result_col_format_codes);
	assert_null(err);

        free(test_data);
	free(parm1);
	free(parm2);
        free(bind->parms);
	free(bind);
}

static void test_valid_input_multi_parms_actual_parm_codes(void **state) {
	// Test a valid, simple, Bind command
	int message_length = 0;
	message_length += sizeof(unsigned int);		// length

	// Strings
	char *dest = "Hello";
	message_length += strlen(dest) + 1;		// count null
	char *source = "SELECT * FROM names;";
	message_length += strlen(source) + 1;		// count null

	// Format codes
	short int num_parm_format_codes = htons(2);
	message_length += sizeof(short int);
	short int formats[2] = {0, 0};			// arbitrary format code
	message_length += sizeof(formats);

	// Parameters
	short int num_parms = htons(2);
	message_length += sizeof(short int);
	// First parameter
	char *parm1_value = "parameter1";
	BindParm *parm1 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm1_value) + 1);	// count null
	parm1->length = htonl(strlen(parm1_value) + 1);
	memcpy(&parm1->value, parm1_value, strlen(parm1_value) + 1);
	message_length += sizeof(int) + strlen(parm1_value) + 1;
	// Second parameter
	char *parm2_value = "parameter2";
	BindParm *parm2 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm2_value) + 1);	// count null
	parm2->length = htonl(strlen(parm2_value) + 1);
	memcpy(&parm2->value, parm2_value, strlen(parm2_value) + 1);
	message_length += sizeof(int) + strlen(parm2_value) + 1;

	// Column format codes
	short int num_result_col_format_codes = htons(2);
	message_length += sizeof(short int);
	short int col_formats[2] = {0, 0};			// arbitrary col format code
	message_length += sizeof(col_formats);

	ErrorResponse *err = NULL;
	char *c = NULL;

	// Create and initialize base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	// Copy strings into message
	c = test_data->data;
	memcpy(c, dest, strlen(dest) + 1);		// include null
	c += strlen(dest) + 1;
	memcpy(c, source, strlen(source) + 1);		// include null
	c += strlen(source) + 1;

	// Copy parameter codes into message
	*((short int*)c) = num_parm_format_codes;
	c += sizeof(short int);
	memcpy(c, formats, sizeof(formats));
	c += sizeof(formats);
	// Copy parameters into message
	*((short int*)c) = num_parms;
	c += sizeof(short int);
	memcpy(c, parm1, sizeof(int) + strlen(parm1_value) + 1);
	c += sizeof(int) + strlen(parm1_value) + 1;
	memcpy(c, parm2, sizeof(int) + strlen(parm2_value) + 1);
	c += sizeof(int) + strlen(parm2_value) + 1;
	// Copy column format codes into message
	*((short int*)c) = num_result_col_format_codes;
	c += sizeof(short int);
	memcpy(c, col_formats, sizeof(col_formats));
	c += sizeof(col_formats);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(bind);
	assert_string_equal(bind->dest, dest);
	assert_string_equal(bind->source, source);
	assert_int_equal(bind->num_parm_format_codes, ntohs(num_parm_format_codes));
	assert_int_equal(bind->num_parms, ntohs(num_parms));
	assert_int_equal(bind->num_result_col_format_codes, ntohs(num_result_col_format_codes));

	// Checks for this case, since we don't have some values
	assert_non_null(bind->parm_format_codes);
	assert_non_null(bind->parms);
	assert_non_null(bind->result_col_format_codes);
	assert_null(err);

        free(test_data);
	free(parm1);
	free(parm2);
        free(bind->parms);
	free(bind);
}

static void test_too_many_parameters(void **state) {
	int message_length = 0;
	message_length += sizeof(unsigned int);		// length

	// Strings
	char *dest = "Hello";
	message_length += strlen(dest) + 1;		// count null
	char *source = "SELECT * FROM names;";
	message_length += strlen(source) + 1;		// count null

	// Format codes
	short int num_parm_format_codes = htons(0);
	message_length += sizeof(short int);

	// Parameters
	short int num_parms = htons(2);
	message_length += sizeof(short int);
	// First parameter
	char *parm1_value = "parameter1";
	BindParm *parm1 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm1_value) + 1);	// count null
	parm1->length = htonl(strlen(parm1_value) + 1);
	memcpy(&parm1->value, parm1_value, strlen(parm1_value) + 1);
	message_length += sizeof(int) + strlen(parm1_value) + 1;
	// Second parameter
	char *parm2_value = "parameter2";
	BindParm *parm2 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm2_value) + 1);	// count null
	parm2->length = htonl(strlen(parm2_value) + 1);
	memcpy(&parm2->value, parm2_value, strlen(parm2_value) + 1);
	message_length += sizeof(int) + strlen(parm2_value) + 1;
	// Third parameter
	char *parm3_value = "parameter3";
	BindParm *parm3 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm3_value) + 1);	// count null
	parm3->length = htonl(strlen(parm2_value) + 1);
	memcpy(&parm3->value, parm3_value, strlen(parm3_value) + 1);
	message_length += sizeof(int) + strlen(parm3_value) + 1;

	// Column format codes
	short int num_result_col_format_codes = htons(2);
	message_length += sizeof(short int);
	short int col_formats[2] = {0, 0};			// arbitrary col format code
	message_length += sizeof(col_formats);

	ErrorResponse *err = NULL;
	char *c = NULL;

	// Create and initialize base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	// Copy strings into message
	c = test_data->data;
	memcpy(c, dest, strlen(dest) + 1);		// include null
	c += strlen(dest) + 1;
	memcpy(c, source, strlen(source) + 1);		// include null
	c += strlen(source) + 1;

	// Copy parameter codes into message
	*((short int*)c) = num_parm_format_codes;
	c += sizeof(short int);
	// Copy parameters into message
	*((short int*)c) = num_parms;
	c += sizeof(short int);
	memcpy(c, parm1, sizeof(int) + strlen(parm1_value) + 1);
	c += sizeof(int) + strlen(parm1_value) + 1;
	memcpy(c, parm2, sizeof(int) + strlen(parm2_value) + 1);
	c += sizeof(int) + strlen(parm2_value) + 1;
	memcpy(c, parm3, sizeof(int) + strlen(parm3_value) + 1);
	c += sizeof(int) + strlen(parm3_value) + 1;
	// Copy column format codes into message
	*((short int*)c) = num_result_col_format_codes;
	c += sizeof(short int);
	memcpy(c, col_formats, sizeof(col_formats));
	c += sizeof(col_formats);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(err);
	assert_non_null(bind);		// May contain junk, but this is acceptable
	assert_string_equal(bind->dest, dest);
	assert_string_equal(bind->source, source);
	assert_int_equal(bind->num_parm_format_codes, ntohs(num_parm_format_codes));
	assert_int_equal(bind->num_parms, ntohs(num_parms));
	assert_non_null(bind->parms);
	// Ignore bad result_col_format_codes data, since it may be junk

        free(test_data);
	free(parm1);
	free(parm2);
	free(parm3);
	free(bind->parms);
	free(bind);
	free_error_response(err);
}

static void test_no_parms_with_too_many_num_parm_codes(void **state) {
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(1);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(0);
	int message_length = sizeof(unsigned int) + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 3;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(bind);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_TOO_MANY_VALUES, "Bind", "parameter format codes");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free_error_response(err);
}

static void test_one_parm_with_too_many_num_parm_codes(void **state) {
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	int message_length = 0;
	message_length += sizeof(unsigned int);		// length

	// Strings
	char *dest = "Hello";
	message_length += strlen(dest) + 1;		// count null
	char *source = "SELECT * FROM names;";
	message_length += strlen(source) + 1;		// count null

	// Format codes
	short int num_parm_format_codes = htons(2);
	message_length += sizeof(short int);
	short int formats[2] = {0, 0};			// arbitrary format code
	message_length += sizeof(formats);

	// Parameters
	short int num_parms = htons(1);
	message_length += sizeof(short int);
	char *parm_value = "parameter";
	BindParm *parm = (BindParm*)malloc(sizeof(BindParm) + strlen(parm_value) + 1);	// count null
	parm->length = htonl(strlen(parm_value) + 1);
	memcpy(&parm->value, parm_value, strlen(parm_value) + 1);
	message_length += sizeof(int) + strlen(parm_value) + 1;

	// Column format codes
	short int num_result_col_format_codes = htons(1);
	message_length += sizeof(short int);
	short int col_formats[1] = {0};			// arbitrary col format code
	message_length += sizeof(col_formats);

	ErrorResponse *err = NULL;
	char *c = NULL;

	// Create and initialize base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	// Copy strings into message
	c = test_data->data;
	memcpy(c, dest, strlen(dest) + 1);		// include null
	c += strlen(dest) + 1;
	memcpy(c, source, strlen(source) + 1);		// include null
	c += strlen(source) + 1;

	// Copy parameter codes into message
	*((short int*)c) = num_parm_format_codes;
	c += sizeof(short int);
	memcpy(c, formats, sizeof(formats));
	c += sizeof(formats);
	// Copy parameters into message
	*((short int*)c) = num_parms;
	c += sizeof(short int);
	memcpy(c, parm, sizeof(int) + strlen(parm_value) + 1);
	c += sizeof(int) + strlen(parm_value) + 1;
	// Copy column format codes into message
	*((short int*)c) = num_result_col_format_codes;
	c += sizeof(short int);
	memcpy(c, col_formats, sizeof(col_formats));
	c += sizeof(col_formats);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_null(bind);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_TOO_MANY_VALUES, "Bind", "parameter format codes");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
	free(parm);
	free_error_response(err);
}


static void test_multi_parms_with_too_many_num_parm_codes(void **state) {
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	int message_length = 0;
	message_length += sizeof(unsigned int);		// length

	// Strings
	char *dest = "Hello";
	message_length += strlen(dest) + 1;		// count null
	char *source = "SELECT * FROM names;";
	message_length += strlen(source) + 1;		// count null

	// Format codes
	short int num_parm_format_codes = htons(3);
	message_length += sizeof(short int);
	short int formats[3] = {0, 0, 0};		// arbitrary format code
	message_length += sizeof(formats);

	// Parameters
	short int num_parms = htons(2);
	message_length += sizeof(short int);
	// First parameter
	char *parm1_value = "parameter1";
	BindParm *parm1 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm1_value) + 1);	// count null
	parm1->length = htonl(strlen(parm1_value) + 1);
	memcpy(&parm1->value, parm1_value, strlen(parm1_value) + 1);
	message_length += sizeof(int) + strlen(parm1_value) + 1;
	// Second parameter
	char *parm2_value = "parameter2";
	BindParm *parm2 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm2_value) + 1);	// count null
	parm2->length = htonl(strlen(parm2_value) + 1);
	memcpy(&parm2->value, parm2_value, strlen(parm2_value) + 1);
	message_length += sizeof(int) + strlen(parm2_value) + 1;

	// Column format codes
	short int num_result_col_format_codes = htons(2);
	message_length += sizeof(short int);
	short int col_formats[2] = {0, 0};			// arbitrary col format code
	message_length += sizeof(col_formats);

	ErrorResponse *err = NULL;
	char *c = NULL;

	// Create and initialize base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	// Copy strings into message
	c = test_data->data;
	memcpy(c, dest, strlen(dest) + 1);		// include null
	c += strlen(dest) + 1;
	memcpy(c, source, strlen(source) + 1);		// include null
	c += strlen(source) + 1;

	// Copy parameter codes into message
	*((short int*)c) = num_parm_format_codes;
	c += sizeof(short int);
	memcpy(c, formats, sizeof(formats));
	c += sizeof(formats);
	// Copy parameters into message
	*((short int*)c) = num_parms;
	c += sizeof(short int);
	memcpy(c, parm1, sizeof(int) + strlen(parm1_value) + 1);
	c += sizeof(int) + strlen(parm1_value) + 1;
	memcpy(c, parm2, sizeof(int) + strlen(parm2_value) + 1);
	c += sizeof(int) + strlen(parm2_value) + 1;
	// Copy column format codes into message
	*((short int*)c) = num_result_col_format_codes;
	c += sizeof(short int);
	memcpy(c, col_formats, sizeof(col_formats));
	c += sizeof(col_formats);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(bind);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_TOO_MANY_VALUES, "Bind", "parameter format codes");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
	free(parm1);
	free(parm2);
	free_error_response(err);
}

static void test_multi_parms_with_too_few_num_parm_codes(void **state) {
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	int message_length = 0;
	message_length += sizeof(unsigned int);		// length

	// Strings
	char *dest = "Hello";
	message_length += strlen(dest) + 1;		// count null
	char *source = "SELECT * FROM names;";
	message_length += strlen(source) + 1;		// count null

	// Format codes
	short int num_parm_format_codes = htons(2);
	message_length += sizeof(short int);
	short int formats[2] = {0, 0};			// arbitrary col format code
	message_length += sizeof(formats);

	// Parameters
	short int num_parms = htons(3);
	message_length += sizeof(short int);
	// First parameter
	char *parm1_value = "parameter1";
	BindParm *parm1 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm1_value) + 1);	// count null
	parm1->length = htonl(strlen(parm1_value) + 1);
	memcpy(&parm1->value, parm1_value, strlen(parm1_value) + 1);
	message_length += sizeof(int) + strlen(parm1_value) + 1;
	// Second parameter
	char *parm2_value = "parameter2";
	BindParm *parm2 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm2_value) + 1);	// count null
	parm2->length = htonl(strlen(parm2_value) + 1);
	memcpy(&parm2->value, parm2_value, strlen(parm2_value) + 1);
	message_length += sizeof(int) + strlen(parm2_value) + 1;
	// Third parameter
	char *parm3_value = "parameter3";
	BindParm *parm3 = (BindParm*)malloc(sizeof(BindParm) + strlen(parm3_value) + 1);	// count null
	parm3->length = htonl(strlen(parm2_value) + 1);
	memcpy(&parm3->value, parm3_value, strlen(parm3_value) + 1);
	message_length += sizeof(int) + strlen(parm3_value) + 1;

	// Column format codes
	short int num_result_col_format_codes = htons(2);
	message_length += sizeof(short int);
	short int col_formats[2] = {0, 0};			// arbitrary col format code
	message_length += sizeof(col_formats);

	ErrorResponse *err = NULL;
	char *c = NULL;

	// Create and initialize base message
	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	// Copy strings into message
	c = test_data->data;
	memcpy(c, dest, strlen(dest) + 1);		// include null
	c += strlen(dest) + 1;
	memcpy(c, source, strlen(source) + 1);		// include null
	c += strlen(source) + 1;

	// Copy parameter codes into message
	*((short int*)c) = num_parm_format_codes;
	c += sizeof(short int);
	memcpy(c, formats, sizeof(formats));
	c += sizeof(formats);
	// Copy parameters into message
	*((short int*)c) = num_parms;
	c += sizeof(short int);
	memcpy(c, parm1, sizeof(int) + strlen(parm1_value) + 1);
	c += sizeof(int) + strlen(parm1_value) + 1;
	memcpy(c, parm2, sizeof(int) + strlen(parm2_value) + 1);
	c += sizeof(int) + strlen(parm2_value) + 1;
	memcpy(c, parm3, sizeof(int) + strlen(parm3_value) + 1);
	c += sizeof(int) + strlen(parm3_value) + 1;
	// Copy column format codes into message
	*((short int*)c) = num_result_col_format_codes;
	c += sizeof(short int);
	memcpy(c, col_formats, sizeof(col_formats));
	c += sizeof(col_formats);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(bind);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_TOO_FEW_VALUES, "Bind", "parameter format codes");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
	free(parm1);
	free(parm2);
	free(parm3);
	free_error_response(err);
}

static void test_input_too_short(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(0);
	int message_length = sizeof(unsigned int) + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 3;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length/2);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure correct error message returned
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "Bind", "source");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free(bind);
	free_error_response(err);
}

static void test_input_too_long(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(0);
	// Add 50 to make the input too long
	int message_length = sizeof(unsigned int) + strlen(dest) + strlen(source) + 2  + sizeof(short int) * 3 + 50;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(bind);
	assert_string_equal(bind->dest, dest);
	assert_string_equal(bind->source, source);
	assert_int_equal(bind->num_parm_format_codes, ntohs(num_parm_format_codes));
	assert_int_equal(bind->num_parms, ntohs(num_parms));
	assert_int_equal(bind->num_result_col_format_codes, ntohs(num_result_col_format_codes));

	// Checks for this case, since we don't have some values
	assert_null(bind->parm_format_codes);
	assert_null(bind->parms);
	assert_null(bind->result_col_format_codes);

	// Verify an error was issued with correct error message
	assert_non_null(err);
	error_message = format_error_string(&err_buff, ERR_ROCTO_TRAILING_CHARS, "Bind");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free(bind);
	free_error_response(err);
}

static void test_no_null_terminators_on_dest(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	int message_length = sizeof(unsigned int) + strlen(dest);
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	memset(test_data, 0xdeadbeef, message_length + 0);
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure the return error mentions "dest"
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "Bind", "destination");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free(bind);
	free_error_response(err);
}

static void test_no_null_terminators_on_source(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	int message_length = sizeof(unsigned int) + strlen(dest) + 1 + strlen(source);
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	memset(test_data, 0xdeadbeef, message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "Bind", "source");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free(bind);
	free_error_response(err);
}

static void test_unexpected_null_terminator_on_source(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM nam\0es;";
	// Count extra chars and nulls in source
	int message_length = sizeof(unsigned int) + strlen(dest) + 1 + strlen(source) + 5;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	memset(test_data, 0xdeadbeef, message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "Bind", "source");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free(bind);
	free_error_response(err);
}

static void test_unexpected_null_terminator_on_dest(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hel\0lo";
	int message_length = sizeof(unsigned int) + strlen(dest) + 4;		// count extra chars and nulls
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	memset(test_data, 0xdeadbeef, message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest) + 4);		// count extra chars and nulls

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "Bind", "number of parameter format codes");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free(bind);
	free_error_response(err);
}


static void test_missing_parameter_types(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(10);
	int message_length = sizeof(unsigned int) + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 1;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "Bind", "parameter format codes");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free(bind);
	free_error_response(err);
}

static void test_missing_parameters(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(10);
	int message_length = sizeof(unsigned int) + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 2;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "Bind", "parameters");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free(bind);
	free_error_response(err);
}

static void test_missing_result_col_format_codes(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(10);
	int message_length = sizeof(unsigned int) + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 3;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	assert_null(bind);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_DATA, "Bind", "result column format codes");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free(bind);
	free_error_response(err);
}

static void test_invalid_type(void **state) {
	// Test a valid, simple, Bind command
	char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(0);
	int message_length = sizeof(unsigned int) + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 3;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = 'X';
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(bind);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_TYPE, "Bind", test_data->type, PSQL_Bind);
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free_error_response(err);
}

static void test_invalid_num_parm_format_codes(void **state) {
	// Test a valid, simple, Bind command char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(-1);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(0);
	int message_length = sizeof(unsigned int) + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 3;
	char *ptr = NULL;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(bind);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_NUMBER, "Bind", "parameter format codes");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free_error_response(err);
}

static void test_invalid_num_parms(void **state) {
	// Test a valid, simple, Bind command char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(-1);
	short int num_result_col_format_codes = htons(0);
	int message_length = sizeof(unsigned int) + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 3;
	char *ptr = NULL;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(bind);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_NUMBER, "Bind", "parameters");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free_error_response(err);
}

static void test_invalid_num_col_format_codes(void **state) {
	// Test a valid, simple, Bind command char *ptr = NULL;
	char *dest = "Hello";
	char *source = "SELECT * FROM names;";
	short int num_parm_format_codes = htons(0);
	short int num_parms = htons(0);
	short int num_result_col_format_codes = htons(-1);
	int message_length = sizeof(unsigned int) + strlen(dest) + strlen(source) + 2 + sizeof(short int) * 3;
	char *ptr = NULL;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	BaseMessage *test_data = (BaseMessage*)malloc(message_length + sizeof(BaseMessage) - sizeof(unsigned int));
	test_data->type = PSQL_Bind;
	test_data->length = htonl(message_length);
	ptr = test_data->data;
	memcpy(ptr, dest, strlen(dest));
	ptr += strlen(dest);
	*ptr++ = '\0';
	memcpy(ptr, source, strlen(source));
	ptr += strlen(source);
	*ptr++ = '\0';
	*((short int*)ptr) = num_parm_format_codes;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_parms;
	ptr += sizeof(short int);
	*((short int*)ptr) = num_result_col_format_codes;
	ptr += sizeof(short int);

	// The actual test
	Bind *bind = read_bind(test_data, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(bind);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_NUMBER, "Bind", "result column format codes");
	assert_string_equal(error_message, err->args[2].value + 1);

        free(test_data);
        free_error_response(err);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input_no_parms),
		cmocka_unit_test(test_valid_input_one_parm_no_parm_code),
		cmocka_unit_test(test_valid_input_one_parm_one_parm_code),
		cmocka_unit_test(test_valid_input_multi_parms_default_parm_code),
		cmocka_unit_test(test_valid_input_multi_parms_one_parm_code),
		cmocka_unit_test(test_valid_input_multi_parms_actual_parm_codes),
		cmocka_unit_test(test_too_many_parameters),
		cmocka_unit_test(test_no_parms_with_too_many_num_parm_codes),
		cmocka_unit_test(test_one_parm_with_too_many_num_parm_codes),
		cmocka_unit_test(test_multi_parms_with_too_few_num_parm_codes),
		cmocka_unit_test(test_multi_parms_with_too_many_num_parm_codes),
		cmocka_unit_test(test_input_too_short),
		cmocka_unit_test(test_input_too_long),
		cmocka_unit_test(test_no_null_terminators_on_dest),
		cmocka_unit_test(test_no_null_terminators_on_source),
		cmocka_unit_test(test_unexpected_null_terminator_on_dest),
		cmocka_unit_test(test_unexpected_null_terminator_on_source),
		cmocka_unit_test(test_missing_parameter_types),
		cmocka_unit_test(test_missing_parameters),
		cmocka_unit_test(test_missing_result_col_format_codes),
		cmocka_unit_test(test_invalid_type),
		cmocka_unit_test(test_invalid_num_parm_format_codes),
		cmocka_unit_test(test_invalid_num_parms),
		cmocka_unit_test(test_invalid_num_col_format_codes),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
