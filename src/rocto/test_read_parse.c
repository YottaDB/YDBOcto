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

static void test_valid_input_with_parms(void **state) {
	uint32_t message_len = 0;
	message_len += sizeof(uint32_t);
	char *dest = "a";
	message_len += strlen(dest) + 1;
	char *query = "SELECT * FROM names;";
	message_len += strlen(query) + 1;
	int16_t type_params = 5;
	message_len += sizeof(int16_t);
	int32_t parm_data_types[5] = {1, 2, 3, 4, 5};
	message_len += sizeof(parm_data_types);

	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage) + message_len);
	memset(test_data, 0, sizeof(BaseMessage) + message_len);
	char *c = test_data->data;
	test_data->type = PSQL_Parse;
	test_data->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);
	*c++ = '\0';
	memcpy(c, query, strlen(query));
	c += strlen(query);
	*c++ = '\0';
	*((int16_t *)c) = htons(type_params);
	c += sizeof(int16_t);
	memcpy(c, parm_data_types, sizeof(parm_data_types));

	Parse *parse = read_parse(test_data);

	assert_non_null(parse);
	assert_string_equal(parse->dest, dest);
	assert_string_equal(parse->query, query);
	assert_int_equal(parse->num_parm_data_types, type_params);

	free(test_data);
	free(parse);
}

static void test_valid_input_without_parms(void **state) {
	uint32_t message_len = 0;
	message_len += sizeof(uint32_t);
	char *dest = "a";
	message_len += strlen(dest) + 1;
	char *query = "SELECT * FROM names;";
	message_len += strlen(query) + 1;
	int16_t type_params = 0;
	message_len += sizeof(int16_t);

	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage) + message_len);
	memset(test_data, 0, sizeof(BaseMessage) + message_len);
	char *c = test_data->data;
	test_data->type = PSQL_Parse;
	test_data->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);
	*c++ = '\0';
	memcpy(c, query, strlen(query));
	c += strlen(query);
	*c++ = '\0';
	*((int16_t *)c) = htons(type_params);

	Parse *parse = read_parse(test_data);

	assert_non_null(parse);
	assert_string_equal(parse->dest, dest);
	assert_string_equal(parse->query, query);
	assert_int_equal(parse->num_parm_data_types, type_params);

	free(test_data);
	free(parse);
}

static void test_non_terminated_dest(void **state) {
	uint32_t message_len = 0;
	message_len += sizeof(uint32_t);
	char *dest = "a saosadfkasdfjkasd fwearf asdfkds f";
	message_len += strlen(dest) - 1;

	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage) + message_len);
	memset(test_data, 0, sizeof(BaseMessage) + message_len);
	char *c = test_data->data;
	test_data->type = PSQL_Parse;
	test_data->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);

	Parse *parse = read_parse(test_data);

	assert_null(parse);

	free(test_data);
}

static void test_non_terminated_query(void **state) {
	uint32_t message_len = 0;
	message_len += sizeof(uint32_t);
	char *dest = "a";
	message_len += strlen(dest) + 1;
	char *query = "SELECT * FROM names;";
	message_len += strlen(query) - 1;

	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage) + message_len);
	memset(test_data, 0, sizeof(BaseMessage) + message_len);
	char *c = test_data->data;
	test_data->type = PSQL_Parse;
	test_data->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);
	*c++ = '\0';
	memcpy(c, query, strlen(query));
	c += strlen(query);

	Parse *parse = read_parse(test_data);

	assert_null(parse);

	free(test_data);
}

static void test_unexpectedly_terminated_dest(void **state) {
	uint32_t message_len = 0;
	message_len += sizeof(uint32_t);
	char *dest = "1234\0 1234";
	message_len += strlen(dest) + 5 + 2; // count remaining chars + nulls
	char *query = "SELECT * FROM names;";
	message_len += strlen(query) + 1; // count null

	BaseMessage *test_data = (BaseMessage *)malloc(message_len + sizeof(BaseMessage) - sizeof(uint32_t));
	memset(test_data, 0, message_len + sizeof(BaseMessage) - sizeof(uint32_t));
	char *c = test_data->data;
	test_data->type = PSQL_Parse;
	test_data->length = htonl(message_len);
	memcpy(c, dest, message_len - sizeof(uint32_t) - strlen(query) - 1);
	c += message_len - sizeof(uint32_t) - strlen(query) - 1;
	memcpy(c, query, strlen(query) + 1);

	Parse *parse = read_parse(test_data);

	assert_null(parse);

	free(test_data);
}

static void test_unexpectedly_terminated_query(void **state) {
	uint32_t message_len = 0;
	message_len += sizeof(uint32_t);
	char *dest = "a";
	message_len += strlen(dest) + 1;
	char *query = "SELECT * FROM\0 names;";
	message_len += strlen(query) + 7 + 2; // count remaining chars + nulls

	BaseMessage *test_data = (BaseMessage *)malloc(message_len + sizeof(BaseMessage) - sizeof(uint32_t));
	memset(test_data, 0, message_len + sizeof(BaseMessage) - sizeof(uint32_t));
	char *c = test_data->data;
	test_data->type = PSQL_Parse;
	test_data->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);
	*c++ = '\0';
	memcpy(c, query, message_len - sizeof(uint32_t) - strlen(dest) - 1);

	Parse *parse = read_parse(test_data);

	assert_null(parse);

	free(test_data);
}

static void test_missing_dest_or_query(void **state) {
	uint32_t message_len = 0;
	message_len += sizeof(uint32_t);
	char *dest = "Laputa";
	message_len += strlen(dest) + 1; // count remaining chars + null

	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage) + message_len);
	memset(test_data, 0, sizeof(BaseMessage) + message_len);
	char *c = test_data->data;
	test_data->type = PSQL_Parse;
	test_data->length = htonl(message_len);
	memcpy(c, dest, message_len - sizeof(uint32_t));

	Parse *parse = read_parse(test_data);

	assert_null(parse);

	free(test_data);
}

static void test_missing_num_parm_data_types(void **state) {
	uint32_t message_len = 0;
	message_len += sizeof(uint32_t);
	char *dest = "Laputa";
	message_len += strlen(dest) + 1; // count remaining chars + null
	char *query = "FROM * SELECT names";
	message_len += strlen(query) + 1; // count remaining chars + null

	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage) + message_len);
	memset(test_data, 0, sizeof(BaseMessage) + message_len);
	char *c = test_data->data;
	test_data->type = PSQL_Parse;
	test_data->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);
	*c++ = '\0';
	memcpy(c, query, strlen(query));
	c += strlen(query);
	*c++ = '\0';

	Parse *parse = read_parse(test_data);

	assert_null(parse);

	free(test_data);
}

static void test_too_many_parms(void **state) {
	uint32_t message_len = 0;
	message_len += sizeof(uint32_t);
	char *dest = "a";
	message_len += strlen(dest) + 1;
	char *query = "SELECT * FROM names;";
	message_len += strlen(query) + 1;
	int16_t type_params = 4;
	message_len += sizeof(int16_t);
	int32_t parm_data_types[5] = {1, 2, 3, 4, 5};
	message_len += sizeof(parm_data_types);

	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage) + message_len);
	memset(test_data, 0, sizeof(BaseMessage) + message_len);
	char *c = test_data->data;
	test_data->type = PSQL_Parse;
	test_data->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);
	*c++ = '\0';
	memcpy(c, query, strlen(query));
	c += strlen(query);
	*c++ = '\0';
	*((int16_t *)c) = htons(type_params);
	c += sizeof(int16_t);
	memcpy(c, parm_data_types, sizeof(parm_data_types));

	Parse *parse = read_parse(test_data);

	assert_null(parse);

	free(test_data);
}

static void test_too_few_parms(void **state) {
	uint32_t message_len = 0;
	message_len += sizeof(uint32_t);
	char *dest = "a";
	message_len += strlen(dest) + 1;
	char *query = "SELECT * FROM names;";
	message_len += strlen(query) + 1;
	int16_t type_params = 6;
	message_len += sizeof(int16_t);
	int32_t parm_data_types[5] = {1, 2, 3, 4, 5};
	message_len += sizeof(parm_data_types);

	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage) + message_len);
	memset(test_data, 0, sizeof(BaseMessage) + message_len);
	char *c = test_data->data;
	test_data->type = PSQL_Parse;
	test_data->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);
	*c++ = '\0';
	memcpy(c, query, strlen(query));
	c += strlen(query);
	*c++ = '\0';
	*((int16_t *)c) = htons(type_params);
	c += sizeof(int16_t);
	memcpy(c, parm_data_types, sizeof(parm_data_types));

	Parse *parse = read_parse(test_data);

	assert_null(parse);

	free(test_data);
}

static void test_invalid_type(void **state) {
	uint32_t message_len = 0;
	message_len += sizeof(uint32_t);
	char *dest = "a";
	message_len += strlen(dest) + 1;
	char *query = "SELECT * FROM names;";
	message_len += strlen(query) + 1;
	int16_t type_params = 5;
	message_len += sizeof(int16_t);
	int32_t parm_data_types[5] = {1, 2, 3, 4, 5};
	message_len += sizeof(parm_data_types);

	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage) + message_len);
	memset(test_data, 0, sizeof(BaseMessage) + message_len);
	char *c = test_data->data;
	test_data->type = 'X';
	test_data->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);
	*c++ = '\0';
	memcpy(c, query, strlen(query));
	c += strlen(query);
	*c++ = '\0';
	*((int16_t *)c) = htons(type_params);
	c += sizeof(int16_t);
	memcpy(c, parm_data_types, sizeof(parm_data_types));

	Parse *parse = read_parse(test_data);

	assert_null(parse);

	free(test_data);
}

static void test_invalid_num_parm_data_types(void **state) {
	uint32_t message_len = 0;
	message_len += sizeof(uint32_t);
	char *dest = "a";
	message_len += strlen(dest) + 1;
	char *query = "SELECT * FROM names;";
	message_len += strlen(query) + 1;
	int16_t type_params = -1;
	message_len += sizeof(int16_t);

	BaseMessage *test_data = (BaseMessage *)malloc(sizeof(BaseMessage) + message_len);
	memset(test_data, 0, sizeof(BaseMessage) + message_len);
	char *c = test_data->data;
	test_data->type = PSQL_Parse;
	test_data->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);
	*c++ = '\0';
	memcpy(c, query, strlen(query));
	c += strlen(query);
	*c++ = '\0';
	*((int16_t *)c) = htons(type_params);

	Parse *parse = read_parse(test_data);

	assert_null(parse);

	free(test_data);
}

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_valid_input_with_parms),
	    cmocka_unit_test(test_valid_input_without_parms),
	    cmocka_unit_test(test_non_terminated_dest),
	    cmocka_unit_test(test_non_terminated_query),
	    cmocka_unit_test(test_unexpectedly_terminated_dest),
	    cmocka_unit_test(test_unexpectedly_terminated_query),
	    cmocka_unit_test(test_missing_dest_or_query),
	    cmocka_unit_test(test_missing_num_parm_data_types),
	    cmocka_unit_test(test_too_many_parms),
	    cmocka_unit_test(test_too_few_parms),
	    cmocka_unit_test(test_invalid_type),
	    cmocka_unit_test(test_invalid_num_parm_data_types),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
