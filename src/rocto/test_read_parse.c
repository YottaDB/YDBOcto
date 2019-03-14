/* Copyright (C) 2018-2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
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

static void test_valid_input(void **state) {
	unsigned int message_len = 0;
	message_len += sizeof(unsigned int);
	char *dest = "a";
	message_len += strlen(dest) + 1;
	char *query = "SELECT * FROM names;";
	message_len += strlen(query) + 1;
	short type_params = 0;
	message_len += sizeof(short);

	BaseMessage *message = (BaseMessage*)malloc(sizeof(BaseMessage) + message_len);
	memset(message, 0, sizeof(BaseMessage) + message_len);
	char *c = message->data;
	message->type = PSQL_Parse;
	message->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);
	*c++ = '\0';
	memcpy(c, query, strlen(query));
	c += strlen(query);
	*c++ = '\0';
	*((short*)c) = htons(type_params);

	ErrorResponse *err = NULL;
	Parse *parse = read_parse(message, &err);

	assert_non_null(parse);
	assert_string_equal(parse->dest, dest);
	assert_string_equal(parse->query, query);
	assert_int_equal(parse->num_parms, type_params);

	assert_null(err);
	free(message);
	free(parse);
}

static void test_too_many_parms(void **state) {
	unsigned int message_len = 0;
	message_len += sizeof(unsigned int);
	char *dest = "a";
	message_len += strlen(dest) + 1;
	char *query = "SELECT * FROM names;";
	message_len += strlen(query) + 1;
	short type_params = 100;
	message_len += sizeof(short);

	BaseMessage *message = (BaseMessage*)malloc(sizeof(BaseMessage) + message_len);
	memset(message, 0, sizeof(BaseMessage) + message_len);
	char *c = message->data;
	message->type = PSQL_Parse;
	message->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);
	*c++ = '\0';
	memcpy(c, query, strlen(query));
	c += strlen(query);
	*c++ = '\0';
	*((short*)c) = htons(type_params);

	ErrorResponse *err = NULL;
	Parse *parse = read_parse(message, &err);

	assert_null(parse);
	assert_non_null(err);

	free_error_response(err);
	free(message);
}

static void test_non_terminated_query(void **state) {
	unsigned int message_len = 0;
	message_len += sizeof(unsigned int);
	char *dest = "a";
	message_len += strlen(dest) + 1;
	char *query = "SELECT * FROM names;";
	message_len += strlen(query) - 1;

	BaseMessage *message = (BaseMessage*)malloc(sizeof(BaseMessage) + message_len);
	memset(message, 0, sizeof(BaseMessage) + message_len);
	char *c = message->data;
	message->type = PSQL_Parse;
	message->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);
	*c++ = '\0';
	memcpy(c, query, strlen(query));
	c += strlen(query);

	ErrorResponse *err = NULL;
	Parse *parse = read_parse(message, &err);

	assert_null(parse);
	assert_non_null(err);

	free(message);
	free_error_response(err);
}

static void test_non_terminated_dest(void **state) {
	unsigned int message_len = 0;
	message_len += sizeof(unsigned int);
	char *dest = "a saosadfkasdfjkasd fwearf asdfkds f";
	message_len += strlen(dest) - 1;

	BaseMessage *message = (BaseMessage*)malloc(sizeof(BaseMessage) + message_len);
	memset(message, 0, sizeof(BaseMessage) + message_len);
	char *c = message->data;
	message->type = PSQL_Parse;
	message->length = htonl(message_len);
	memcpy(c, dest, strlen(dest));
	c += strlen(dest);

	ErrorResponse *err = NULL;
	Parse *parse = read_parse(message, &err);

	assert_null(parse);
	assert_non_null(err);

	free_error_response(err);
	free(message);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input),
		cmocka_unit_test(test_non_terminated_dest),
		cmocka_unit_test(test_non_terminated_query),
		cmocka_unit_test(test_too_many_parms)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
