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

int __wrap_read_bytes(RoctoSession *session, char *buffer, int buffer_size, int bytes_to_read) {
	int data_len = mock_type(int);
	if (data_len > 0) {
		char *data = mock_type(char*);
		memcpy(buffer, data, bytes_to_read);
	}
	return 0;
}

static void test_valid_input(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = htonl(196608);
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1;
	// Terminating null
	message_length += 1;
	char *c;
	ErrorResponse *err = NULL;

	// Length + extra stuff - already counted (length, protocol version_
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - 8);
	test_data->length = htonl(message_length);
	test_data->protocol_version = protocol_version;
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name));
	c += strlen(parm1_name);
	*c++ = '\0';
	memcpy(c, parm1_value, strlen(parm1_value));
	c += strlen(parm1_value);
	*c++ = '\0';
	*c++ = '\0';

	will_return(__wrap_read_bytes, message_length - 8);
	will_return(__wrap_read_bytes, test_data->data);


	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(startup);
	assert_null(err);
	assert_string_equal(parm1_name, startup->parameters[0].name);
	assert_string_equal(parm1_value, startup->parameters[0].value);
}

static void test_wrong_version(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0xdeadbeef;
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1;
	// Terminating null
	message_length += 1;
	char *c;
	ErrorResponse *err = NULL;

	// Length + extra stuff - already counted (length, protocol version_
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - 8);
	test_data->length = htonl(message_length);
	test_data->protocol_version = protocol_version;
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name));
	c += strlen(parm1_name);
	*c++ = '\0';
	memcpy(c, parm1_value, strlen(parm1_value));
	c += strlen(parm1_value);
	*c++ = '\0';
	*c++ = '\0';

	will_return(__wrap_read_bytes, 0);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_null(startup);
	assert_non_null(err);
}

static void test_non_terminated_name(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = htonl(196608);
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	// Pretend the messageis horter than it is
	message_length += strlen(parm1_name) + 1 - 2;
	char *c;
	ErrorResponse *err = NULL;

	// Length + extra stuff - already counted (length, protocol version_
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - 8);
	test_data->length = htonl(message_length);
	test_data->protocol_version = protocol_version;
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name));
	c += strlen(parm1_name);

	will_return(__wrap_read_bytes, 0);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_null(startup);
	assert_non_null(err);
}

static void test_non_terminated_value(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = htonl(196608);
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1 - 3;
	char *c;
	ErrorResponse *err = NULL;

	// Length + extra stuff - already counted (length, protocol version_
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - 8);
	test_data->length = htonl(message_length);
	test_data->protocol_version = protocol_version;
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name));
	c += strlen(parm1_name);
	*c++ = '\0';
	memcpy(c, parm1_value, strlen(parm1_value));
	c += strlen(parm1_value);

	will_return(__wrap_read_bytes, 0);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_null(startup);
	assert_non_null(err);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input),
		   cmocka_unit_test(test_wrong_version),
		   cmocka_unit_test(test_non_terminated_name),
		   cmocka_unit_test(test_non_terminated_value)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
