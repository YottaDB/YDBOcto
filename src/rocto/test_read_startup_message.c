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

static void test_valid_input_no_parms(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0x00030000;		// version 3.0
	message_length += sizeof(unsigned int);
	message_length += 1;		// count parameter list terminator
	ErrorResponse *err = NULL;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage*)malloc(
			message_length + sizeof(StartupMessage) - sizeof(unsigned int) - sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);
	*test_data->data = '\0';

	// mock read_bytes from socket
	will_return(__wrap_read_bytes, message_length - sizeof(unsigned int) - sizeof(unsigned int));
	will_return(__wrap_read_bytes, "");

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(startup);
	assert_null(err);
	assert_int_equal(0, startup->num_parameters);

	free(test_data);
	free(startup->parameters);
	free(startup);
}

static void test_valid_input_one_parm(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0x00030000;		// version 3.0
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;	// count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1;	// count null
	message_length += 1;		// count parameter list terminator
	char *c;
	ErrorResponse *err = NULL;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage*)malloc(
			message_length + sizeof(StartupMessage) - sizeof(unsigned int) - sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1);		// copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1);	// copy null
	c += strlen(parm1_value) + 1;
	*c = '\0';		// parameter list terminator

	// mock read_bytes from socket
	will_return(__wrap_read_bytes, message_length - sizeof(unsigned int) - sizeof(unsigned int));
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(startup);
	assert_null(err);
	assert_string_equal(parm1_name, startup->parameters[0].name);
	assert_string_equal(parm1_value, startup->parameters[0].value);

	free(test_data);
	free(startup->parameters);
	free(startup);
}

static void test_valid_input_multi_parm(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0x00030000;		// version 3.0
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;	// count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1;	// count null
	char *parm2_name = "user";
	message_length += strlen(parm2_name) + 1;	// count null
	char *parm2_value = "jon";
	message_length += strlen(parm2_value) + 1;	// count null
	message_length += 1;		// count parameter list terminator
	char *c;
	ErrorResponse *err = NULL;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1);		// copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1);	// copy null
	c += strlen(parm1_value) + 1;
	memcpy(c, parm2_name, strlen(parm2_name) + 1);		// copy null
	c += strlen(parm2_name) + 1;
	memcpy(c, parm2_value, strlen(parm2_value) + 1);	// copy null
	c += strlen(parm2_value) + 1;
	*c = '\0';		// parameter list terminator

	// mock read_bytes from socket
	will_return(__wrap_read_bytes, message_length - 2 * sizeof(unsigned int));
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(startup);
	assert_null(err);
	assert_string_equal(parm1_name, startup->parameters[0].name);
	assert_string_equal(parm1_value, startup->parameters[0].value);
	assert_string_equal(parm2_name, startup->parameters[1].name);
	assert_string_equal(parm2_value, startup->parameters[1].value);

	free(test_data);
	free(startup->parameters);
	free(startup);
}

static void test_no_parms_without_null(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0x00030000;		// version 3.0
	message_length += sizeof(unsigned int);
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage*)malloc(
			message_length + sizeof(StartupMessage) - sizeof(unsigned int) - sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(startup);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "StartupMessage", "parameter list");
	assert_string_equal(error_message, err->args[2].value + 1);

	free_error_response(err);
	free(test_data);
}

static void test_one_parm_without_null(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0x00030000;		// version 3.0
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;	// count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1;	// count null
	char *c;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage*)malloc(
			message_length + sizeof(StartupMessage) - sizeof(unsigned int) - sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1);		// copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1);	// copy null

	// mock read_bytes from socket
	will_return(__wrap_read_bytes, message_length - sizeof(unsigned int) - sizeof(unsigned int));
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(startup);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "StartupMessage", "parameter list");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(test_data);
	free_error_response(err);
}

static void test_multi_parm_without_null(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0x00030000;		// version 3.0
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;	// count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1;	// count null
	char *parm2_name = "user";
	message_length += strlen(parm2_name) + 1;	// count null
	char *parm2_value = "jon";
	message_length += strlen(parm2_value) + 1;	// count null
	char *c;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1);		// copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1);	// copy null
	c += strlen(parm1_value) + 1;
	memcpy(c, parm2_name, strlen(parm2_name) + 1);		// copy null
	c += strlen(parm2_name) + 1;
	memcpy(c, parm2_value, strlen(parm2_value) + 1);	// copy null

	// mock read_bytes from socket
	will_return(__wrap_read_bytes, message_length - 2 * sizeof(unsigned int));
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(startup);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "StartupMessage", "parameter list");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(test_data);
	free_error_response(err);
}


static void test_wrong_version(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0xdeadbeef;	// bad version number
	message_length += sizeof(unsigned int);
	char *parm1_name = "user\0";
	message_length += strlen(parm1_name) + 1;	// count null
	char *parm1_value = "charles\0";
	message_length += strlen(parm1_value) + 1;	// count null
	// Terminating null
	message_length += 1;
	char *c;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Length + extra stuff - already counted (length, protocol version_
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1);		// copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1);	// copy null
	c += strlen(parm1_value) + 1;
	*c = '\0';

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_null(startup);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_VERSION,
			"StartupMessage", ntohl(test_data->protocol_version), 0x00030000);
	assert_string_equal(error_message, err->args[2].value + 1);

	free_error_response(err);
	free(test_data);
}

static void test_missing_version(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;	// count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1;	// count null
	// Terminating null
	message_length += 1;
	char *c;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Length + extra stuff - already counted (length)
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - sizeof(unsigned int));
	test_data->length = htonl(message_length);

	// Copy parms into message
	c = (char*)&(test_data->protocol_version);
	memcpy(c, parm1_name, strlen(parm1_name) + 1);		// copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1);	// copy null
	c += strlen(parm1_value) + 1;
	*c = '\0';

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_null(startup);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_INVALID_VERSION,
			"StartupMessage", ntohl(test_data->protocol_version), 0x00030000);
	assert_string_equal(error_message, err->args[2].value + 1);

	free_error_response(err);
	free(test_data);
}

static void test_non_terminated_name(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0x00030000;		// version 3.0
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	// Pretend the name is shorter than it is
	message_length += strlen(parm1_name) - 1;	// exclude null
	char *parm1_value = "charles\0";
	message_length += strlen(parm1_value) + 1;	// count null
	char *c;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) - 1);
	c += strlen(parm1_name) - 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1);	// copy null
	c += strlen(parm1_value) + 1;
	// *c = '\0';

	// mock read_bytes from socket
	will_return(__wrap_read_bytes, message_length - 2 * sizeof(unsigned int));
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_null(startup);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "StartupMessage", "name or value");
	assert_string_equal(error_message, err->args[2].value + 1);

	free_error_response(err);
	free(test_data);
}

static void test_non_terminated_value(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = htonl(0x00030000);
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;
	// Pretend the name is shorter than it is
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) - 2;	// exclude nulls
	char *c;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Length + extra stuff - already counted (length, protocol version_
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = protocol_version;
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1);		// include null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) - 2);	// exclude nulls
	c += strlen(parm1_value);

	// mock read_bytes from socket
	will_return(__wrap_read_bytes, message_length - 2 * sizeof(unsigned int));
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_null(startup);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "StartupMessage", "name or value");
	assert_string_equal(error_message, err->args[2].value + 1);

	free_error_response(err);
	free(test_data);
}

static void test_unexpectedly_terminated_name(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0x00030000;		// version 3.0
	message_length += sizeof(unsigned int);
	char *parm1_name = "us\0er";
	// Pretend the name is shorter than it is
	message_length += strlen(parm1_name) + 2;	// count extra null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1;	// count null
	char *c;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	StartupMessage *test_data = (StartupMessage*)malloc(
			message_length + sizeof(StartupMessage) - sizeof(unsigned int) - sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, message_length - sizeof(unsigned int) - sizeof(unsigned int) - strlen(parm1_value) - 1);
	c += message_length - sizeof(unsigned int) - sizeof(unsigned int) - strlen(parm1_value) - 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1);	// copy null

	// mock read_bytes from socket
	will_return(__wrap_read_bytes, message_length - sizeof(unsigned int) - sizeof(unsigned int));
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_null(startup);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "StartupMessage", "parameter list");
	assert_string_equal(error_message, err->args[2].value + 1);

	free_error_response(err);
	free(test_data);
}

static void test_unexpectedly_terminated_value(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0x00030000;		// version 3.0
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	// Pretend the name is shorter than it is
	message_length += strlen(parm1_name) + 1;	// count null
	char *parm1_value = "char\0les";
	message_length += strlen(parm1_value) + 3 + 2;	// count remaining chars + nulls
	char *c;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	StartupMessage *test_data = (StartupMessage*)malloc(
			message_length + sizeof(StartupMessage) - sizeof(unsigned int) - sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_value, strlen(parm1_name) + 1);	// copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, message_length - sizeof(unsigned int) - sizeof(unsigned int) - strlen(parm1_name) - 1);

	// mock read_bytes from socket
	will_return(__wrap_read_bytes, message_length - sizeof(unsigned int) - sizeof(unsigned int));
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_null(startup);
	assert_non_null(err);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "StartupMessage", "name or value");
	assert_string_equal(error_message, err->args[2].value + 1);

	free_error_response(err);
	free(test_data);
}

static void test_missing_parm_name(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0x00030000;		// version 3.0
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;	// count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1;	// count null
	char *parm2_value = "jon";
	message_length += strlen(parm2_value) + 1;	// count null
	message_length += 1;		// count parameter list terminator
	char *c;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1);		// copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1);	// copy null
	c += strlen(parm1_value) + 1;
	memcpy(c, parm2_value, strlen(parm2_value) + 1);	// copy null
	c += strlen(parm2_value) + 1;
	*c = '\0';		// parameter list terminator

	// mock read_bytes from socket
	will_return(__wrap_read_bytes, message_length - 2 * sizeof(unsigned int));
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(startup);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "StartupMessage", "parameter list");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(test_data);
	free_error_response(err);
}

static void test_missing_parm_value(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0x00030000;		// version 3.0
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;	// count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1;	// count null
	char *parm2_name = "use";
	message_length += strlen(parm2_name) + 1;	// count null
	message_length += 1;		// count parameter list terminator
	char *c;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1);		// copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1);	// copy null
	c += strlen(parm1_value) + 1;
	memcpy(c, parm2_name, strlen(parm2_name) + 1);	// copy null
	c += strlen(parm2_name) + 1;
	*c = '\0';		// parameter list terminator

	// mock read_bytes from socket
	will_return(__wrap_read_bytes, message_length - 2 * sizeof(unsigned int));
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(startup);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_MISSING_NULL, "StartupMessage", "parameter list");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(test_data);
	free_error_response(err);
}

static void test_message_has_trailing_chars(void **state) {
	// Test a single startup message
	unsigned int message_length = 0;
	message_length += sizeof(unsigned int);
	unsigned int protocol_version = 0x00030000;		// version 3.0
	message_length += sizeof(unsigned int);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;	// count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1;	// count null
	char *parm2_name = "user";
	message_length += strlen(parm2_name) + 1;	// count null
	char *parm2_value = "jon";
	message_length += strlen(parm2_value) + 1;	// count null
	message_length += 1;		// count parameter list terminator
	char *c;
	ErrorResponse *err = NULL;
	ErrorBuffer err_buff;
	err_buff.offset = 0;
	const char *error_message;

	message_length *= 2;		// Create trailing chars
	StartupMessage *test_data = (StartupMessage*)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(unsigned int));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1);		// copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1);	// copy null
	c += strlen(parm1_value) + 1;
	memcpy(c, parm2_name, strlen(parm2_name) + 1);		// copy null
	c += strlen(parm2_name) + 1;
	memcpy(c, parm2_value, strlen(parm2_value) + 1);	// copy null
	c += strlen(parm2_value) + 1;
	*c = '\0';		// parameter list terminator

	// mock read_bytes from socket
	will_return(__wrap_read_bytes, message_length - 2 * sizeof(unsigned int));
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	StartupMessage *startup = read_startup_message(NULL, (char*)(&test_data->length), message_length, &err);

	// Standard checks
	assert_non_null(err);
	assert_null(startup);

	// Ensure correct error message
	error_message = format_error_string(&err_buff, ERR_ROCTO_TRAILING_CHARS, "StartupMessage");
	assert_string_equal(error_message, err->args[2].value + 1);

	free(test_data);
	free_error_response(err);
}


int main(void) {
	const struct CMUnitTest tests[] = {
			cmocka_unit_test(test_valid_input_no_parms),
			cmocka_unit_test(test_valid_input_one_parm),
			cmocka_unit_test(test_valid_input_multi_parm),
			cmocka_unit_test(test_no_parms_without_null),
			cmocka_unit_test(test_one_parm_without_null),
			cmocka_unit_test(test_multi_parm_without_null),
			cmocka_unit_test(test_wrong_version),
			cmocka_unit_test(test_missing_version),
			cmocka_unit_test(test_non_terminated_name),
			cmocka_unit_test(test_non_terminated_value),
			cmocka_unit_test(test_unexpectedly_terminated_name),
			cmocka_unit_test(test_unexpectedly_terminated_value),
			cmocka_unit_test(test_missing_parm_name),
			cmocka_unit_test(test_missing_parm_value),
			cmocka_unit_test(test_message_has_trailing_chars),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
