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

int __wrap_read_bytes(RoctoSession *session, char **buffer, int32_t *buffer_size, int32_t bytes_to_read) {
	int32_t *data_len = mock_type(int *);
	if (*data_len > 0) {
		char *data = mock_type(char *);
		memcpy(*buffer, data, bytes_to_read);
	}
	return 0;
}

static void test_no_parms(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);
	message_length += 1; // count parameter list terminator

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data
	    = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - sizeof(uint32_t) - sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);
	*test_data->data = '\0';

	// mock read_bytes from socket
	passed_length = message_length - sizeof(uint32_t) - sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, "");

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Octo should catch missing username
	assert_null(startup);

	free(test_data);
}

static void test_valid_input_one_parm(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1; // count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1; // count null
	message_length += 1;			   // count parameter list terminator
	char *c;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data
	    = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - sizeof(uint32_t) - sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1); // copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1); // copy null
	c += strlen(parm1_value) + 1;
	*c = '\0'; // parameter list terminator

	// mock read_bytes from socket
	passed_length = message_length - sizeof(uint32_t) - sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_non_null(startup);
	assert_string_equal(parm1_name, startup->parameters[0].name);
	assert_string_equal(parm1_value, startup->parameters[0].value);

	free(test_data);
	free(startup->parameters);
	free(startup);
}

static void test_valid_input_multi_parm(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1; // count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1; // count null
	char *parm2_name = "user";
	message_length += strlen(parm2_name) + 1; // count null
	char *parm2_value = "jon";
	message_length += strlen(parm2_value) + 1; // count null
	message_length += 1;			   // count parameter list terminator
	char *c;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1); // copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1); // copy null
	c += strlen(parm1_value) + 1;
	memcpy(c, parm2_name, strlen(parm2_name) + 1); // copy null
	c += strlen(parm2_name) + 1;
	memcpy(c, parm2_value, strlen(parm2_value) + 1); // copy null
	c += strlen(parm2_value) + 1;
	*c = '\0'; // parameter list terminator

	// mock read_bytes from socket
	passed_length = message_length - 2 * sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_non_null(startup);
	assert_string_equal(parm1_name, startup->parameters[0].name);
	assert_string_equal(parm1_value, startup->parameters[0].value);
	assert_string_equal(parm2_name, startup->parameters[1].name);
	assert_string_equal(parm2_value, startup->parameters[1].value);

	free(test_data);
	free(startup->parameters);
	free(startup);
}

static void test_no_parms_without_null(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data
	    = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - sizeof(uint32_t) - sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_null(startup);

	free(test_data);
}

static void test_one_parm_without_null(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1; // count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1; // count null
	char *c;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data
	    = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - sizeof(uint32_t) - sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1); // copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1); // copy null

	// mock read_bytes from socket
	passed_length = message_length - sizeof(uint32_t) - sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_null(startup);

	free(test_data);
}

static void test_multi_parm_without_null(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1; // count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1; // count null
	char *parm2_name = "user";
	message_length += strlen(parm2_name) + 1; // count null
	char *parm2_value = "jon";
	message_length += strlen(parm2_value) + 1; // count null
	char *c;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1); // copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1); // copy null
	c += strlen(parm1_value) + 1;
	memcpy(c, parm2_name, strlen(parm2_name) + 1); // copy null
	c += strlen(parm2_name) + 1;
	memcpy(c, parm2_value, strlen(parm2_value) + 1); // copy null

	// mock read_bytes from socket
	passed_length = message_length - 2 * sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_null(startup);

	free(test_data);
}

static void test_wrong_version(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0xdeadbeef; // bad version number
	message_length += sizeof(uint32_t);
	char *parm1_name = "user\0";
	message_length += strlen(parm1_name) + 1; // count null
	char *parm1_value = "charles\0";
	message_length += strlen(parm1_value) + 1; // count null
	// Terminating null
	message_length += 1;
	char *c;

	// Length + extra stuff - already counted (length, protocol version_
	StartupMessage *test_data = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1); // copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1); // copy null
	c += strlen(parm1_value) + 1;
	*c = '\0';

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_null(startup);

	free(test_data);
}

static void test_missing_version(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1; // count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1; // count null
	// Terminating null
	message_length += 1;
	char *c;

	// Length + extra stuff - already counted (length)
	StartupMessage *test_data = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - sizeof(uint32_t));
	test_data->length = htonl(message_length);

	// Copy parms into message
	c = (char *)&(test_data->protocol_version);
	memcpy(c, parm1_name, strlen(parm1_name) + 1); // copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1); // copy null
	c += strlen(parm1_value) + 1;
	*c = '\0';

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_null(startup);

	free(test_data);
}

static void test_non_terminated_name(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);
	char *parm1_name = "user";
	// Pretend the name is shorter than it is
	message_length += strlen(parm1_name) - 1; // exclude null
	char *parm1_value = "charles\0";
	message_length += strlen(parm1_value) + 1; // count null
	char *c;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) - 1);
	c += strlen(parm1_name) - 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1); // copy null
	c += strlen(parm1_value) + 1;
	// *c = '\0';

	// mock read_bytes from socket
	passed_length = message_length - 2 * sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_null(startup);

	free(test_data);
}

static void test_non_terminated_value(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = htonl(0x00030000);
	message_length += sizeof(uint32_t);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1;
	// Pretend the name is shorter than it is
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) - 2; // exclude nulls
	char *c;

	// Length + extra stuff - already counted (length, protocol version_
	StartupMessage *test_data = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = protocol_version;
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1); // include null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) - 2); // exclude nulls
	c += strlen(parm1_value);

	// mock read_bytes from socket
	passed_length = message_length - 2 * sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_null(startup);

	free(test_data);
}

static void test_unexpectedly_terminated_name(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);
	char *parm1_name = "us\0er";
	// Pretend the name is shorter than it is
	message_length += strlen(parm1_name) + 2; // count extra null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1; // count null
	char *c;

	StartupMessage *test_data
	    = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - sizeof(uint32_t) - sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, message_length - sizeof(uint32_t) - sizeof(uint32_t) - strlen(parm1_value) - 1);
	c += message_length - sizeof(uint32_t) - sizeof(uint32_t) - strlen(parm1_value) - 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1); // copy null

	// mock read_bytes from socket
	passed_length = message_length - sizeof(uint32_t) - sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_null(startup);

	free(test_data);
}

static void test_unexpectedly_terminated_value(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);
	char *parm1_name = "user";
	// Pretend the name is shorter than it is
	message_length += strlen(parm1_name) + 1; // count null
	char *parm1_value = "char\0les";
	message_length += strlen(parm1_value) + 3 + 2; // count remaining chars + nulls
	char *c;

	StartupMessage *test_data
	    = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - sizeof(uint32_t) - sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_value, strlen(parm1_name) + 1); // copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, message_length - sizeof(uint32_t) - sizeof(uint32_t) - strlen(parm1_name) - 1);

	// mock read_bytes from socket
	passed_length = message_length - sizeof(uint32_t) - sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_null(startup);

	free(test_data);
}

static void test_missing_parm_name(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1; // count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1; // count null
	char *parm2_value = "jon";
	message_length += strlen(parm2_value) + 1; // count null
	message_length += 1;			   // count parameter list terminator
	char *c;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1); // copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1); // copy null
	c += strlen(parm1_value) + 1;
	memcpy(c, parm2_value, strlen(parm2_value) + 1); // copy null
	c += strlen(parm2_value) + 1;
	*c = '\0'; // parameter list terminator

	// mock read_bytes from socket
	passed_length = message_length - 2 * sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_null(startup);

	free(test_data);
}

static void test_missing_parm_value(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1; // count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1; // count null
	char *parm2_name = "use";
	message_length += strlen(parm2_name) + 1; // count null
	message_length += 1;			  // count parameter list terminator
	char *c;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1); // copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1); // copy null
	c += strlen(parm1_value) + 1;
	memcpy(c, parm2_name, strlen(parm2_name) + 1); // copy null
	c += strlen(parm2_name) + 1;
	*c = '\0'; // parameter list terminator

	// mock read_bytes from socket
	passed_length = message_length - 2 * sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_null(startup);

	free(test_data);
}

static void test_message_has_trailing_chars(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1; // count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1; // count null
	char *parm2_name = "user";
	message_length += strlen(parm2_name) + 1; // count null
	char *parm2_value = "jon";
	message_length += strlen(parm2_value) + 1; // count null
	message_length += 1;			   // count parameter list terminator
	char *c;

	message_length *= 2; // Create trailing chars
	StartupMessage *test_data = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - 2 * sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1); // copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1); // copy null
	c += strlen(parm1_value) + 1;
	memcpy(c, parm2_name, strlen(parm2_name) + 1); // copy null
	c += strlen(parm2_name) + 1;
	memcpy(c, parm2_value, strlen(parm2_value) + 1); // copy null
	c += strlen(parm2_value) + 1;
	*c = '\0'; // parameter list terminator

	// mock read_bytes from socket
	passed_length = message_length - 2 * sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	data = (char *)(&test_data->length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_null(startup);

	free(test_data);
}

static void test_buffer_resize(void **state) {
	char *data;
	// Test a single startup message
	uint32_t message_length = 0, passed_length = 0;
	message_length += sizeof(uint32_t);
	uint32_t protocol_version = 0x00030000; // version 3.0
	message_length += sizeof(uint32_t);
	char *parm1_name = "user";
	message_length += strlen(parm1_name) + 1; // count null
	char *parm1_value = "charles";
	message_length += strlen(parm1_value) + 1; // count null
	message_length += 1;			   // count parameter list terminator
	char *c;

	// Length + extra stuff - already counted (length, protocol version)
	StartupMessage *test_data
	    = (StartupMessage *)malloc(message_length + sizeof(StartupMessage) - sizeof(uint32_t) - sizeof(uint32_t));
	test_data->length = htonl(message_length);
	test_data->protocol_version = htonl(protocol_version);

	// Copy parms into message
	c = test_data->data;
	memcpy(c, parm1_name, strlen(parm1_name) + 1); // copy null
	c += strlen(parm1_name) + 1;
	memcpy(c, parm1_value, strlen(parm1_value) + 1); // copy null
	c += strlen(parm1_value) + 1;
	*c = '\0'; // parameter list terminator

	// mock read_bytes from socket
	passed_length = message_length - sizeof(uint32_t) - sizeof(uint32_t);
	will_return(__wrap_read_bytes, &passed_length);
	will_return(__wrap_read_bytes, test_data->data);

	// The actual test
	message_length = 8; // Arbitrary small value
	/* Allocate a separate buffer to allow reallocation (normall pass from the StartupMessage->length member, which is not at
	 * the start of the struct
	 */
	data = (char *)malloc(sizeof(char) * message_length);
	memcpy(data, &test_data->length, message_length);
	StartupMessage *startup = read_startup_message(NULL, &data, &message_length);

	// Standard checks
	assert_non_null(startup);
	assert_string_equal(parm1_name, startup->parameters[0].name);
	assert_string_equal(parm1_value, startup->parameters[0].value);

	free(data);
	free(test_data);
	free(startup->parameters);
	free(startup);
}

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_no_parms),
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
	    cmocka_unit_test(test_buffer_resize),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
