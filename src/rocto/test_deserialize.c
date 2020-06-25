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
#include <stdint.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <endian.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include <openssl/md5.h>

#include "octo.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

static void test_byte_to_hex(void **state) {
	char c = 0xff, hex[3];
	byte_to_hex(c, hex);
	assert_string_equal(hex, "ff");

	c = 0;
	byte_to_hex(c, hex);
	assert_string_equal(hex, "00");

	c = 8;
	byte_to_hex(c, hex);
	assert_string_equal(hex, "08");

	c = 19;
	byte_to_hex(c, hex);
	assert_string_equal(hex, "13");

	c = 47;
	byte_to_hex(c, hex);
	assert_string_equal(hex, "2f");
}

static void test_bin_to_bool(void **state) {
	int64_t result = FALSE;
	char	bin = TRUE;
	result = bin_to_bool((char *)&bin);
	assert_int_equal(result, bin);

	result = TRUE, bin = FALSE;
	result = bin_to_bool((char *)&bin);
	assert_int_equal(result, bin);
}

static void test_bin_to_char(void **state) {
	int64_t result = '\0';
	char	bin = 'A';
	result = bin_to_char((char *)&bin);
	assert_int_equal(result, bin);

	result = '\0', bin = '7';
	result = bin_to_char((char *)&bin);
	assert_int_equal(result, bin);
}

static void test_bin_to_int16(void **state) {
	int64_t result = 0;
	char	bin[2];
	*(int16_t *)bin = htons(12);
	result = bin_to_int16(bin);
	assert_int_equal(result, ntohs(*(int16_t *)bin));

	result = 0;
	*(int16_t *)bin = htons(256);
	result = bin_to_int16(bin);
	assert_int_equal(result, ntohs(*(int16_t *)bin));

	result = 0;
	*(int16_t *)bin = htons(INT16_MAX);
	result = bin_to_int16(bin);
	assert_int_equal(result, ntohs(*(int16_t *)bin));

	result = -1;
	*(int16_t *)bin = htons(0);
	result = bin_to_int16(bin);
	assert_int_equal(result, ntohs(*(int16_t *)bin));
}

static void test_bin_to_int32(void **state) {
	int64_t result = 0;
	char	bin[4];
	*(int32_t *)bin = htonl(12);
	result = bin_to_int32(bin);
	assert_int_equal(result, ntohl(*(int32_t *)bin));

	result = 0;
	*(int32_t *)bin = htonl(256);
	result = bin_to_int32(bin);
	assert_int_equal(result, ntohl(*(int32_t *)bin));

	result = 0;
	*(int32_t *)bin = htonl(INT32_MAX);
	result = bin_to_int32(bin);
	assert_int_equal(result, ntohl(*(int32_t *)bin));

	result = -1;
	*(int32_t *)bin = htonl(0);
	result = bin_to_int32(bin);
	assert_int_equal(result, ntohl(*(int32_t *)bin));
}

static void test_bin_to_int64(void **state) {
	int64_t result = 0;
	char	bin[8];
	*(int64_t *)bin = (int64_t)htobe64(12);
	result = bin_to_int64(bin);
	assert_int_equal(result, (int64_t)be64toh(*(uint64_t *)bin));

	result = 0;
	*(int64_t *)bin = (int64_t)htobe64(256);
	result = bin_to_int64(bin);
	assert_int_equal(result, (int64_t)be64toh(*(uint64_t *)bin));
	result = 0;
	*(int64_t *)bin = (int64_t)htobe64(INT64_MAX);
	result = bin_to_int64(bin);
	assert_int_equal(result, (int64_t)be64toh(*(uint64_t *)bin));

	result = -1;
	*(int64_t *)bin = (int64_t)htobe64(0);
	result = bin_to_int64(bin);
	assert_int_equal(result, (int64_t)be64toh(*(uint64_t *)bin));
}

static void test_bin_to_oid(void **state) {
	int64_t result = 0;
	char	bin[4];
	*(int32_t *)bin = htonl(12);
	result = bin_to_oid(bin);
	assert_int_equal(result, ntohl(*(int32_t *)bin));

	result = 0;
	*(int32_t *)bin = htonl(256);
	result = bin_to_oid(bin);
	assert_int_equal(result, ntohl(*(int32_t *)bin));

	result = 0;
	*(int32_t *)bin = htonl(INT32_MAX);
	result = bin_to_oid(bin);
	assert_int_equal(result, ntohl(*(int32_t *)bin));

	result = -1;
	*(int32_t *)bin = htonl(0);
	result = bin_to_oid(bin);
	assert_int_equal(result, ntohl(*(int32_t *)bin));
}

static void test_bin_to_bytea(void **state) {
	char *bin = "it's just a tester";
	char *result = NULL;
	result = bin_to_bytea(bin);
	assert_string_equal(result, bin);
}

static void test_bin_to_uuid(void **state) {
	int32_t buf_len = UUID_CHARACTER_LENGTH;
	char	bin[16];
	char	buffer[buf_len];
	int32_t i = 0;
	for (i = 0; i < 16; i++) {
		bin[i] = i;
	}
	bin_to_uuid(bin, buffer);
	assert_string_equal(buffer, "00010203-0405-0607-0809-0a0b0c0d0e0f");
}

static void test_md5_to_hex(void **state) {
	const uint32_t md5_len = 33;
	char	       hex_buf[md5_len];
	char *	       message = "bluemonday";	      // md5 hash: 1865f47f47b0ccc5c69178ecbbcbf645
	unsigned char  digest[MD5_DIGEST_LENGTH + 1]; // count null

	MD5((const unsigned char *)message, strlen(message), digest);
	digest[MD5_DIGEST_LENGTH] = '\0';

	// Check with valid input
	int32_t result = md5_to_hex(digest, hex_buf, md5_len);
	assert_int_equal(result, 0);
	assert_string_equal(hex_buf, "1865f47f47b0ccc5c69178ecbbcbf645");

	// Check with bad length
	result = md5_to_hex(digest, hex_buf, 0);
	assert_int_equal(result, 1);
}

int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
	    cmocka_unit_test(test_byte_to_hex),	 cmocka_unit_test(test_bin_to_bool),  cmocka_unit_test(test_bin_to_char),
	    cmocka_unit_test(test_bin_to_int16), cmocka_unit_test(test_bin_to_int32), cmocka_unit_test(test_bin_to_int64),
	    cmocka_unit_test(test_bin_to_oid),	 cmocka_unit_test(test_bin_to_uuid),  cmocka_unit_test(test_md5_to_hex),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
