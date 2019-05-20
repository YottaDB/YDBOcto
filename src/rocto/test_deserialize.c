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
#include <stdint.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "octo.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

static void test_ntoh64(void **state) {
	int64_t number = -1;
	number <<= 8;
	number = ntoh64(number);
	assert_int_equal(number, (uint64_t)-1 >> 8);	// Logical shift

	number = -1;
	number <<= 16;
	number = ntoh64(number);
	assert_int_equal(number, (uint64_t)-1 >> 16);	// Logical shift

	number = -1;
	number <<= 24;
	number = ntoh64(number);
	assert_int_equal(number, (uint64_t)-1 >> 24);	// Logical shift

	number = -1;
	number <<= 32;
	number = ntoh64(number);
	assert_int_equal(number, (uint64_t)-1 >> 32);	// Logical shift

	number = -1;
	number <<= 40;
	number = ntoh64(number);
	assert_int_equal(number, (uint64_t)-1 >> 40);	// Logical shift

	number = -1;
	number <<= 48;
	number = ntoh64(number);
	assert_int_equal(number, (uint64_t)-1 >> 48);	// Logical shift

	number = -1;
	number <<= 56;
	number = ntoh64(number);
	assert_int_equal(number, (uint64_t)-1 >> 56);	// Logical shift

	number = -1;
	number = ntoh64(number);
	assert_int_equal(number, -1);	// Arithmetic shift
}

static void test_hton64(void **state) {
	int64_t number = -1;
	number <<= 8;
	number = hton64(number);
	assert_int_equal(number, (uint64_t)-1 >> 8);	// Logical shift

	number = -1;
	number <<= 16;
	number = hton64(number);
	assert_int_equal(number, (uint64_t)-1 >> 16);	// Logical shift

	number = -1;
	number <<= 24;
	number = hton64(number);
	assert_int_equal(number, (uint64_t)-1 >> 24);	// Logical shift

	number = -1;
	number <<= 32;
	number = hton64(number);
	assert_int_equal(number, (uint64_t)-1 >> 32);	// Logical shift

	number = -1;
	number <<= 40;
	number = hton64(number);
	assert_int_equal(number, (uint64_t)-1 >> 40);	// Logical shift

	number = -1;
	number <<= 48;
	number = hton64(number);
	assert_int_equal(number, (uint64_t)-1 >> 48);	// Logical shift

	number = -1;
	number <<= 56;
	number = hton64(number);
	assert_int_equal(number, (uint64_t)-1 >> 56);	// Logical shift

	number = -1;
	hton64(number);
	assert_int_equal(number, -1);	// Arithmetic shift
}

static void test_byte_to_hex(void **state) {
	char c = UINT8_MAX, hex[3];
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
	char result = FALSE, bin = TRUE;
	result = bin_to_bool((char*)&bin);
	assert_int_equal(result, bin);

	result = TRUE, bin = FALSE;
	result = bin_to_bool((char*)&bin);
	assert_int_equal(result, bin);
}

static void test_bin_to_char(void **state) {
	char result = '\0', bin = 'A';
	result = bin_to_char((char*)&bin);
	assert_int_equal(result, bin);

	result = '\0', bin = '7';
	result = bin_to_char((char*)&bin);
	assert_int_equal(result, bin);
}

static void test_bin_to_int16(void **state) {
	int64_t result = 0;
	char bin[2];
	*(int16_t*)bin = htons(12);
	result = bin_to_int16(bin);
	assert_int_equal(result, ntohs(*(int16_t*)bin));

	result = 0;
	*(int16_t*)bin = htons(256);
	result = bin_to_int16(bin);
	assert_int_equal(result, ntohs(*(int16_t*)bin));

	result = 0;
	*(int16_t*)bin = htons(INT16_MAX);
	result = bin_to_int16(bin);
	assert_int_equal(result, ntohs(*(int16_t*)bin));

	result = -1;
	*(int16_t*)bin = htons(0);
	result = bin_to_int16(bin);
	assert_int_equal(result, ntohs(*(int16_t*)bin));
}

static void test_bin_to_int32(void **state) {
	int64_t result = 0;
	char bin[4];
	*(int32_t*)bin = htonl(12);
	result = bin_to_int32(bin);
	assert_int_equal(result, ntohl(*(int32_t*)bin));

	result = 0;
	*(int32_t*)bin = htonl(256);
	result = bin_to_int32(bin);
	assert_int_equal(result, ntohl(*(int32_t*)bin));

	result = 0;
	*(int32_t*)bin = htonl(INT32_MAX);
	result = bin_to_int32(bin);
	assert_int_equal(result, ntohl(*(int32_t*)bin));

	result = -1;
	*(int32_t*)bin = htonl(0);
	result = bin_to_int32(bin);
	assert_int_equal(result, ntohl(*(int32_t*)bin));
}

static void test_bin_to_int64(void **state) {
	int64_t result = 0;
	char bin[8];
	*(int64_t*)bin = hton64(12);
	result = bin_to_int64(bin);
	assert_int_equal(result, ntoh64(*(int64_t*)bin));

	result = 0;
	*(int64_t*)bin = hton64(256);
	result = bin_to_int64(bin);
	assert_int_equal(result, ntoh64(*(int64_t*)bin));
	result = 0;
	*(int64_t*)bin = hton64(INT64_MAX);
	result = bin_to_int64(bin);
	assert_int_equal(result, ntoh64(*(int64_t*)bin));

	result = -1;
	*(int64_t*)bin = hton64(0);
	result = bin_to_int64(bin);
	assert_int_equal(result, ntoh64(*(int64_t*)bin));
}

static void test_bin_to_oid(void **state) {
	int64_t result = 0;
	char bin[4];
	*(int32_t*)bin = htonl(12);
	result = bin_to_oid(bin);
	assert_int_equal(result, ntohl(*(int32_t*)bin));

	result = 0;
	*(int32_t*)bin = htonl(256);
	result = bin_to_oid(bin);
	assert_int_equal(result, ntohl(*(int32_t*)bin));

	result = 0;
	*(int32_t*)bin = htonl(INT32_MAX);
	result = bin_to_oid(bin);
	assert_int_equal(result, ntohl(*(int32_t*)bin));

	result = -1;
	*(int32_t*)bin = htonl(0);
	result = bin_to_oid(bin);
	assert_int_equal(result, ntohl(*(int32_t*)bin));
}

static void test_bin_to_bytea(void **state) {
	char *bin = "it's just a tester";
	char *result = NULL;
	result = bin_to_bytea(bin);
	assert_string_equal(result, bin);
}

static void test_bin_to_uuid(void **state) {
	int buf_len = 37;
	char bin[16];
	char buffer[buf_len];
	int i = 0;
	for (i = 0; i < 16; i++) {
		bin[i] = i;
	}
	bin_to_uuid(bin, buffer, buf_len);
	assert_string_equal(buffer, "00010203-0405-0607-0809-0a0b0c0d0e0f");
}

int main(void) {
	octo_init(0, NULL, FALSE);
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_ntoh64),
		   cmocka_unit_test(test_hton64),
		   cmocka_unit_test(test_byte_to_hex),
		   cmocka_unit_test(test_bin_to_bool),
		   cmocka_unit_test(test_bin_to_char),
		   cmocka_unit_test(test_bin_to_int16),
		   cmocka_unit_test(test_bin_to_int32),
		   cmocka_unit_test(test_bin_to_int64),
		   cmocka_unit_test(test_bin_to_oid),
		   cmocka_unit_test(test_bin_to_uuid),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
