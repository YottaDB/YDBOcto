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
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <arpa/inet.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"

int64_t ntoh64(int64_t little_endian) {
	char big_endian[8];		// 64 bits
	*(int64_t*)big_endian = little_endian;
	int i = 0, j = 7;
	for (i = 0; i < 4; i++, j--) {
		// XOR swap algorithm
		big_endian[i] ^= big_endian[j];
		big_endian[j] ^= big_endian[i];
		big_endian[i] ^= big_endian[j];
	}
	return *(int64_t*)big_endian;
}

int64_t hton64(int64_t big_endian) {
	char little_endian[8];		// 64 bits
	*(int64_t*)little_endian = big_endian;
	int i = 0, j = 7;
	for (i = 0; i < 4; i++, j--) {
		// XOR swap algorithm
		little_endian[i] ^= little_endian[j];
		little_endian[j] ^= little_endian[i];
		little_endian[i] ^= little_endian[j];
	}
	return *(int64_t*)little_endian;
}

char *byte_to_hex(char c, char *hex) {
	unsigned char high = 0, low = 0;
	// Isolate nibbles
	high = (unsigned char)c >> 4;
	low = c << 4;
	low >>= 4;

	// Map to ASCII digits or lower case a-f
	if (high < 10) {
		high += 48;
	} else {
		high += 87;
	}
	if (low < 10) {
		low += 48;
	} else {
		low += 87;
	}

	hex[0] = high;
	hex[1] = low;
	hex[2] = '\0';
	return hex;
}

char bin_to_bool(char *bin) {
	return bin[0];
}

char bin_to_char(char *bin) {
	return bin[0];
}

int64_t bin_to_int16(char *bin) {
	int64_t new_int = 0;
	new_int = ntohs(*(int16_t*)bin);
	return new_int;
}

int64_t bin_to_int32(char *bin) {
	int64_t new_int = 0;
	new_int = ntohl(*(int32_t*)bin);
	return new_int;
}

int64_t bin_to_int64(char *bin) {
	int64_t new_int = 0;
	new_int = ntoh64(*(int64_t*)bin);
	return new_int;
}

int64_t bin_to_oid(char *bin) {
	int64_t new_int = 0;
	new_int = ntohl(*(int32_t*)bin);
	return new_int;
}

/*
float bin_to_float4(char *bin) {
}

double bin_to_float8(char *bin) {
}
*/

// TODO: This is currently ambiguous, the Go pq and rust-postgres drivers
// both handle this case by simply returning the input. However, the PostgreSQL
// documentation states that the bytea type must be a hex string preceded by '\x'
// or a (largely deprecated) 'escape string'
char *bin_to_bytea(char *bin) {
	return bin;
}

void bin_to_uuid(char *bin, char *buffer, int buf_len) {
	const int uuid_len = 36;	// 16 bytes * 2 nibbles/byte + 4 dashes
	const int two_bytes = 2;
	char *temp_16 = NULL;
	assert(buf_len > uuid_len);
	int dash_offset = 8;
	int i = 0, j = 0;

	temp_16 = (char*)malloc(two_bytes);
	// Initialize dashes according to UUID format
	buffer[8] = buffer[13] = buffer[18] = buffer[23] = '-';
	for (i = 0; i < dash_offset; i++) {
		strncpy(&buffer[i], byte_to_hex(bin[i], temp_16), two_bytes);
	}

	i = dash_offset + 1;
	dash_offset += 5;
	for (j = 0; j < 4; j++) {
		while (i < dash_offset) {
			strncpy(&buffer[i], byte_to_hex(bin[i], temp_16), two_bytes);
			i++;
		}
		dash_offset += 5;
	}
	buffer[uuid_len] = '\0';
	free(temp_16);
}

// HSTORE?

// VARBIT/BIT?

// TIME/TIMETZ?

// TIMESTAMP/TIMESTAMPTZ?

// DATE?

// MACADDR?

// ARRAY?

// RANGE?

// POINT?

// BOX?

// PATH?

// INET?
