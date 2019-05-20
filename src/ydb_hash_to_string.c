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
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <stdint.h>

#include "mmrhash.h"

void ydb_hash_to_string(ydb_uint16 *hash, char *buffer, const unsigned int buf_len) {
	// Converts internal YottaDB hash format (ydb_uint16) to characters to fill buffer.
	// String is composed of 62 alphanumeric characters for M compatibility.
	char *base62_chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
	unsigned short next_char = 0;
	unsigned int i = 0;
	#ifdef __SIZEOF_INT128__
	unsigned __int128 *long_hash;
	long_hash = (__int128*)hash;
	#else
	int64_t *long_hash;
	long_hash = (int64_t*)&hash.one;
	#endif

	for (i = 0; i < buf_len; i++) {
		next_char = *long_hash % 62;
		buffer[i] = base62_chars[next_char];
		*long_hash = *long_hash / 62;
	}
}
