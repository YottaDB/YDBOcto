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
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <stdint.h>

#include "mmrhash.h"

void ydb_hash_to_string(ydb_uint16 *hash, char *buffer, const unsigned int buf_len) {
	// Converts internal YottaDB hash format (ydb_uint16) to characters to fill buffer.
	// String is composed of 62 alphanumeric characters for M compatibility.
	char *	       base62_chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
	unsigned short next_char = 0;
	unsigned int   i = 0;
	ydb_uint8      hashone, hashtwo;

	hashone = hash->one;
	hashtwo = hash->two;
	for (i = 0; i < buf_len;) {
		next_char = hashone % 62;
		buffer[i++] = base62_chars[next_char];
		hashone = hashone / 62;
		next_char = hashtwo % 62;
		buffer[i++] = base62_chars[next_char];
		hashtwo = hashtwo / 62;
	}
}
