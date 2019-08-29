/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"

uint32_t get_user_column_value(char *buffer, const uint32_t buf_len, const char *row, const uint32_t row_len, enum UserColumns column) {
	if (NULL == buffer || NULL == row || column > UserColumn_ROLVALIDUNTIL) {
		return 0;
	}
	const char *c = NULL, *col_start = NULL;
	const char *row_end = row + row_len;
	uint32_t col_num = 0, value_len = 0;
	char *delimiter = COLUMN_DELIMITER;	// Allow access to delimiter as character

	c = row;
	// Find start of desired column value
	while (c < row_end && col_num < column) {
		if (delimiter[0] == *c) {
			col_num++;
		}
		c++;
	}
	col_start = c;
	// Find length of column value string
	while (delimiter[0] != *c) {
		value_len++;
		c++;
	}
	if (value_len > buf_len) {
		return 0;
	}
	strncpy(buffer, col_start, value_len);
	buffer[value_len] = '\0';

	return value_len;
}
