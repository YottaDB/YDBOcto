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

unsigned int get_user_column_value(char *buffer, unsigned int buf_len, char *row, unsigned int row_len, enum UserColumns column) {
	char *c = NULL, *col_start = NULL, *row_end = NULL;
	unsigned int i = 0, col_num = 0, value_len = 0;

	c = row;
	row_end = row + row_len;
	// Find start of desired column value
	while (c < row_end && col_num < column) {
		if (COLUMN_DELIMITER == *c) {
			col_num++;
		}
		c++;
	}
	col_start = c;
	// Find length of column value string
	while (COLUMN_DELIMITER != *c) {
		value_len++;
		c++;
	}
	if (value_len > buf_len) {
		return 0;
	}
	strncpy(buffer, col_start, value_len);
	buffer[value_len] = '\0'

	return value_len;
}
