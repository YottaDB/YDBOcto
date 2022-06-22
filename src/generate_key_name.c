/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

/**
 * Populates buffer with the M representation of the specified target_key_num for this key from table
 *
 * @returns the number of characters written
 */
int generate_key_name(char **buffer, int *buffer_size, int target_key_num, SqlTable *table, SqlColumn **key_columns) {
	char *		    buffer_ptr, *columnName;
	int		    written;
	SqlValue *	    value;
	SqlOptionalKeyword *keyword;

	UNUSED(table); // we may eventually need this, and it's already in the code

	if (NULL == key_columns[target_key_num]) {
		return 0;
	}
	keyword = get_keyword(key_columns[target_key_num], OPTIONAL_EXTRACT);
	if (NULL != keyword) {
		/* EXTRACT columns cannot be key columns, so no key name is necessary in that case.
		 * So, just return without populating the buffer with a key name here.
		 */
		return 0;
	}
	UNPACK_SQL_STATEMENT(value, key_columns[target_key_num]->columnName, value);
	columnName = value->v.reference;

	buffer_ptr = *buffer;
	written = snprintf(buffer_ptr, *buffer_size - (buffer_ptr - *buffer), "keys(\"%s\")", columnName);
	if (written >= (*buffer_size - (buffer_ptr - *buffer))) {
		free(*buffer);
		*buffer_size = written + 1; // Null terminator
		*buffer = (char *)malloc(sizeof(char) * (*buffer_size));
		buffer_ptr = *buffer;
		written = snprintf(buffer_ptr, *buffer_size - (buffer_ptr - *buffer), "keys(\"%s\")", columnName);
	}
	assert(written < (*buffer_size - (buffer_ptr - *buffer)));
	buffer_ptr += written;
	*buffer_ptr++ = '\0';

	return buffer_ptr - *buffer;
}
