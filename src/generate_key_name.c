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
int generate_key_name(char *buffer, int buffer_size, int target_key_num, SqlTable *table, SqlColumn **key_columns) {
	char *		    buffer_ptr, *columnName, *temp;
	int		    written;
	SqlValue *	    value;
	SqlOptionalKeyword *keyword;

	UNUSED(table); // we may eventually need this, and it's already in the code

	if (NULL == key_columns[target_key_num]) {
		return 0;
	}
	keyword = get_keyword(key_columns[target_key_num], OPTIONAL_EXTRACT);
	if (NULL != keyword) {
		UNPACK_SQL_STATEMENT(value, keyword->v, value);
		temp = m_unescape_string(value->v.string_literal);
		written = snprintf(buffer, buffer_size, "%s", temp);
		assert(written < buffer_size);
		return written;
	}
	UNPACK_SQL_STATEMENT(value, key_columns[target_key_num]->columnName, value);
	columnName = value->v.reference;

	buffer_ptr = buffer;
	buffer_ptr += snprintf(buffer_ptr, buffer_size - (buffer_ptr - buffer), "keys(\"%s\")", columnName);
	*buffer_ptr++ = '\0';

	return buffer_ptr - buffer;
}
