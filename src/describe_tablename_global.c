/****************************************************************
 *								*
 * Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <assert.h>

#include "octo.h"

/* This function displays the GLOBAL that holds the table records */
void describe_tablename_global(SqlTable *table) {
	SqlOptionalKeyword *keyword;
	UNPACK_SQL_STATEMENT(keyword, table->source, keyword);
	fprintf(stdout, "Global: ");

	SqlValue *value;
	UNPACK_SQL_STATEMENT(value, keyword->v, value);

	/* The below code is similar to that in "tmpl_emit_source.ctemplate" */
	char *source_ptr;
	source_ptr = value->v.string_literal;

	boolean_t table_has_hidden_key_column;
	table_has_hidden_key_column = table_has_hidden_column(table);
	while ('\0' != *source_ptr) {
		char column[OCTO_MAX_IDENT + 1]; // Null terminator
		int  t;
		t = match_keys_expression(source_ptr, column, sizeof(column));
		assert(-1 != t);
		if (0 < t) {
			fprintf(stdout, "%s", column);
			source_ptr += t;
		} else {
			if (table_has_hidden_key_column && ('(' == *source_ptr)) {
				/* Table has a HIDDEN key column. In that case, stop at printing the global name.
				 * No need of any subscripts as the only subscript is the hidden key column name
				 * which the user has no clue about.
				 */
				break;
			}
			fprintf(stdout, "%c", *source_ptr);
			source_ptr++;
		}
	}
	return;
}
