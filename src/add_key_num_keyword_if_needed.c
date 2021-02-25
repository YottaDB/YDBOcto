/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"

/* If no column in input table (pointed to by "table_statement" variable) is identified as a KEY column
 * (i.e. no PRIMARY KEY or KEY NUM keyword, add KEY NUM keyword to all columns in table.
 * Returns
 *	0 on success.
 *	1 on failure.
 */
int add_key_num_keyword_if_needed(SqlStatement *table_statement) {
	SqlTable * table;
	SqlColumn *key_columns[MAX_KEY_COUNT];
	int	   copied, max_key;

	UNPACK_SQL_STATEMENT(table, table_statement, create_table);

	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn *));
	max_key = get_key_columns(table, key_columns);
	/* max_key >= 0 is the number of key columns found
	 * -1 indicates no keys found to make all columns keys
	 * -2 indicates there was some error in the key columns
	 */
	if (max_key == -1) {
		char	   key_num_buffer[INT32_TO_STRING_MAX];
		int	   i, len;
		char *	   out_buffer;
		SqlColumn *cur_column, *start_column;

		UNPACK_SQL_STATEMENT(start_column, table->columns, column);
		cur_column = start_column;
		i = 0;
		do {
			SqlStatement *	    statement;
			SqlOptionalKeyword *keyword, *t_keyword;

			// Construct the key num keyword
			SQL_STATEMENT(statement, keyword_STATEMENT);
			MALLOC_STATEMENT(statement, keyword, SqlOptionalKeyword);
			keyword = statement->v.keyword;
			keyword->keyword = OPTIONAL_KEY_NUM;
			// key num value is index of key in table
			copied = snprintf(key_num_buffer, INT32_TO_STRING_MAX, "%d", i);
			assert(INT32_TO_STRING_MAX > copied);
			UNUSED(
			    copied); /* Only used for asserts, so use macro to prevent compiler warnings in RelWithDebInfo builds */
			len = strlen(key_num_buffer);
			out_buffer = octo_cmalloc(memory_chunks, len + 1);
			strncpy(out_buffer, key_num_buffer, len + 1);
			SQL_VALUE_STATEMENT(keyword->v, INTEGER_LITERAL, out_buffer);
			// Insert statement into column keyword list
			dqinit(keyword);
			UNPACK_SQL_STATEMENT(t_keyword, cur_column->keywords, keyword);
			dqappend(t_keyword, keyword);
			// Walk to next key and increment index
			cur_column = cur_column->next;
			i++;
		} while (cur_column != start_column);
		// Get the new key columns
		max_key = get_key_columns(table, key_columns);
		assert(max_key == i - 1);
		UNUSED(max_key); // needed to avoid DeadStores warning for Release builds
	} else if (-2 == max_key) {
		return 1; // non-zero return value is an error (i.e causes YYABORT in caller)
	}
	return 0;
}
