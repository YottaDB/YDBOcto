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

#include <assert.h>
#include "octo.h"
#include "octo_types.h"

#define CALL_COMPRESS_HELPER(temp, value, new_value, out, out_length)		\
{										\
	(temp) = compress_statement_helper((value), (out), (out_length));	\
	if (NULL != (out)) {							\
		(new_value) = (temp);						\
		if (NULL != new_value) {					\
			A2R((new_value), temp);					\
		}								\
	}									\
}

void *compress_statement_helper(SqlStatement *stmt, char *out, int *out_length);

void compress_statement(SqlStatement *stmt, char **out, int *out_length) {
	*out_length = 0;
	compress_statement_helper(stmt, NULL, out_length);
	*out = malloc(*out_length);
	*out_length = 0;
	compress_statement_helper(stmt, *out, out_length);
}

/*
 * Returns a pointer to a new memory location for stmt within the out buffer
 *
 * If the out buffer is NULL, doesn't copy the statement, but just counts size
 */
void *compress_statement_helper(SqlStatement *stmt, char *out, int *out_length) {
	SqlColumn		*cur_column, *start_column, *new_column;
	SqlOptionalKeyword	*start_keyword, *cur_keyword, *new_keyword;
	SqlStatement		*new_stmt;
	SqlTable		*table, *new_table;
	SqlValue		*value, *new_value;
	int			len;
	void			*r, *ret;

	if ((NULL == stmt) || (NULL == stmt->v.value))
		return NULL;
	if (NULL != out) {
		new_stmt = ((void*)&out[*out_length]);
		memcpy(new_stmt, stmt, sizeof(SqlStatement));
		ret = new_stmt;
	} else {
		ret = NULL;
	}
	*out_length += sizeof(SqlStatement);
	if (NULL != out) {
		new_stmt->v.value = ((void*)&out[*out_length]);
		A2R(new_stmt->v.value, new_stmt->v.value);
	}
	switch(stmt->type) {
	case table_STATEMENT:
		UNPACK_SQL_STATEMENT(table, stmt, table);
		if (NULL != out) {
			new_table = ((void*)&out[*out_length]);
			memcpy(new_table, table, sizeof(SqlTable));
		}
		*out_length += sizeof(SqlTable);
		/// TODO: tables should no longer be a double list
		CALL_COMPRESS_HELPER(r, table->tableName, new_table->tableName, out, out_length);
		CALL_COMPRESS_HELPER(r, table->source, new_table->source, out, out_length);
		CALL_COMPRESS_HELPER(r, table->columns, new_table->columns, out, out_length);
		CALL_COMPRESS_HELPER(r, table->delim, new_table->delim, out, out_length);
		/* table->oid is not a pointer value so no need to call CALL_COMPRESS_HELPER on this member */
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		if (NULL != out) {
			new_value = ((void*)&out[*out_length]);
			memcpy(new_value, value, sizeof(SqlValue));
		}
		*out_length += sizeof(SqlValue);
		len = strlen(value->v.string_literal);
		if (NULL != out) {
			memcpy(&out[*out_length], value->v.string_literal, len);
			new_value->v.string_literal = &out[*out_length];
			A2R(new_value->v.string_literal, new_value->v.string_literal);
		}
		*out_length += len;
		if (NULL != out) {
			out[*out_length] = '\0';
		}
		*out_length += 1;
		break;
	case column_STATEMENT:
		UNPACK_SQL_STATEMENT(cur_column, stmt, column);
		start_column = cur_column;
		do {
			if (NULL != out) {
				new_column = ((void*)&out[*out_length]);
				memcpy(new_column, cur_column, sizeof(SqlColumn));
				new_column->next = new_column->prev = NULL;
				new_column->table = (void *)0;	/* offset 0 : table is first element in compressed structure
								 *            which means a relative offset of 0.
								 */
			}
			*out_length += sizeof(SqlColumn);
			CALL_COMPRESS_HELPER(r, cur_column->columnName, new_column->columnName, out, out_length);
			CALL_COMPRESS_HELPER(r, cur_column->keywords, new_column->keywords, out, out_length);
			cur_column = cur_column->next;
			if ((NULL != out) && (cur_column != start_column)) {
				new_column->next = ((void*)&out[*out_length]);
				A2R(new_column->next, new_column->next);
			}
		} while (cur_column != start_column);
		break;
	case keyword_STATEMENT:
		UNPACK_SQL_STATEMENT(start_keyword, stmt, keyword);
		cur_keyword = start_keyword;
		do {
			if (NULL != out) {
				new_keyword = ((void*)&out[*out_length]);
				memcpy(new_keyword, cur_keyword, sizeof(SqlOptionalKeyword));
				new_keyword->next = new_keyword->prev = NULL;
			}
			*out_length += sizeof(SqlOptionalKeyword);
			CALL_COMPRESS_HELPER(r, cur_keyword->v, new_keyword->v, out, out_length);
			cur_keyword = cur_keyword->next;
			if ((NULL != out) && (cur_keyword != start_keyword)) {
				new_keyword->next = ((void*)&out[*out_length]);
				A2R(new_keyword->next, new_keyword->next);
			}
		} while (cur_keyword != start_keyword);
		break;
	default:
		assert(FALSE);
		FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
		return NULL;
		break;
	}
	return ret;
}
