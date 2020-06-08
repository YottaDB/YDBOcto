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
	assert(0 != *out_length);
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
	SqlColumn			*cur_column, *start_column, *new_column;
	SqlOptionalKeyword		*start_keyword, *cur_keyword, *new_keyword;
	SqlStatement			*new_stmt;
	SqlTable			*table, *new_table;
	SqlFunction			*function, *new_function;
	SqlParameterTypeList		*new_parameter_type_list, *cur_parameter_type_list, *start_parameter_type_list;
	SqlValue			*value, *new_value;
	int				len;
	void				*r, *ret;

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
		if (data_type_STATEMENT == new_stmt->type) {
			/* In this case, the relevant data is the SqlDataType enum from the union member of `stmt`,
			 * i.e. NOT a pointer. So, do not perform the A2R conversion and just return as is.
			 * See similar note in decompress_statement.c.
			 */
			return ret;
		}
		new_stmt->v.value = ((void*)&out[*out_length]);
		A2R(new_stmt->v.value, new_stmt->v.value);
	}
	switch (stmt->type) {
	case create_table_STATEMENT:
		UNPACK_SQL_STATEMENT(table, stmt, create_table);
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
	case create_function_STATEMENT:
		UNPACK_SQL_STATEMENT(function, stmt, create_function);
		if (NULL != out) {
			new_function = ((void*)&out[*out_length]);
			memcpy(new_function, function, sizeof(SqlFunction));
		}
		*out_length += sizeof(SqlFunction);
		CALL_COMPRESS_HELPER(r, function->function_name, new_function->function_name, out, out_length);
		CALL_COMPRESS_HELPER(r, function->parameter_type_list, new_function->parameter_type_list, out, out_length);
		CALL_COMPRESS_HELPER(r, function->return_type, new_function->return_type, out, out_length);
		CALL_COMPRESS_HELPER(r, function->extrinsic_function, new_function->extrinsic_function, out, out_length);
		break;
	case parameter_type_list_STATEMENT:
		UNPACK_SQL_STATEMENT(cur_parameter_type_list, stmt, parameter_type_list);
		if (NULL == cur_parameter_type_list) {
			// No parameter types were specified, nothing to compress
			break;
		}
		start_parameter_type_list = cur_parameter_type_list;
		do {
			if (NULL != out) {
				new_parameter_type_list = ((void*)&out[*out_length]);
				memcpy(new_parameter_type_list, cur_parameter_type_list, sizeof(SqlParameterTypeList));
				new_parameter_type_list->next = new_parameter_type_list->prev = NULL;
			}
			*out_length += sizeof(SqlParameterTypeList);

			CALL_COMPRESS_HELPER(r, cur_parameter_type_list->data_type, new_parameter_type_list->data_type,
					out, out_length);
			cur_parameter_type_list = cur_parameter_type_list->next;
			if ((NULL != out) && (cur_parameter_type_list != start_parameter_type_list)) {
				new_parameter_type_list->next = ((void*)&out[*out_length]);
				A2R(new_parameter_type_list->next, new_parameter_type_list->next);
			}
		} while (cur_parameter_type_list != start_parameter_type_list);
		break;
	case data_type_STATEMENT:
		// Hit this case only for the initial length count, i.e. (NULL == out). See note on data_type_STATEMENTs above.
		assert(NULL == out);
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
