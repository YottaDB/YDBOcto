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

#define CALL_DECOMPRESS_HELPER(value, out, out_length)                       \
	{                                                                    \
		if (value != NULL) {                                         \
			value = R2A(value);                                  \
			decompress_statement_helper(value, out, out_length); \
		}                                                            \
	}

void *decompress_statement_helper(SqlStatement *stmt, char *out, int out_length);

SqlStatement *decompress_statement(char *buffer, int out_length) {
	return (SqlStatement *)decompress_statement_helper((SqlStatement *)buffer, buffer, out_length);
}

/*
 * Returns a pointer to a new memory location for stmt within the out buffer
 *
 * If the out buffer is NULL, doesn't copy the statement, but just counts size
 */
void *decompress_statement_helper(SqlStatement *stmt, char *out, int out_length) {
	SqlTable *	      table;
	SqlColumn *	      cur_column, *start_column;
	SqlValue *	      value;
	SqlOptionalKeyword *  start_keyword, *cur_keyword;
	SqlFunction *	      function;
	SqlParameterTypeList *cur_parameter_type_list, *start_parameter_type_list;

	assert(((char *)stmt) < out + out_length);
	if (NULL == stmt) {
		return NULL;
	}
	// In each case below, after storing the pointer of the statement in
	//  a temporary variable we mark the statement as NULL, then check here
	//  to see if the statement has been marked NULL, and if so, skip it
	// This lets us play a bit fast-and-loose with the structures to
	//  avoid extra copies during runtime, and not have issues with double
	//  frees
	if (NULL == stmt->v.value) {
		return NULL;
	}
	if (data_type_STATEMENT == stmt->type) {
		/* Relevant data is the SqlDataType enum in the `stmt` union member, which is NOT a pointer.
		 * So, do not do R2A conversion and just return as-is. See similar note in compress_statement.c.
		 */
		return stmt;
	}
	stmt->v.value = R2A(stmt->v.value);
	switch (stmt->type) {
	case create_table_STATEMENT:
		UNPACK_SQL_STATEMENT(table, stmt, create_table);
		CALL_DECOMPRESS_HELPER(table->tableName, out, out_length);
		CALL_DECOMPRESS_HELPER(table->source, out, out_length);
		CALL_DECOMPRESS_HELPER(table->columns, out, out_length);
		CALL_DECOMPRESS_HELPER(table->delim, out, out_length);
		CALL_DECOMPRESS_HELPER(table->nullchar, out, out_length);
		/* table->oid is not a pointer value so no need to call CALL_DECOMPRESS_HELPER on this member */
		break;
	case create_function_STATEMENT:
		UNPACK_SQL_STATEMENT(function, stmt, create_function);
		CALL_DECOMPRESS_HELPER(function->function_name, out, out_length);
		CALL_DECOMPRESS_HELPER(function->parameter_type_list, out, out_length);
		CALL_DECOMPRESS_HELPER(function->return_type, out, out_length);
		CALL_DECOMPRESS_HELPER(function->extrinsic_function, out, out_length);
		break;
	case parameter_type_list_STATEMENT:
		UNPACK_SQL_STATEMENT(cur_parameter_type_list, stmt, parameter_type_list);
		if (NULL == cur_parameter_type_list) {
			// No parameter types were specified, nothing to decompress
			break;
		}
		start_parameter_type_list = cur_parameter_type_list;
		do {
			if (0 == cur_parameter_type_list->next) {
				cur_parameter_type_list->next = start_parameter_type_list;
			} else {
				cur_parameter_type_list->next = R2A(cur_parameter_type_list->next);
			}
			CALL_DECOMPRESS_HELPER(cur_parameter_type_list->data_type, out, out_length);
			cur_parameter_type_list->next->prev = cur_parameter_type_list;
			cur_parameter_type_list = cur_parameter_type_list->next;
		} while (cur_parameter_type_list != start_parameter_type_list);
		break;
	case data_type_STATEMENT:
		// No pointers to restore, so just break here
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		value->v.string_literal = R2A(value->v.string_literal);
		break;
	case column_STATEMENT:
		UNPACK_SQL_STATEMENT(cur_column, stmt, column);
		start_column = cur_column;
		do {
			CALL_DECOMPRESS_HELPER(cur_column->columnName, out, out_length);
			// Don't copy table
			CALL_DECOMPRESS_HELPER(cur_column->keywords, out, out_length);
			if (0 == cur_column->next) {
				cur_column->next = start_column;
			} else {
				cur_column->next = R2A(cur_column->next);
			}
			cur_column->table = (SqlStatement *)out; /* table is first element in compressed structure i.e. "out" */
			cur_column->next->prev = cur_column;
			cur_column = cur_column->next;
		} while (cur_column != start_column);
		break;
	case keyword_STATEMENT:
		UNPACK_SQL_STATEMENT(start_keyword, stmt, keyword);
		cur_keyword = start_keyword;
		do {
			CALL_DECOMPRESS_HELPER(cur_keyword->v, out, out_length);
			if (cur_keyword->next == 0) {
				cur_keyword->next = start_keyword;
			} else {
				cur_keyword->next = R2A(cur_keyword->next);
			}
			cur_keyword->next->prev = cur_keyword;
			cur_keyword = cur_keyword->next;
		} while (cur_keyword != start_keyword);
		break;
	default:
		assert(FALSE);
		FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
		return NULL;
		break;
	}
	return stmt;
}
