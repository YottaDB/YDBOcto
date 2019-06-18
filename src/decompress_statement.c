/* Copyright (C) 2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <assert.h>
#include "octo.h"
#include "octo_types.h"

#define CALL_DECOMPRESS_HELPER(value, out, out_length) \
	if(value != NULL) { \
		value = R2A(value); \
		decompress_statement_helper(value, out, out_length); \
	}

void *decompress_statement_helper(SqlStatement *stmt, char *out, int out_length);

SqlStatement *decompress_statement(char *buffer, int out_length) {
	return (SqlStatement*)decompress_statement_helper((SqlStatement*)buffer, buffer, out_length);
}

/*
 * Returns a pointer to a new memory location for stmt within the out buffer
 *
 * If the out buffer is NULL, doesn't copy the statement, but just counts size
 */
void *decompress_statement_helper(SqlStatement *stmt, char *out, int out_length) {
	SqlTable *table;
	SqlColumn *cur_column, *start_column;
	SqlColumnList *cur_column_list, *start_column_list;
	SqlColumnListAlias *start_cla, *cur_cla;
	SqlJoin *cur_join, *start_join;
	SqlValue *value;
	SqlOptionalKeyword *start_keyword, *cur_keyword;

	assert(((char*)stmt) < out + out_length);
	if(stmt == NULL)
		return NULL;
	// In each case below, after storing the pointer of the statement in
	//  a temporary variable we mark the statement as NULL, then check here
	//  to see if the statement has been marked NULL, and if so, skip it
	// This lets us play a bit fast-and-loose with the structures to
	//  avoid extra copies during runtime, and not have issues with double
	//  frees
	if(stmt->v.value == NULL)
		return NULL;
	stmt->v.value = R2A(stmt->v.value);
	switch(stmt->type) {
	case table_STATEMENT:
		UNPACK_SQL_STATEMENT(table, stmt, table);
		CALL_DECOMPRESS_HELPER(table->tableName, out, out_length);
		CALL_DECOMPRESS_HELPER(table->source, out, out_length);
		CALL_DECOMPRESS_HELPER(table->columns, out, out_length);
		CALL_DECOMPRESS_HELPER(table->delim, out, out_length);
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch(value->type) {
		case CALCULATED_VALUE:
			CALL_DECOMPRESS_HELPER(value->v.calculated, out, out_length);
			break;
		default:
			value->v.string_literal = R2A(value->v.string_literal);
			break;
		}
		break;
	case column_list_STATEMENT:
		UNPACK_SQL_STATEMENT(start_column_list, stmt, column_list);
		if(start_column_list) {
			cur_column_list = start_column_list;
			do {
				cur_column_list = cur_column_list->next;
			} while(cur_column_list != start_column_list);
		}
		break;
	case column_STATEMENT:
		UNPACK_SQL_STATEMENT(cur_column, stmt, column);
		start_column = cur_column;
		do {
			CALL_DECOMPRESS_HELPER(cur_column->columnName, out, out_length);
			// Don't copy table
			CALL_DECOMPRESS_HELPER(cur_column->keywords, out, out_length);
			if(cur_column->next == 0) {
				cur_column->next = start_column;
			} else {
				cur_column->next = R2A(cur_column->next);
			}
			cur_column->next->prev = cur_column;
			cur_column = cur_column->next;
		} while(cur_column != start_column);
		break;
	case join_STATEMENT:
		UNPACK_SQL_STATEMENT(start_join, stmt, join);
		do {
			cur_join = cur_join->next;
		} while(cur_join != start_join);
		break;
	case keyword_STATEMENT:
		UNPACK_SQL_STATEMENT(start_keyword, stmt, keyword);
		cur_keyword = start_keyword;
		do {
			CALL_DECOMPRESS_HELPER(cur_keyword->v, out, out_length);
			if(cur_keyword->next == 0) {
				cur_keyword->next = start_keyword;
			} else {
				cur_keyword->next = R2A(cur_keyword->next);
			}
			cur_keyword->next->prev = cur_keyword;
			cur_keyword = cur_keyword->next;
		} while(cur_keyword != start_keyword);
		break;
	case column_list_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(start_cla, stmt, column_list_alias);
		cur_cla = start_cla;
		if(cur_cla == NULL)
			break;
		do {
			cur_cla = cur_cla->next;
		} while(cur_cla != start_cla);
		break;
	case begin_STATEMENT:
	case binary_STATEMENT:
	case cas_STATEMENT:
	case cas_branch_STATEMENT:
	case column_alias_STATEMENT:
	case commit_STATEMENT:
	case constraint_STATEMENT:
	case constraint_type_STATEMENT:
	case data_type_STATEMENT:
	case drop_STATEMENT:
	case function_call_STATEMENT:
	case insert_STATEMENT:
	case join_type_STATEMENT:
	case no_data_STATEMENT:
	case select_STATEMENT:
	case set_STATEMENT:
	case set_operation_STATEMENT:
	case show_STATEMENT:
	case table_alias_STATEMENT:
	case unary_STATEMENT:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
		break;
	case invalid_STATEMENT:
		assert(stmt->type != invalid_STATEMENT);
		break;
	}
	return stmt;
}

