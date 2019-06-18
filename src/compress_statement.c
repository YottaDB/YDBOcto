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

#define CALL_COMPRESS_HELPER(temp, value, new_value, out, out_length) \
	(temp) = compress_statement_helper((value), (out), (out_length)); \
	if((out) != NULL) { \
		(new_value) = (temp); \
		if(new_value != NULL) { \
			A2R((new_value), temp); \
		} \
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
	SqlStatement *new_stmt;
	SqlTable *table, *new_table;
	SqlColumn *cur_column, *start_column, *new_column;
	SqlColumnList *cur_column_list, *start_column_list;
	SqlColumnListAlias *start_cla, *cur_cla;
	SqlJoin *cur_join, *start_join;
	SqlValue *value, *new_value;
	SqlOptionalKeyword *start_keyword, *cur_keyword, *new_keyword;
	void *r, *ret;
	int len;
	if(stmt == NULL)
		return NULL;
	if(stmt->v.value == NULL)
		return NULL;
	if(out != NULL) {
		new_stmt = ((void*)&out[*out_length]);
		memcpy(new_stmt, stmt, sizeof(SqlStatement));
		ret = new_stmt;
	}
	*out_length += sizeof(SqlStatement);
	if(out != NULL) {
		new_stmt->v.value = ((void*)&out[*out_length]);
		A2R(new_stmt->v.value, new_stmt->v.value);
	}
	switch(stmt->type) {
	case table_STATEMENT:
		UNPACK_SQL_STATEMENT(table, stmt, table);
		if(out != NULL) {
			new_table = ((void*)&out[*out_length]);
			memcpy(new_table, table, sizeof(SqlTable));
		}
		*out_length += sizeof(SqlTable);
		/// TODO: tables should no longer be a double list
		CALL_COMPRESS_HELPER(r, table->tableName, new_table->tableName, out, out_length);
		CALL_COMPRESS_HELPER(r, table->source, new_table->source, out, out_length);
		CALL_COMPRESS_HELPER(r, table->columns, new_table->columns, out, out_length);
		CALL_COMPRESS_HELPER(r, table->delim, new_table->delim, out, out_length);
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		if(out != NULL) {
			new_value = ((void*)&out[*out_length]);
			memcpy(new_value, value, sizeof(SqlValue));
		}
		*out_length += sizeof(SqlValue);
		switch(value->type) {
		case CALCULATED_VALUE:
			CALL_COMPRESS_HELPER(r, value->v.calculated, new_value->v.calculated, out, out_length);
			break;
		default:
			len = strlen(value->v.string_literal);
			if(out != NULL) {
				memcpy(&out[*out_length], value->v.string_literal, len);
				new_value->v.string_literal = &out[*out_length];
				A2R(new_value->v.string_literal, new_value->v.string_literal);
			}
			*out_length += len;
			if(out != NULL) {
				out[*out_length] = '\0';
			}
			*out_length += 1;
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
			if(out != NULL) {
				new_column = ((void*)&out[*out_length]);
				memcpy(new_column, cur_column, sizeof(SqlColumn));
				new_column->next = new_column->prev = NULL;
			}
			*out_length += sizeof(SqlColumn);
			CALL_COMPRESS_HELPER(r, cur_column->columnName, new_column->columnName, out, out_length);
			// Don't copy table since we want to point to the parent table, which we can't
			// find here. Those relying on a compressed table should call
			// assign_table_to_columns on the decompressed result
			CALL_COMPRESS_HELPER(r, cur_column->keywords, new_column->keywords, out, out_length);
			cur_column = cur_column->next;
			if(out != NULL && cur_column != start_column) {
				new_column->next = ((void*)&out[*out_length]);
				A2R(new_column->next, new_column->next);
			}
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
			if(out != NULL) {
				new_keyword = ((void*)&out[*out_length]);
				memcpy(new_keyword, cur_keyword, sizeof(SqlOptionalKeyword));
				new_keyword->next = new_keyword->prev = NULL;
			}
			*out_length += sizeof(SqlOptionalKeyword);
			CALL_COMPRESS_HELPER(r, cur_keyword->v, new_keyword->v, out, out_length);
			cur_keyword = cur_keyword->next;
			if(out != NULL && cur_keyword != start_keyword) {
				new_keyword->next = ((void*)&out[*out_length]);
				A2R(new_keyword->next, new_keyword->next);
			}
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
	return ret;
}

