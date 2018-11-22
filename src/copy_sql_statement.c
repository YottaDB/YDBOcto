/* Copyright (C) 2018 YottaDB, LLC
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
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

SqlStatement *copy_sql_statement(SqlStatement *stmt) {
	SqlTable *cur_table, *start_table, *table;
	SqlTableAlias *table_alias, *new_table_alias;
	SqlColumn *cur_column, *start_column, *new_column, *t_column;
	SqlColumnList *cur_column_list, *start_column_list, *new_column_list, *t_column_list;
	SqlJoin *cur_join, *start_join, *new_join, *t_join;
	SqlInsertStatement *insert;
	SqlStatement *ret;
	SqlSelectStatement *select;
	SqlDropStatement *drop;
	SqlValue *value;
	SqlBinaryOperation *binary;
	SqlUnaryOperation *unary;
	SqlOptionalKeyword *cur_keyword, *start_keyword, *new_keyword, *t_keyword;
	SqlColumnListAlias *new_cl_alias, *cur_cl_alias, *start_cl_alias, *t_cl_alias;
	SqlColumnAlias *new_column_alias, *cur_column_alias, *start_column_alias, *t_column_alias;
	int len;

	if(stmt == NULL)
		return NULL;
	ret = (SqlStatement*)malloc(sizeof(SqlStatement));
	memset(ret, 0, sizeof(SqlStatement));
	ret->type = stmt->type;
	switch(stmt->type)
	{
	case table_STATEMENT:
		// Although there is a dq within table, we should not copy it,
		//  otherwise we might copy the entire list of tables for
		//  no reason
		/// TODO: fill this out if needed
		UNPACK_SQL_STATEMENT(cur_table, stmt, table);
		SQL_STATEMENT(ret, table_STATEMENT);
		ret->v.table = copy_sql_table(cur_table);
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		MALLOC_STATEMENT(ret, table_alias, SqlTableAlias);
		new_table_alias = ret->v.table_alias;
		new_table_alias->table = copy_sql_statement(table_alias->table);
		new_table_alias->alias = copy_sql_statement(table_alias->alias);
		new_table_alias->unique_id = table_alias->unique_id;
		// the column list has a reference to this table, so don't copy it
		//new_table_alias->column_list = copy_sql_statement(table_alias->column_list);
		new_table_alias->column_list = table_alias->column_list;
		break;
	case select_STATEMENT:
		select = stmt->v.select;
		MALLOC_STATEMENT(ret, select, SqlSelectStatement);
		ret->v.select->select_list = copy_sql_statement(select->select_list);
		ret->v.select->table_list = copy_sql_statement(select->table_list);
		break;
	case drop_STATEMENT:
		drop = stmt->v.drop;
		MALLOC_STATEMENT(ret, drop, SqlDropStatement);
		ret->v.drop->table_name = copy_sql_statement(drop->table_name);
		ret->v.drop->optional_keyword = copy_sql_statement(drop->optional_keyword);
		break;
	case value_STATEMENT:
		value = stmt->v.value;
		MALLOC_STATEMENT(ret, value, SqlValue);
		ret->v.value->type = value->type;
		if(value->type == CALCULATED_VALUE) {
			ret->v.value->v.calculated = copy_sql_statement(value->v.calculated);
		} else {
			len = strlen(value->v.reference) + 1;
			ret->v.value->v.reference = malloc(len);
			memcpy(ret->v.value->v.reference, value->v.reference, len);
		}
		break;
	case binary_STATEMENT:
		binary = stmt->v.binary;
		MALLOC_STATEMENT(ret, binary, SqlBinaryOperation);
		*ret->v.binary = *binary;
		ret->v.binary->operands[0] = copy_sql_statement(binary->operands[0]);
		ret->v.binary->operands[1] = copy_sql_statement(binary->operands[1]);
		break;
	case unary_STATEMENT:
		unary = stmt->v.unary;
		MALLOC_STATEMENT(ret, unary, SqlUnaryOperation);
		*ret->v.unary = *unary;
		ret->v.unary->operand = copy_sql_statement(unary->operand);
		break;
	case column_list_STATEMENT:
		UNPACK_SQL_STATEMENT(start_column_list, stmt, column_list);
		//MALLOC_STATEMENT(ret, column_list, SqlColumnList);
		SQL_STATEMENT(ret, column_list_STATEMENT);
		if(start_column_list) {
			cur_column_list = start_column_list;
			do {
				new_column_list = (SqlColumnList*)malloc(sizeof(SqlColumnList));
				dqinit(new_column_list);
				new_column_list->value = copy_sql_statement(cur_column_list->value);
				if(ret->v.column_list == NULL) {
					ret->v.column_list = new_column_list;
				} else {
					dqinsert(new_column_list, ret->v.column_list, t_column_list);
				}
				cur_column_list = cur_column_list->next;
			} while(cur_column_list != start_column_list);
		}
		break;
	case column_list_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(start_cl_alias, stmt, column_list_alias);
		SQL_STATEMENT(ret, column_list_alias_STATEMENT);
		if(start_cl_alias) {
			cur_cl_alias = start_cl_alias;
			do {
				new_cl_alias = (SqlColumnListAlias*)malloc(sizeof(SqlColumnListAlias));
				memset(new_cl_alias, 0, sizeof(SqlColumnListAlias));
				dqinit(new_cl_alias);
				new_cl_alias->column_list = copy_sql_statement(cur_cl_alias->column_list);
				new_cl_alias->alias = copy_sql_statement(cur_cl_alias->alias);
				new_cl_alias->keywords = copy_sql_statement(new_cl_alias->keywords);
				if(ret->v.column_list_alias == NULL) {
					ret->v.column_list_alias = new_cl_alias;
				} else {
					dqinsert(new_cl_alias, ret->v.column_list_alias, t_cl_alias);
				}
				cur_cl_alias = cur_cl_alias->next;
			} while(cur_cl_alias != start_cl_alias);
		}
		break;
	case column_STATEMENT:
		// Columns should only be copied as part of a table copy, in copy_sql_table
		//  Otherwise, they should be wrapped in a column_alias
		assert(FALSE);
		/*		if(stmt->v.column) {
			UNPACK_SQL_STATEMENT(cur_column, stmt, column);
			start_column = cur_column;
			do {
				new_column = (SqlColumn*)malloc(sizeof(SqlColumn));
				*new_column = *cur_column;
				dqinit(new_column);
				new_column->columnName = copy_sql_statement(cur_column->columnName);
				// This gives us trouble with a table copying the column itself;
				//  just setup a pointer
				new_column->table = copy_sql_statement(cur_column->table);
				//SQL_STATEMENT(new_column->table, table_STATEMENT);
				//new_column->table->v.table = cur_column->table->v.table;
				//new_column->table = cur_column->table;
				new_column->keywords = copy_sql_statement(cur_column->keywords);
				if(ret->v.column == NULL) {
					MALLOC_STATEMENT(ret, column, SqlColumn);
					ret->v.column = new_column;
				} else {
					dqinsert(new_column, ret->v.column, t_column);
				}
				cur_column = cur_column->next;
			} while(cur_column != start_column);
			}*/
		break;
	case column_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(start_column_alias, stmt, column_alias);
		MALLOC_STATEMENT(ret, column_alias, SqlColumnAlias);
		if(start_column_alias) {
			new_column_alias = ret->v.column_alias;
			SQL_STATEMENT(new_column_alias->column, column_STATEMENT);
			//*new_column_alias->column = *start_column_alias->column;
			new_column_alias->table_alias = copy_sql_statement(start_column_alias->table_alias);
			// Find the correct column in the table
			UNPACK_SQL_STATEMENT(table_alias, new_column_alias->table_alias, table_alias);
			UNPACK_SQL_STATEMENT(table, table_alias->table, table);
			UNPACK_SQL_STATEMENT(value, start_column_alias->column->v.column->columnName, value);
			PACK_SQL_STATEMENT(new_column_alias->column, find_column(value->v.string_literal, table), column);
		}

		break;

	case join_STATEMENT:
		UNPACK_SQL_STATEMENT(start_join, stmt, join);
		//MALLOC_STATEMENT(ret, join, SqlJoin);
		cur_join = start_join;
		do {
			new_join = (SqlJoin*)malloc(sizeof(SqlJoin));
			memset(new_join, 0, sizeof(SqlJoin));
			dqinit(new_join);
			new_join->value = copy_sql_statement(cur_join->value);
			if(ret->v.join == NULL) {
				ret->v.join = new_join;
			} else {
				dqinsert(new_join, ret->v.join, t_join);
			}
			cur_join = cur_join->next;
		} while(cur_join != start_join);
		break;
	case data_type_STATEMENT:
		*ret = *stmt;
		break;
	case constraint_type_STATEMENT:
		*ret = *stmt;
		break;
	case constraint_STATEMENT:
	case keyword_STATEMENT:
		if(stmt->v.keyword) {
			UNPACK_SQL_STATEMENT(start_keyword, stmt, keyword);
			cur_keyword = start_keyword;
			do {
				new_keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
				*new_keyword = *cur_keyword;
				dqinit(new_keyword);
				new_keyword->v = copy_sql_statement(cur_keyword->v);
				if(ret->v.keyword) {
					dqinsert(ret->v.keyword, new_keyword, t_keyword);
				} else {
					ret->v.keyword = new_keyword;
				}
				cur_keyword = cur_keyword->next;
			} while(cur_keyword != start_keyword);
		} else {
			// Does this happen? It shouldn't, I don't think
			assert(FALSE);
			if(stmt->type == keyword_STATEMENT) {
				MALLOC_STATEMENT(ret, keyword, SqlOptionalKeyword);
			} else {
				MALLOC_STATEMENT(ret, constraint, SqlOptionalKeyword);
			}
			dqinit(ret->v.keyword);
		}
		break;
	case insert_STATEMENT:
		UNPACK_SQL_STATEMENT(insert, stmt, insert);
		MALLOC_STATEMENT(ret, insert, SqlInsertStatement);
		ret->v.insert->source = copy_sql_statement(insert->source);
		ret->v.insert->columns = copy_sql_statement(insert->columns);
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
		break;
	}
	return ret;
};
