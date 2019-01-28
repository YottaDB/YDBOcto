#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

void cleanup_sql_statement(SqlStatement *stmt)
{
	SqlTable *cur_table, *start_table;
	SqlColumn *cur_column, *start_column;
	SqlColumnList *cur_column_list, *start_column_list;
	SqlColumnAlias *column_alias;
	SqlTableAlias *table_alias;
	SqlColumnListAlias *start_cla, *cur_cla, *t_cla;
	SqlJoin *cur_join, *start_join;
	SqlInsertStatement *insert;
	SqlSelectStatement *select;
	SqlDropStatement *drop;
	SqlValue *value;
	SqlBinaryOperation *binary;
	if(stmt == NULL)
		return;
	// In each case below, after storing the pointer of the statement in
	//  a temporary variable we mark the statement as NULL, then check here
	//  to see if the statement has been marked NULL, and if so, skip it
	// This lets us play a bit fast-and-loose with the structures to
	//  avoid extra copies during runtime, and not have issues with double
	//  frees
	if(stmt->v.value == NULL)
		return;
	switch(stmt->type) {
	case table_STATEMENT:
		if(stmt->v.table) {
			cur_table = start_table = stmt->v.table;
			stmt->v.value = NULL;
			do {
				cleanup_sql_statement(cur_table->tableName);
				cleanup_sql_statement(cur_table->source);
				cleanup_sql_statement(cur_table->columns);
				if(cur_table->next == start_table) {
					free(cur_table);
					cur_table = start_table;
				} else {
					assert(cur_table->next->prev == cur_table);
					cur_table = cur_table->next;
					free(cur_table->prev);
				}
			} while(cur_table != start_table);
		}
		free(stmt);
		break;
	case select_STATEMENT:
		UNPACK_SQL_STATEMENT(select, stmt, select);
		stmt->v.value = NULL;
		if(select) {
			cleanup_sql_statement(select->select_list);
			cleanup_sql_statement(select->table_list);
			free(select);
		}
		free(stmt);
		break;
	case drop_STATEMENT:
		UNPACK_SQL_STATEMENT(drop, stmt, drop);
		stmt->v.value = NULL;
		if(stmt->v.drop) {
			cleanup_sql_statement(stmt->v.drop->table_name);
			cleanup_sql_statement(stmt->v.drop->optional_keyword);
			free(stmt->v.drop);
		}
		free(stmt);
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		stmt->v.value = NULL;
		if(value) {
			if(value->type == CALCULATED_VALUE)
				cleanup_sql_statement(value->v.calculated);
			else
				free(value->v.reference);
			free(value);
		}
		free(stmt);
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		stmt->v.value = NULL;
		if(binary) {
			cleanup_sql_statement(binary->operands[0]);
			cleanup_sql_statement(binary->operands[1]);
			free(binary);
		}
		free(stmt);
		break;
	case unary_STATEMENT:
		if(stmt->v.unary) {
			cleanup_sql_statement(stmt->v.unary->operand);
			free(stmt->v.unary);
		}
		free(stmt);
		break;
	case column_list_STATEMENT:
		UNPACK_SQL_STATEMENT(start_column_list, stmt, column_list);
		stmt->v.value = NULL;
		if(start_column_list) {
			cur_column_list = start_column_list;
			do {
				cleanup_sql_statement(cur_column_list->value);
				if(cur_column_list->next == start_column_list) {
					free(cur_column_list);
					cur_column_list = start_column_list;
				} else {
					cur_column_list = cur_column_list->next;
					free(cur_column_list->prev);
				}
			} while(cur_column_list != start_column_list);
		}
		free(stmt);
		break;
	case column_STATEMENT:
		if(stmt->v.column) {
			UNPACK_SQL_STATEMENT(cur_column, stmt, column);
			stmt->v.value = NULL;
			start_column = cur_column;
			do {
				cleanup_sql_statement(cur_column->columnName);
				//cleanup_sql_statement(cur_column->table);
				assert(cur_column->next == start_column ||
				       cur_column->next->prev == cur_column);
				if(cur_column->next == start_column) {
					free(cur_column);
					cur_column = start_column;
				} else {
					cur_column = cur_column->next;
					free(cur_column->prev);
				}
			} while(cur_column != start_column);
		}
		free(stmt);
		break;
	case join_STATEMENT:
		UNPACK_SQL_STATEMENT(start_join, stmt, join);
		stmt->v.value = NULL;
		cur_join = start_join;
		do {
			/// TODO: this should be cleaned
			// We don't want to cleanup tables themselves
			//cleanup_sql_statement(cur_join->value);
			if(cur_join->next == start_join) {
				free(cur_join);
				cur_join = start_join;
			} else {
				cur_join = cur_join->next;
				free(cur_join->prev);
			}
		} while(cur_join != start_join);
		free(stmt);
		break;
	case data_type_STATEMENT:
		/* No op */
		free(stmt);
		break;
	case constraint_type_STATEMENT:
		/* No op */
		free(stmt);
		break;
	case constraint_STATEMENT:
	case keyword_STATEMENT:
		/// TODO: this is now a double queue
		if(stmt->v.keyword) {
			cleanup_sql_statement(stmt->v.keyword->v);
			free(stmt->v.keyword);
		}
		free(stmt);
		break;
	case insert_STATEMENT:
		UNPACK_SQL_STATEMENT(insert, stmt, insert);
		cleanup_sql_statement(insert->source);
		if(insert->columns) {
			cleanup_sql_statement(insert->columns);
		}
		free(stmt);
		break;
	case column_list_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(start_cla, stmt, column_list_alias);
		stmt->v.value = NULL;
		cur_cla = start_cla;
		if(cur_cla == NULL)
			break;
		do {
			cleanup_sql_statement(cur_cla->column_list);
			cleanup_sql_statement(cur_cla->alias);
			t_cla = cur_cla->next;
			free(cur_cla);
			cur_cla = t_cla;
		} while(cur_cla != start_cla);
		break;
	case column_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		stmt->v.value = NULL;
		if(column_alias == NULL) {
			break;
		}
		// Do not cleanup columns from an alias statement, as they are
		//  just temporary pointers to columns from a long-living table
		//  structure
		//cleanup_sql_statement(column_alias->column);
		cleanup_sql_statement(column_alias->table_alias);
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		if(table_alias == NULL)
			break;
		stmt->v.value = NULL;
		// Do not cleanup tables from an alias statement, as they are
		//  just temporary pointers
		//cleanup_sql_statement(table_alias->table);
		cleanup_sql_statement(table_alias->alias);
		cleanup_sql_statement(table_alias->column_list);
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
		break;
	}
}
