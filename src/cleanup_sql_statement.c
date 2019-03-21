#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

void cleanup_sql_statement_helper(SqlStatement *stmt, void *items_to_free[], size_t *max_item_to_free);

void cleanup_sql_statement(SqlStatement *stmt) {
	/// TODO: it would be nice to have a C++ vector or something here so we don't
	// have to worry about resizing if this get's filled
	// Even a hash set would be great, to ensure we don't free the same thing twice
	void *items_to_free[MAX_STR_CONST];
	size_t max_item_to_free, i, j;

	memset(items_to_free, 0, sizeof(void*) * MAX_STR_CONST);
	max_item_to_free = 0;

	cleanup_sql_statement_helper(stmt, items_to_free, &max_item_to_free);

	for(i = 0; i < max_item_to_free; i++) {
		if(items_to_free[i] == NULL)
			continue;
		free(items_to_free[i]);
		for(j = i+1; j < max_item_to_free; j++) {
			if(items_to_free[j] == items_to_free[i])
				items_to_free[j] = NULL;
		}
	}
}

void cleanup_sql_statement_helper(SqlStatement *stmt, void *items_to_free[], size_t *max_item_to_free)
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
	SqlUnaryOperation *unary;
	SqlFunctionCall *function_call;
	SqlOptionalKeyword *start_keyword, *cur_keyword;
	SqlSetOperation *set_operation;
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
	assert(*max_item_to_free < MAX_STR_CONST);
	switch(stmt->type) {
	case table_STATEMENT:
		items_to_free[(*max_item_to_free)++] = stmt;
		if(stmt->v.table) {
			cur_table = start_table = stmt->v.table;
			stmt->v.value = NULL;
			do {
				cleanup_sql_statement_helper(cur_table->tableName, items_to_free, max_item_to_free);
				cleanup_sql_statement_helper(cur_table->source, items_to_free, max_item_to_free);
				cleanup_sql_statement_helper(cur_table->columns, items_to_free, max_item_to_free);
				// source will be part of a circular list for delim, which is just for ease of access.
				// Free the associated SqlStatement though
				cleanup_sql_statement_helper(cur_table->delim, items_to_free, max_item_to_free);
				//items_to_free[(*max_item_to_free)++] = cur_table->delim;
				if(cur_table->next == start_table) {
					items_to_free[(*max_item_to_free)++] = cur_table;
					cur_table = start_table;
				} else {
					assert(cur_table->next->prev == cur_table);
					cur_table = cur_table->next;
					items_to_free[(*max_item_to_free)++] = cur_table->prev;
				}
			} while(cur_table != start_table);
		}
		break;
	case select_STATEMENT:
		UNPACK_SQL_STATEMENT(select, stmt, select);
		items_to_free[(*max_item_to_free)++] = stmt;
		stmt->v.value = NULL;
		if(select) {
			cleanup_sql_statement_helper(select->select_list, items_to_free, max_item_to_free);
			cleanup_sql_statement_helper(select->table_list, items_to_free, max_item_to_free);
			cleanup_sql_statement_helper(select->where_expression, items_to_free, max_item_to_free);
			cleanup_sql_statement_helper(select->order_expression, items_to_free, max_item_to_free);
			cleanup_sql_statement_helper(select->optional_words, items_to_free, max_item_to_free);
			cleanup_sql_statement_helper(select->set_operation, items_to_free, max_item_to_free);
			items_to_free[(*max_item_to_free)++] = select;
		}
		break;
	case drop_STATEMENT:
		UNPACK_SQL_STATEMENT(drop, stmt, drop);
		items_to_free[(*max_item_to_free)++] = stmt;
		stmt->v.value = NULL;
		if(drop) {
			cleanup_sql_statement_helper(drop->table_name, items_to_free, max_item_to_free);
			cleanup_sql_statement_helper(drop->optional_keyword, items_to_free, max_item_to_free);
			items_to_free[(*max_item_to_free)++] = drop;
		}
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		items_to_free[(*max_item_to_free)++] = stmt;
		stmt->v.value = NULL;
		if(value) {
			if(value->type == CALCULATED_VALUE)
				cleanup_sql_statement_helper(value->v.calculated, items_to_free, max_item_to_free);
			else
				items_to_free[(*max_item_to_free)++] = value->v.reference;
			items_to_free[(*max_item_to_free)++] = value;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		items_to_free[(*max_item_to_free)++] = stmt;
		stmt->v.value = NULL;
		if(binary) {
			cleanup_sql_statement_helper(binary->operands[0], items_to_free, max_item_to_free);
			cleanup_sql_statement_helper(binary->operands[1], items_to_free, max_item_to_free);
			items_to_free[(*max_item_to_free)++] = binary;
		}
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		items_to_free[(*max_item_to_free)++] = stmt;
		stmt->v.value = NULL;
		if(unary) {
			cleanup_sql_statement_helper(unary->operand, items_to_free, max_item_to_free);
			items_to_free[(*max_item_to_free)++] = unary;
		}
		break;
	case column_list_STATEMENT:
		UNPACK_SQL_STATEMENT(start_column_list, stmt, column_list);
		items_to_free[(*max_item_to_free)++] = stmt;
		stmt->v.value = NULL;
		if(start_column_list) {
			cur_column_list = start_column_list;
			do {
				cleanup_sql_statement_helper(cur_column_list->value, items_to_free, max_item_to_free);
				if(cur_column_list->next == start_column_list) {
					items_to_free[(*max_item_to_free)++] = cur_column_list;
					cur_column_list = start_column_list;
				} else {
					cur_column_list = cur_column_list->next;
					items_to_free[(*max_item_to_free)++] = cur_column_list->prev;
				}
			} while(cur_column_list != start_column_list);
		}
		break;
	case column_STATEMENT:
		items_to_free[(*max_item_to_free)++] = stmt;
		if(stmt->v.column) {
			UNPACK_SQL_STATEMENT(cur_column, stmt, column);
			stmt->v.value = NULL;
			start_column = cur_column;
			do {
				cleanup_sql_statement_helper(cur_column->columnName, items_to_free, max_item_to_free);
				cleanup_sql_statement_helper(cur_column->keywords, items_to_free, max_item_to_free);
				// This is generally not a copy of the table, but just a reference to it
				// We still need to free the SqlStatement wrapping it
				//cleanup_sql_statement_helper(cur_column->table, items_to_free, max_item_to_free);
				items_to_free[(*max_item_to_free)++] = cur_column->table;
				assert(cur_column->next == start_column ||
				       cur_column->next->prev == cur_column);
				if(cur_column->next == start_column) {
					items_to_free[(*max_item_to_free)++] = cur_column;
					cur_column = start_column;
				} else {
					cur_column = cur_column->next;
					items_to_free[(*max_item_to_free)++] = cur_column->prev;
				}
			} while(cur_column != start_column);
		}
		break;
	case join_STATEMENT:
		UNPACK_SQL_STATEMENT(start_join, stmt, join);
		items_to_free[(*max_item_to_free)++] = stmt;
		stmt->v.value = NULL;
		cur_join = start_join;
		do {
			cleanup_sql_statement_helper(cur_join->value, items_to_free, max_item_to_free);
			cleanup_sql_statement_helper(cur_join->condition, items_to_free, max_item_to_free);
			if(cur_join->next == start_join) {
				items_to_free[(*max_item_to_free)++] = cur_join;
				cur_join = start_join;
			} else {
				cur_join = cur_join->next;
				items_to_free[(*max_item_to_free)++] = cur_join->prev;
			}
		} while(cur_join != start_join);
		break;
	case data_type_STATEMENT:
		/* No op */
		items_to_free[(*max_item_to_free)++] = stmt;
		break;
	case constraint_type_STATEMENT:
		/* No op */
		items_to_free[(*max_item_to_free)++] = stmt;
		break;
	case constraint_STATEMENT:
		assert(FALSE);
		break;
	case keyword_STATEMENT:
		UNPACK_SQL_STATEMENT(start_keyword, stmt, keyword);
		items_to_free[(*max_item_to_free)++] = stmt;
		stmt->v.value = NULL;
		cur_keyword = start_keyword;
		do {
			assert(cur_keyword != NULL);
			cleanup_sql_statement_helper(cur_keyword->v, items_to_free, max_item_to_free);
			if(cur_keyword->next == start_keyword) {
				items_to_free[(*max_item_to_free)++] = cur_keyword;
				cur_keyword = start_keyword;
			} else {
				cur_keyword = cur_keyword->next;
				items_to_free[(*max_item_to_free)++] = cur_keyword->prev;
			}
		} while(cur_keyword != start_keyword);
		break;
	case insert_STATEMENT:
		UNPACK_SQL_STATEMENT(insert, stmt, insert);
		items_to_free[(*max_item_to_free)++] = stmt;
		stmt->v.value = NULL;
		cleanup_sql_statement_helper(insert->source, items_to_free, max_item_to_free);
		//cleanup_sql_statement_helper(insert->destination, items_to_free, max_item_to_free);
		if(insert->columns) {
			cleanup_sql_statement_helper(insert->columns, items_to_free, max_item_to_free);
		}
		break;
	case column_list_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(start_cla, stmt, column_list_alias);
		items_to_free[(*max_item_to_free)++] = stmt;
		stmt->v.value = NULL;
		cur_cla = start_cla;
		if(cur_cla == NULL)
			break;
		do {
			cleanup_sql_statement_helper(cur_cla->column_list, items_to_free, max_item_to_free);
			cleanup_sql_statement_helper(cur_cla->alias, items_to_free, max_item_to_free);
			cleanup_sql_statement_helper(cur_cla->keywords, items_to_free, max_item_to_free);
			t_cla = cur_cla->next;
			items_to_free[(*max_item_to_free)++] = cur_cla;
			cur_cla = t_cla;
		} while(cur_cla != start_cla);
		break;
	case column_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		items_to_free[(*max_item_to_free)++] = stmt;
		stmt->v.value = NULL;
		if(column_alias == NULL) {
			break;
		}
		// Do not cleanup columns from an alias statement, as they are
		//  just temporary pointers to columns from a long-living table
		//  structure
		//cleanup_sql_statement_helper(column_alias->column, items_to_free, max_item_to_free);
		cleanup_sql_statement_helper(column_alias->table_alias, items_to_free, max_item_to_free);
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		items_to_free[(*max_item_to_free)++] = stmt;
		if(table_alias == NULL)
			break;
		stmt->v.value = NULL;
		cleanup_sql_statement_helper(table_alias->table, items_to_free, max_item_to_free);
		cleanup_sql_statement_helper(table_alias->alias, items_to_free, max_item_to_free);
		cleanup_sql_statement_helper(table_alias->column_list, items_to_free, max_item_to_free);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		items_to_free[(*max_item_to_free)++] = stmt;
		if(function_call == NULL)
			break;
		stmt->v.value = NULL;
		cleanup_sql_statement_helper(function_call->function_name, items_to_free, max_item_to_free);
		cleanup_sql_statement_helper(function_call->parameters, items_to_free, max_item_to_free);
		break;
	case set_operation_STATEMENT:
		UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
		stmt->v.value = NULL;
		cleanup_sql_statement_helper(set_operation->operand[0], items_to_free, max_item_to_free);
		cleanup_sql_statement_helper(set_operation->operand[1], items_to_free, max_item_to_free);
		items_to_free[(*max_item_to_free)++] = stmt;
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
		break;
	}
}
