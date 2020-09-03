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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

/* Note: This function is very similar to "match_sql_statement.c". Any changes there might need to be reflected here too. */
SqlStatement *copy_sql_statement(SqlStatement *stmt) {
	SqlTableAlias *		table_alias, *new_table_alias;
	SqlColumnList *		cur_column_list, *start_column_list, *new_column_list;
	SqlJoin *		cur_join, *start_join, *new_join;
	SqlInsertStatement *	insert;
	SqlStatement *		ret;
	SqlSelectStatement *	select;
	SqlDropTableStatement * drop_table;
	SqlValue *		value;
	SqlBinaryOperation *	binary;
	SqlUnaryOperation *	unary;
	SqlOptionalKeyword *	cur_keyword, *start_keyword, *new_keyword;
	SqlColumnListAlias *	new_cl_alias, *cur_cl_alias, *start_cl_alias;
	SqlColumnAlias *	column_alias;
	SqlCaseStatement *	cas;
	SqlCaseBranchStatement *cur_cas_branch, *start_cas_branch, *new_cas_branch;
	SqlFunctionCall *	function_call;
	SqlAggregateFunction *	aggregate_function;
	int			len;

	if (NULL == stmt)
		return NULL;
	// We don't need copy these things because they never get changed
	if ((create_table_STATEMENT == stmt->type) || (column_STATEMENT == stmt->type))
		return stmt;
	SQL_STATEMENT(ret, stmt->type);
	switch (stmt->type) {
	case create_table_STATEMENT:
		assert(FALSE);
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		MALLOC_STATEMENT(ret, table_alias, SqlTableAlias);
		new_table_alias = ret->v.table_alias;
		new_table_alias->table = copy_sql_statement(table_alias->table);
		new_table_alias->alias = copy_sql_statement(table_alias->alias);
		new_table_alias->unique_id = table_alias->unique_id;
		// the column list has a reference to this table, so don't copy it
		// new_table_alias->column_list = copy_sql_statement(table_alias->column_list);
		new_table_alias->column_list = table_alias->column_list;
		break;
	case select_STATEMENT:
		UNPACK_SQL_STATEMENT(select, stmt, select);
		MALLOC_STATEMENT(ret, select, SqlSelectStatement);
		*ret->v.select = *select;
		ret->v.select->select_list = copy_sql_statement(select->select_list);
		ret->v.select->table_list = copy_sql_statement(select->table_list);
		ret->v.select->where_expression = copy_sql_statement(select->where_expression);
		ret->v.select->group_by_expression = copy_sql_statement(select->group_by_expression);
		ret->v.select->having_expression = copy_sql_statement(select->having_expression);
		ret->v.select->optional_words = copy_sql_statement(select->optional_words);
		// Don't copy the order by, which has a pointer to an element in the select list which has
		// a pointer to this statement, which results in a loop that causes a stack overflow
		/// TODO: update the pointers to match the new items in the select_list which has already
		// been copied by looking at the aliases for each column below
		// ret->v.select->order_by_expression = copy_sql_statement(select->order_by_expression);
		break;
	case discard_all_STATEMENT:
		break;
	case drop_table_STATEMENT:
		drop_table = stmt->v.drop_table;
		MALLOC_STATEMENT(ret, drop_table, SqlDropTableStatement);
		ret->v.drop_table->table_name = copy_sql_statement(drop_table->table_name);
		ret->v.drop_table->optional_keyword = copy_sql_statement(drop_table->optional_keyword);
		break;
	case value_STATEMENT:
		value = stmt->v.value;
		MALLOC_STATEMENT(ret, value, SqlValue);
		ret->v.value->type = value->type;
		if (COERCE_TYPE == value->type) {
			ret->v.value->pre_coerced_type = value->pre_coerced_type;
			ret->v.value->coerced_type = value->coerced_type;
		}
		ret->v.value->parameter_index = value->parameter_index;
		if (CALCULATED_VALUE == value->type) {
			ret->v.value->v.calculated = copy_sql_statement(value->v.calculated);
		} else if (NUL_VALUE == value->type) {
			// Don't copy a null value
		} else {
			len = strlen(value->v.reference) + 1;
			ret->v.value->v.reference = octo_cmalloc(memory_chunks, len);
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
		// MALLOC_STATEMENT(ret, column_list, SqlColumnList);
		if (start_column_list) {
			cur_column_list = start_column_list;
			do {
				OCTO_CMALLOC_STRUCT(new_column_list, SqlColumnList);
				dqinit(new_column_list);
				new_column_list->value = copy_sql_statement(cur_column_list->value);
				if (NULL == ret->v.column_list) {
					ret->v.column_list = new_column_list;
				} else {
					dqappend(new_column_list, ret->v.column_list);
				}
				cur_column_list = cur_column_list->next;
			} while (cur_column_list != start_column_list);
		}
		break;
	case column_list_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(start_cl_alias, stmt, column_list_alias);
		if (start_cl_alias) {
			cur_cl_alias = start_cl_alias;
			do {
				OCTO_CMALLOC_STRUCT(new_cl_alias, SqlColumnListAlias);
				dqinit(new_cl_alias);
				new_cl_alias->column_list = copy_sql_statement(cur_cl_alias->column_list);
				new_cl_alias->alias = copy_sql_statement(cur_cl_alias->alias);
				new_cl_alias->keywords = copy_sql_statement(new_cl_alias->keywords);
				new_cl_alias->type = cur_cl_alias->type;
				new_cl_alias->user_specified_alias = cur_cl_alias->user_specified_alias;
				if (NULL == ret->v.column_list_alias) {
					ret->v.column_list_alias = new_cl_alias;
				} else {
					dqappend(new_cl_alias, ret->v.column_list_alias);
				}
				cur_cl_alias = cur_cl_alias->next;
			} while (cur_cl_alias != start_cl_alias);
		}
		break;
	case join_STATEMENT:
		UNPACK_SQL_STATEMENT(start_join, stmt, join);
		// MALLOC_STATEMENT(ret, join, SqlJoin);
		cur_join = start_join;
		do {
			OCTO_CMALLOC_STRUCT(new_join, SqlJoin);
			dqinit(new_join);
			new_join->type = cur_join->type;
			new_join->value = copy_sql_statement(cur_join->value);
			new_join->condition = copy_sql_statement(cur_join->condition);
			if (NULL == ret->v.join) {
				ret->v.join = new_join;
			} else {
				dqappend(new_join, ret->v.join);
			}
			cur_join = cur_join->next;
		} while (cur_join != start_join);
		break;
	case column_alias_STATEMENT:
		MALLOC_STATEMENT(ret, column_alias, SqlColumnAlias);
		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		ret->v.column_alias->column = copy_sql_statement(column_alias->column);
		ret->v.column_alias->table_alias_stmt = copy_sql_statement(column_alias->table_alias_stmt);
		break;
	case data_type_struct_STATEMENT:
		*ret = *stmt;
		break;
	case keyword_STATEMENT:
		UNPACK_SQL_STATEMENT(start_keyword, stmt, keyword);
		cur_keyword = start_keyword;
		do {
			OCTO_CMALLOC_STRUCT(new_keyword, SqlOptionalKeyword);
			*new_keyword = *cur_keyword;
			dqinit(new_keyword);
			new_keyword->v = copy_sql_statement(cur_keyword->v);
			if (ret->v.keyword) {
				dqappend(ret->v.keyword, new_keyword);
			} else {
				ret->v.keyword = new_keyword;
			}
			cur_keyword = cur_keyword->next;
		} while (cur_keyword != start_keyword);
		break;
	case insert_STATEMENT:
		UNPACK_SQL_STATEMENT(insert, stmt, insert);
		MALLOC_STATEMENT(ret, insert, SqlInsertStatement);
		ret->v.insert->source = copy_sql_statement(insert->source);
		ret->v.insert->columns = copy_sql_statement(insert->columns);
		break;
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		MALLOC_STATEMENT(ret, cas, SqlCaseStatement);
		// SqlValue
		ret->v.cas->value = copy_sql_statement(cas->value);
		// SqlCaseBranchStatement
		ret->v.cas->branches = copy_sql_statement(cas->branches);
		// SqlValue
		ret->v.cas->optional_else = copy_sql_statement(cas->optional_else);
		break;
	case cas_branch_STATEMENT:
		UNPACK_SQL_STATEMENT(start_cas_branch, stmt, cas_branch);
		cur_cas_branch = start_cas_branch;
		do {
			OCTO_CMALLOC_STRUCT(new_cas_branch, SqlCaseBranchStatement);
			*new_cas_branch = *cur_cas_branch;
			// SqlValue
			new_cas_branch->condition = copy_sql_statement(cur_cas_branch->condition);
			// SqlValue
			new_cas_branch->value = copy_sql_statement(cur_cas_branch->value);
			dqinit(new_cas_branch);
			if (ret->v.cas_branch) {
				dqappend(ret->v.cas_branch, new_cas_branch);
			} else {
				ret->v.cas_branch = new_cas_branch;
			}
			cur_cas_branch = cur_cas_branch->next;
		} while (cur_cas_branch != start_cas_branch);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		MALLOC_STATEMENT(ret, function_call, SqlFunctionCall);
		ret->v.function_call->function_name = copy_sql_statement(function_call->function_name);
		ret->v.function_call->parameters = copy_sql_statement(function_call->parameters);
		break;
	case aggregate_function_STATEMENT:
		UNPACK_SQL_STATEMENT(aggregate_function, stmt, aggregate_function);
		MALLOC_STATEMENT(ret, aggregate_function, SqlAggregateFunction);
		ret->v.aggregate_function->type = aggregate_function->type;
		ret->v.aggregate_function->parameter = copy_sql_statement(aggregate_function->parameter);
		break;
	default:
		assert(FALSE);
		FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
		break;
	}
	return ret;
}
