/****************************************************************
 *								*
 * Copyright (c) 2020-2022 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

/* This function checks if `stmt` and `match_stmt` both point to the same SQL query. If so it returns TRUE else it returns FALSE.
 * Note: This function is very similar to "copy_sql_statement.c". Any changes there might need to be reflected here too.
 */
boolean_t match_sql_statement(SqlStatement *stmt, SqlStatement *match_stmt) {
	SqlTableAlias *		table_alias, *match_table_alias;
	SqlColumnList *		cur_column_list, *start_column_list;
	SqlColumnList *		match_cur_column_list, *match_start_column_list;
	SqlJoin *		cur_join, *start_join;
	SqlJoin *		match_cur_join, *match_start_join;
	SqlSelectStatement *	select, *match_select;
	SqlDropTableStatement * drop_table, *match_drop_table;
	SqlValue *		value, *match_value;
	SqlBinaryOperation *	binary, *match_binary;
	SqlUnaryOperation *	unary, *match_unary;
	SqlOptionalKeyword *	cur_keyword, *start_keyword;
	SqlOptionalKeyword *	match_cur_keyword, *match_start_keyword;
	SqlColumnListAlias *	cur_cl_alias, *start_cl_alias;
	SqlColumnListAlias *	match_cur_cl_alias, *match_start_cl_alias;
	SqlColumnAlias *	column_alias, *match_column_alias;
	SqlCaseStatement *	cas, *match_cas;
	SqlCaseBranchStatement *cur_cas_branch, *start_cas_branch;
	SqlCaseBranchStatement *match_cur_cas_branch, *match_start_cas_branch;
	SqlFunctionCall *	function_call, *match_function_call;
	SqlGreatest *		greatest_call, *match_greatest_call;
	SqlLeast *		least_call, *match_least_call;
	SqlCoalesceCall *	coalesce_call, *match_coalesce_call;
	SqlNullIf *		null_if, *match_null_if;
	SqlAggregateFunction *	aggregate_function, *match_aggregate_function;
	boolean_t		ret;

	if ((NULL == stmt) != (NULL == match_stmt)) {
		return FALSE;
	}
	if (NULL == stmt) {
		return TRUE;
	}
	if (stmt->type != match_stmt->type) {
		return FALSE;
	}
	switch (stmt->type) {
	case create_table_STATEMENT:
	case table_value_STATEMENT:
		/* This structure never gets copied in "copy_sql_statement.c" so just check if the pointers are identical */
		ret = (stmt == match_stmt);
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		UNPACK_SQL_STATEMENT(match_table_alias, match_stmt, table_alias);
		ret = match_sql_statement(table_alias->table, match_table_alias->table);
		if (!ret)
			break;
		ret = match_sql_statement(table_alias->alias, match_table_alias->alias);
		/* No need to consider "table_alias->column_list" for match check (see hash_canonical_query.c under
		 * "case table_alias_STATEMENT:" for reason).
		 */
		break;
	case select_STATEMENT:
		UNPACK_SQL_STATEMENT(select, stmt, select);
		UNPACK_SQL_STATEMENT(match_select, match_stmt, select);
		ret = match_sql_statement(select->select_list, match_select->select_list);
		if (!ret)
			break;
		ret = match_sql_statement(select->table_list, match_select->table_list);
		if (!ret)
			break;
		ret = match_sql_statement(select->where_expression, match_select->where_expression);
		if (!ret)
			break;
		ret = match_sql_statement(select->group_by_expression, match_select->group_by_expression);
		if (!ret)
			break;
		ret = match_sql_statement(select->having_expression, match_select->having_expression);
		if (!ret)
			break;
		ret = match_sql_statement(select->optional_words, match_select->optional_words);
		if (!ret)
			break;
		/* Note: "copy_sql_statement.c" does not copy the below field (not yet sure why) but the match function
		 * should surely go through this field too to confirm it is identical.
		 */
		ret = match_sql_statement(select->order_by_expression, match_select->order_by_expression);
		if (!ret)
			break;
		break;
	case discard_all_STATEMENT:
		ret = TRUE;
		break;
	case drop_table_STATEMENT:
		UNPACK_SQL_STATEMENT(drop_table, stmt, drop_table);
		UNPACK_SQL_STATEMENT(match_drop_table, match_stmt, drop_table);
		ret = match_sql_statement(drop_table->table_name, match_drop_table->table_name);
		if (!ret)
			break;
		ret = match_sql_statement(drop_table->optional_keyword, match_drop_table->optional_keyword);
		if (!ret)
			break;
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		UNPACK_SQL_STATEMENT(match_value, match_stmt, value);
		ret = (value->type == match_value->type);
		if (!ret)
			break;
		/* Same literals could have different parameter index so do not compare the parameter_index */
		/* ret = (value->parameter_index == match_value->parameter_index); if (!ret) break; */
		if (COERCE_TYPE == value->type) {
			ret = (value->pre_coerced_type == match_value->pre_coerced_type);
			if (!ret)
				break;
			ret = !memcmp(&value->coerced_type, &match_value->coerced_type, sizeof(match_value->coerced_type));
			if (!ret)
				break;
			ret = match_sql_statement(value->v.coerce_target, match_value->v.coerce_target);
			if (!ret)
				break;
		} else if (CALCULATED_VALUE == value->type) {
			ret = match_sql_statement(value->v.calculated, match_value->v.calculated);
			if (!ret)
				break;
		} else if (NUL_VALUE == value->type) {
			// No need to match any more for NULL value
		} else {
			ret = !strcmp(value->v.reference, match_value->v.reference);
			if (!ret)
				break;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		UNPACK_SQL_STATEMENT(match_binary, match_stmt, binary);
		ret = (binary->operation == match_binary->operation);
		if (!ret)
			break;
		ret = match_sql_statement(binary->operands[0], match_binary->operands[0]);
		if (!ret)
			break;
		ret = match_sql_statement(binary->operands[1], match_binary->operands[1]);
		if (!ret)
			break;
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		UNPACK_SQL_STATEMENT(match_unary, match_stmt, unary);
		ret = (unary->operation == match_unary->operation);
		if (!ret)
			break;
		ret = match_sql_statement(unary->operand, match_unary->operand);
		if (!ret)
			break;
		break;
	case column_list_STATEMENT:
		UNPACK_SQL_STATEMENT(start_column_list, stmt, column_list);
		UNPACK_SQL_STATEMENT(match_start_column_list, match_stmt, column_list);
		cur_column_list = start_column_list;
		match_cur_column_list = match_start_column_list;
		ret = ((NULL == cur_column_list) == (NULL == match_cur_column_list));
		if (!ret)
			break;
		ret = (NULL == cur_column_list);
		if (ret)
			break; /* Note: "if (ret)" used here instead of
				* the usual "if (!ret)".
				*/
		do {
			ret = match_sql_statement(cur_column_list->value, match_cur_column_list->value);
			if (!ret)
				break;
			cur_column_list = cur_column_list->next;
			match_cur_column_list = match_cur_column_list->next;
			ret = ((start_column_list == cur_column_list) == (match_start_column_list == match_cur_column_list));
			if (!ret)
				break;
		} while (cur_column_list != start_column_list);
		break;
	case column_list_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(start_cl_alias, stmt, column_list_alias);
		UNPACK_SQL_STATEMENT(match_start_cl_alias, match_stmt, column_list_alias);
		cur_cl_alias = start_cl_alias;
		match_cur_cl_alias = match_start_cl_alias;
		ret = ((NULL == cur_cl_alias) == (NULL == match_cur_cl_alias));
		if (!ret)
			break;
		ret = (NULL == cur_cl_alias);
		if (ret)
			break; /* Note: "if (ret)" used here instead of
				* the usual "if (!ret)".
				*/
		do {
			ret = (cur_cl_alias->type == match_cur_cl_alias->type);
			if (!ret)
				break;
			ret = match_sql_statement(cur_cl_alias->column_list, match_cur_cl_alias->column_list);
			if (!ret)
				break;
			ret = match_sql_statement(cur_cl_alias->alias, match_cur_cl_alias->alias);
			if (!ret)
				break;
			ret = match_sql_statement(cur_cl_alias->keywords, match_cur_cl_alias->keywords);
			if (!ret)
				break;
			cur_cl_alias = cur_cl_alias->next;
			match_cur_cl_alias = match_cur_cl_alias->next;
			ret = ((start_cl_alias == cur_cl_alias) == (match_start_cl_alias == match_cur_cl_alias));
			if (!ret)
				break;
		} while (start_cl_alias != cur_cl_alias);
		break;
	case column_STATEMENT:
		/* This structure never gets copied in "copy_sql_statement.c" so just check if the pointers are identical */
		ret = (stmt->v.column == match_stmt->v.column);
		break;
	case join_STATEMENT:
		UNPACK_SQL_STATEMENT(start_join, stmt, join);
		UNPACK_SQL_STATEMENT(match_start_join, match_stmt, join);
		cur_join = start_join;
		match_cur_join = match_start_join;
		do {
			ret = (cur_join->type == match_cur_join->type);
			if (!ret)
				break;
			ret = match_sql_statement(cur_join->value, match_cur_join->value);
			if (!ret)
				break;
			ret = match_sql_statement(cur_join->condition, match_cur_join->condition);
			if (!ret)
				break;
			cur_join = cur_join->next;
			match_cur_join = match_cur_join->next;
			ret = ((start_join == cur_join) == (match_start_join == match_cur_join));
			if (!ret)
				break;
		} while (cur_join != start_join);
		break;
	case column_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		UNPACK_SQL_STATEMENT(match_column_alias, match_stmt, column_alias);
		ret = match_sql_statement(column_alias->column, match_column_alias->column);
		if (!ret)
			break;
		ret = match_sql_statement(column_alias->table_alias_stmt, match_column_alias->table_alias_stmt);
		if (!ret)
			break;
		break;
	case data_type_struct_STATEMENT:
		ret = !memcmp(&stmt->v.data_type_struct, &match_stmt->v.data_type_struct, sizeof(stmt->v.data_type_struct));
		if (!ret)
			break;
		break;
	case keyword_STATEMENT:
		UNPACK_SQL_STATEMENT(start_keyword, stmt, keyword);
		UNPACK_SQL_STATEMENT(match_start_keyword, match_stmt, keyword);
		cur_keyword = start_keyword;
		match_cur_keyword = match_start_keyword;
		do {
			ret = (cur_keyword->keyword == match_cur_keyword->keyword);
			if (!ret)
				break;
			ret = match_sql_statement(cur_keyword->v, match_cur_keyword->v);
			if (!ret)
				break;
			cur_keyword = cur_keyword->next;
			match_cur_keyword = match_cur_keyword->next;
			ret = ((start_keyword == cur_keyword) == (match_start_keyword == match_cur_keyword));
			if (!ret)
				break;
		} while (cur_keyword != start_keyword);
		break;
	case insert_STATEMENT:
		assert(FALSE); /* Deal with this code when we support INSERT commands */
		ret = FALSE;
		break;
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		UNPACK_SQL_STATEMENT(match_cas, match_stmt, cas);
		ret = match_sql_statement(cas->value, match_cas->value);
		if (!ret)
			break;
		ret = match_sql_statement(cas->branches, match_cas->branches);
		if (!ret)
			break;
		ret = match_sql_statement(cas->optional_else, match_cas->optional_else);
		if (!ret)
			break;
		break;
	case cas_branch_STATEMENT:
		UNPACK_SQL_STATEMENT(start_cas_branch, stmt, cas_branch);
		UNPACK_SQL_STATEMENT(match_start_cas_branch, match_stmt, cas_branch);
		cur_cas_branch = start_cas_branch;
		match_cur_cas_branch = match_start_cas_branch;
		do {
			ret = match_sql_statement(cur_cas_branch->condition, match_cur_cas_branch->condition);
			if (!ret)
				break;
			ret = match_sql_statement(cur_cas_branch->value, match_cur_cas_branch->value);
			if (!ret)
				break;
			cur_cas_branch = cur_cas_branch->next;
			match_cur_cas_branch = match_cur_cas_branch->next;
			ret = ((start_cas_branch == cur_cas_branch) == (match_start_cas_branch == match_cur_cas_branch));
			if (!ret)
				break;
		} while (cur_cas_branch != start_cas_branch);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		UNPACK_SQL_STATEMENT(match_function_call, match_stmt, function_call);
		ret = match_sql_statement(function_call->function_name, match_function_call->function_name);
		if (!ret)
			break;
		ret = match_sql_statement(function_call->parameters, match_function_call->parameters);
		if (!ret)
			break;
		break;
	case greatest_STATEMENT:
		UNPACK_SQL_STATEMENT(greatest_call, stmt, greatest);
		UNPACK_SQL_STATEMENT(match_greatest_call, match_stmt, greatest);
		ret = match_sql_statement(greatest_call->arguments, match_greatest_call->arguments);
		if (!ret)
			break;
		break;
	case coalesce_STATEMENT:
		UNPACK_SQL_STATEMENT(coalesce_call, stmt, coalesce);
		UNPACK_SQL_STATEMENT(match_coalesce_call, match_stmt, coalesce);
		ret = match_sql_statement(coalesce_call->arguments, match_coalesce_call->arguments);
		if (!ret)
			break;
		break;
	case least_STATEMENT:
		UNPACK_SQL_STATEMENT(least_call, stmt, least);
		UNPACK_SQL_STATEMENT(match_least_call, match_stmt, least);
		ret = match_sql_statement(least_call->arguments, match_least_call->arguments);
		if (!ret)
			break;
		break;
	case null_if_STATEMENT:
		UNPACK_SQL_STATEMENT(null_if, stmt, null_if);
		UNPACK_SQL_STATEMENT(match_null_if, match_stmt, null_if);
		ret = match_sql_statement(null_if->left, match_null_if->left);
		if (!ret)
			break;
		ret = match_sql_statement(null_if->right, match_null_if->right);
		if (!ret)
			break;
		break;
	case aggregate_function_STATEMENT:
		UNPACK_SQL_STATEMENT(aggregate_function, stmt, aggregate_function);
		UNPACK_SQL_STATEMENT(match_aggregate_function, match_stmt, aggregate_function);
		ret = (aggregate_function->type == match_aggregate_function->type);
		if (!ret)
			break;
		ret = match_sql_statement(aggregate_function->parameter, match_aggregate_function->parameter);
		if (!ret)
			break;
		break;
	default:
		assert(FALSE);
		FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
		ret = FALSE;
		break;
	}
	return ret;
}
