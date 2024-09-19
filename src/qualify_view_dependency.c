/****************************************************************
 *								*
 * Copyright (c) 2022-2024 YottaDB LLC and/or its subsidiaries.	*
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

#define CLEANUP_AND_RETURN(OID_BUFFER)        \
	{                                     \
		YDB_FREE_BUFFER(&OID_BUFFER); \
		return 1;                     \
	}

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, OID_BUFFER) \
	{                                                    \
		YDB_ERROR_CHECK(STATUS);                     \
		if (YDB_OK != STATUS) {                      \
			CLEANUP_AND_RETURN(OID_BUFFER);      \
		}                                            \
	}

/* Store information in lvn that view definition depends on the function, table and view found. */
int qualify_view_dependency(SqlStatement *stmt, SqlView *view) {
	ydb_buffer_t subs[4];
	ydb_buffer_t ydboctoViewDependency;
	int	     status;

	if (NULL == stmt) {
		return 0;
	}
	int result = 0;
	switch (stmt->type) {
	case create_table_STATEMENT:;
		// Write dependency to lvn
		// %ydboctoViewDependency("V2","tables","names")=""
		SqlTable *table;
		UNPACK_SQL_STATEMENT(table, stmt, create_table);

		ydb_buffer_t table_name;
		YDB_STRING_TO_BUFFER(table->tableName->v.value->v.string_literal, &table_name);

		// Setup the lvn buffers
		YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOVIEWDEPENDENCY, &ydboctoViewDependency);

		// Setup subscripts
		YDB_STRING_TO_BUFFER(view->viewName->v.value->v.string_literal, &subs[0]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_TABLES, &subs[1]);
		subs[2] = table_name;

		// Set the lvn
		status = ydb_set_s(&ydboctoViewDependency, 3, &subs[0], NULL);
		assert(YDB_OK == status);
		if (YDB_OK != status) {
			result = 1;
		}
		break;
	case create_view_STATEMENT:;
		// Write dependency to lvn
		// %ydboctoViewDependency("v2","views","V1")=""
		SqlView *lcl_view;
		UNPACK_SQL_STATEMENT(lcl_view, stmt, create_view);

		ydb_buffer_t view_name;
		YDB_STRING_TO_BUFFER(lcl_view->viewName->v.value->v.string_literal, &view_name);

		// Setup the lvn buffers
		YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOVIEWDEPENDENCY, &ydboctoViewDependency);

		// Setup subscripts
		YDB_STRING_TO_BUFFER(view->viewName->v.value->v.string_literal, &subs[0]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_VIEWS, &subs[1]);
		subs[2] = view_name;

		// Set the lvn
		status = ydb_set_s(&ydboctoViewDependency, 3, &subs[0], NULL);
		assert(YDB_OK == status);
		if (YDB_OK != status) {
			result = 1;
		}
		break;
	case function_call_STATEMENT:;
		// Write dependency to lvn or gvn
		// %ydboctoViewDependency("V2","FUNCTION","ydboctoFN0uUSDY6E7G9VcjaOGNP9G")="SAMEVALUE"
		SqlFunctionCall *fc;
		UNPACK_SQL_STATEMENT(fc, stmt, function_call);

		char *function_name;
		function_name = fc->function_name->v.value->v.string_literal;
		// TOUPPER_STR(function_name);

		// Setup the lvn buffers
		YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOVIEWDEPENDENCY, &ydboctoViewDependency);

		// Setup the subscripts
		YDB_STRING_TO_BUFFER(view->viewName->v.value->v.string_literal, &subs[0]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS, &subs[1]);
		YDB_STRING_TO_BUFFER(fc->function_schema->v.create_function->function_hash->v.value->v.string_literal, &subs[2]);
		YDB_STRING_TO_BUFFER(function_name, &subs[3]);

		// Set the lvn
		status = ydb_set_s(&ydboctoViewDependency, 3, &subs[0], &subs[3]);
		assert(YDB_OK == status);
		if (YDB_OK != status) {
			result = 1;
			break;
		}
		result = qualify_view_dependency(fc->parameters, view);
		break;
	case set_operation_STATEMENT:;
		SqlSetOperation *set_oper;
		UNPACK_SQL_STATEMENT(set_oper, stmt, set_operation);
		for (int i = 0; i < 2; i++) {
			result |= qualify_view_dependency(set_oper->operand[i], view);
		}
		break;
	case table_alias_STATEMENT:;
		SqlTableAlias *table_alias;
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		result |= qualify_view_dependency(table_alias->table, view);
		break;
	case table_value_STATEMENT:;
		SqlTableValue *table_value;
		UNPACK_SQL_STATEMENT(table_value, stmt, table_value);
		result |= qualify_view_dependency(table_value->row_value_stmt, view);
		break;
	case row_value_STATEMENT:;
		SqlRowValue *start_row_value, *cur_row_value;
		UNPACK_SQL_STATEMENT(start_row_value, stmt, row_value);
		cur_row_value = start_row_value;
		do {
			result |= qualify_view_dependency(cur_row_value->value_list, view);
			cur_row_value = cur_row_value->next;
		} while (cur_row_value != start_row_value);
		break;
	case value_STATEMENT:;
		SqlValue *value;
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch (value->type) {
		case CALCULATED_VALUE:
			result |= qualify_view_dependency(value->v.calculated, view);
			break;
		case COERCE_TYPE:
			result |= qualify_view_dependency(value->v.coerce_target, view);
			break;
		case BOOLEAN_VALUE:
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case STRING_LITERAL:
		case NUL_VALUE:
		case DATE_LITERAL:
		case TIME_LITERAL:
		case TIME_WITH_TIME_ZONE_LITERAL:
		case TIMESTAMP_LITERAL:
		case TIMESTAMP_WITH_TIME_ZONE_LITERAL:
		case BOOLEAN_OR_STRING_LITERAL:
			/* It is possible for BOOLEAN_OR_STRING_LITERAL to be unresolved at this point.
			 * An example query which might lead us to this case is `create view view_name as `select 't';`.
			 * This type of value will be a literal no need to do anything here.
			 */
			break;
		case SELECT_ASTERISK:
		case TABLE_ASTERISK:
		case COLUMN_REFERENCE:
		case FUNCTION_NAME:
		case PARAMETER_VALUE:
		case FUNCTION_HASH:
		case DELIM_VALUE:
		case IS_NULL_LITERAL:
		case INVALID_SqlValueType:
		case UNKNOWN_SqlValueType:
			assert(FALSE);
			break;
		}
		break;
	case binary_STATEMENT:;
		SqlBinaryOperation *binary;
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		for (int i = 0; i < 2; i++) {
			result |= qualify_view_dependency(binary->operands[i], view);
		}
		break;
	case unary_STATEMENT:;
		SqlUnaryOperation *unary;
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		result |= qualify_view_dependency(unary->operand, view);
		break;
	case array_STATEMENT:;
		SqlArray *array;
		UNPACK_SQL_STATEMENT(array, stmt, array);
		result |= qualify_view_dependency(array->argument, view);
		break;
	case coalesce_STATEMENT:;
		SqlCoalesceCall *coalesce;
		UNPACK_SQL_STATEMENT(coalesce, stmt, coalesce);
		result |= qualify_view_dependency(coalesce->arguments, view);
		break;
	case greatest_STATEMENT:;
		SqlGreatest *greatest;
		UNPACK_SQL_STATEMENT(greatest, stmt, greatest);
		result |= qualify_view_dependency(greatest->arguments, view);
		break;
	case least_STATEMENT:;
		SqlLeast *least;
		UNPACK_SQL_STATEMENT(least, stmt, least);
		result |= qualify_view_dependency(least->arguments, view);
		break;
	case null_if_STATEMENT:;
		SqlNullIf *null_if;
		UNPACK_SQL_STATEMENT(null_if, stmt, null_if);
		result |= qualify_view_dependency(null_if->left, view);
		result |= qualify_view_dependency(null_if->right, view);
		break;
	case aggregate_function_STATEMENT:;
		SqlAggregateFunction *agg;
		UNPACK_SQL_STATEMENT(agg, stmt, aggregate_function);
		result |= qualify_view_dependency(agg->parameter, view);
		break;
	case cas_STATEMENT:;
		SqlCaseStatement *cas;
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		result |= qualify_view_dependency(cas->value, view);
		result |= qualify_view_dependency(cas->branches, view);
		result |= qualify_view_dependency(cas->optional_else, view);
		break;
	case cas_branch_STATEMENT:;
		SqlCaseBranchStatement *cur_cas_branch, *start_cas_branch;
		UNPACK_SQL_STATEMENT(start_cas_branch, stmt, cas_branch);
		cur_cas_branch = start_cas_branch;
		do {
			result |= qualify_view_dependency(cur_cas_branch->condition, view);
			result |= qualify_view_dependency(cur_cas_branch->value, view);
			cur_cas_branch = cur_cas_branch->next;
		} while (cur_cas_branch != start_cas_branch);
		break;
	case column_list_STATEMENT:;
		SqlColumnList *cur_cl, *start_cl;
		UNPACK_SQL_STATEMENT(start_cl, stmt, column_list);
		cur_cl = start_cl;
		do {
			result |= qualify_view_dependency(cur_cl->value, view);
			cur_cl = cur_cl->next;
		} while (cur_cl != start_cl);
		break;
	case column_list_alias_STATEMENT:;
		SqlColumnListAlias *cur_cla, *start_cla;
		UNPACK_SQL_STATEMENT(start_cla, stmt, column_list_alias);
		cur_cla = start_cla;
		do {
			result |= qualify_view_dependency(cur_cla->column_list, view);
			cur_cla = cur_cla->next;
		} while (cur_cla != start_cla);
		break;
	case select_STATEMENT:;
		SqlSelectStatement *select;
		UNPACK_SQL_STATEMENT(select, stmt, select);
		result |= qualify_view_dependency(select->select_list, view);
		result |= qualify_view_dependency(select->table_list, view);
		result |= qualify_view_dependency(select->where_expression, view);
		result |= qualify_view_dependency(select->group_by_expression, view);
		result |= qualify_view_dependency(select->having_expression, view);
		result |= qualify_view_dependency(select->order_by_expression, view);
		break;
	case join_STATEMENT:;
		SqlJoin *cur_join, *start_join;
		UNPACK_SQL_STATEMENT(start_join, stmt, join);
		cur_join = start_join;
		do {
			result |= qualify_view_dependency(cur_join->value, view);
			cur_join = cur_join->next;
		} while (cur_join != start_join);
		break;
	case column_alias_STATEMENT:
		break;
	case column_STATEMENT:
	case create_function_STATEMENT:
	case drop_table_STATEMENT:
	case drop_view_STATEMENT:
	case drop_function_STATEMENT:
	case truncate_table_STATEMENT:
	case parameter_type_list_STATEMENT:
	case constraint_STATEMENT:
	case keyword_STATEMENT:
	case begin_STATEMENT:
	case commit_STATEMENT:
	case rollback_STATEMENT:
	case dynamic_sql_STATEMENT:
	case set_STATEMENT:
	case show_STATEMENT:
	case no_data_STATEMENT:
	case delim_char_list_STATEMENT:
	case index_STATEMENT:
	case data_type_struct_STATEMENT:
	case join_type_STATEMENT:
	case discard_all_STATEMENT:
	case discard_xrefs_STATEMENT:
	case history_STATEMENT:
	case display_relation_STATEMENT:
	case invalid_STATEMENT:
	case insert_STATEMENT:
	case delete_from_STATEMENT:
	case update_STATEMENT:
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		assert(FALSE);
		result = 1;
		break;
	}
	return result;
}
