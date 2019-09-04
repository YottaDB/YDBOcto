/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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

int replace_table_references_helper(SqlStatement *stmt, SqlTableAlias *old_value, SqlStatement *new_value);

int replace_table_references(SqlStatement *stmt, SqlStatement *to_remove) {
	SqlStatement *new_value;
	SqlTableAlias *table_alias;
	SqlValue *value;

	SQL_STATEMENT(new_value, value_STATEMENT);
	MALLOC_STATEMENT(new_value, value, SqlValue);
	UNPACK_SQL_STATEMENT(value, new_value, value);
	value->type = NUL_VALUE;

	UNPACK_SQL_STATEMENT(table_alias, to_remove, table_alias);
	return replace_table_references_helper(stmt, table_alias, new_value);
}

int replace_table_references_helper(SqlStatement *stmt, SqlTableAlias *old_value, SqlStatement *new_value) {
	SqlUnaryOperation	*unary;
	SqlColumnAlias		*column_alias;
	SqlTableAlias		*table_alias;
	SqlBinaryOperation	*binary;
	SqlSetOperation		*set_operation;
	SqlFunctionCall		*fc;
	SqlCaseStatement	*cas;
	SqlCaseBranchStatement	*cas_branch, *cur_branch;
	SqlColumnList		*column_list;
	SqlColumnList		*cur_cl, *start_cl;
	SqlColumnListAlias	*cur_cla, *start_cla, *column_list_alias;
	SqlSelectStatement	*select;
	SqlValue		*value;
	SqlJoin			*cur_join, *start_join;
	int status = 0;

	if(stmt == NULL)
		return 0;

	switch(stmt->type) {
	case column_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias, table_alias);
		if(table_alias->unique_id == old_value->unique_id) {
			*stmt = *new_value;
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		status = replace_table_references_helper(binary->operands[0], old_value, new_value);
		if (0 != status)
			break;
		status = replace_table_references_helper(binary->operands[1], old_value, new_value);
		break;
	case set_operation_STATEMENT:
		UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
		status = replace_table_references_helper(set_operation->operand[0], old_value, new_value);
		if (0 != status)
			break;
		status = replace_table_references_helper(set_operation->operand[1], old_value, new_value);
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		status = replace_table_references_helper(unary->operand, old_value, new_value);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(fc, stmt, function_call);
		UNPACK_SQL_STATEMENT(column_list, fc->parameters, column_list);
		status = replace_table_references_helper(fc->function_name, old_value, new_value);
		if (0 != status)
			break;
		cur_cl = start_cl = column_list;
		do {
			status = replace_table_references_helper(cur_cl->value, old_value, new_value);
			cur_cl = cur_cl->next;
		} while(cur_cl != start_cl);
		break;
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		status = replace_table_references_helper(cas->value, old_value, new_value);
		if (0 != status)
			break;
		status = replace_table_references_helper(cas->branches, old_value, new_value);
		if (0 != status)
			break;
		status = replace_table_references_helper(cas->optional_else, old_value, new_value);
		break;
	case cas_branch_STATEMENT:
		UNPACK_SQL_STATEMENT(cas_branch, stmt, cas_branch);
		cur_branch = cas_branch;
		do {
			status = replace_table_references_helper(cur_branch->condition, old_value, new_value);
			if (0 != status)
				break;
			status = replace_table_references_helper(cur_branch->value, old_value, new_value);
			if (0 != status)
				break;
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		break;
	case column_list_STATEMENT:
		UNPACK_SQL_STATEMENT(column_list, stmt, column_list);
		status = replace_table_references_helper(column_list->value, old_value, new_value);
		break;
	case column_list_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(column_list_alias, stmt, column_list_alias);
		cur_cla = start_cla = column_list_alias;
		do {
			status = replace_table_references_helper(cur_cla->column_list, old_value, new_value);
			if (0 != status)
				break;
			cur_cla = cur_cla->next;
		} while(cur_cla != start_cla);
		break;
	case join_STATEMENT:
		UNPACK_SQL_STATEMENT(start_join, stmt, join);
		cur_join = start_join;
		do {
			status = replace_table_references_helper(cur_join->value, old_value, new_value);
			if (0 != status)
				break;
			status = replace_table_references_helper(cur_join->condition, old_value, new_value);
			if (0 != status)
				break;
			cur_join = cur_join->next;
		} while(cur_join != start_join);
		break;
	case select_STATEMENT:
		UNPACK_SQL_STATEMENT(select, stmt, select);
		status = replace_table_references_helper(select->select_list, old_value, new_value);
			if (0 != status)
				break;
		status = replace_table_references_helper(select->table_list, old_value, new_value);
			if (0 != status)
				break;
		status = replace_table_references_helper(select->where_expression, old_value, new_value);
		/* Do not replace select->order_expression and select->set_operation as they are currently
		 * left untouched by "copy_sql_statement.c".
		 */
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch(value->type) {
			case NUMBER_LITERAL:
			case STRING_LITERAL:
			case FUNCTION_NAME:
			case DATE_TIME:
			case BOOLEAN_VALUE:
			case COLUMN_REFERENCE:
			case NUL_VALUE:
				break;
			case CALCULATED_VALUE:
				status = replace_table_references_helper(value->v.calculated, old_value, new_value);
				break;
			case COERCE_TYPE:
				status = replace_table_references_helper(value->v.coerce_target, old_value, new_value);
				break;
			case UNKNOWN_SqlValueType:
			default:
				// assert(FALSE); (Uncomment once https://gitlab.com/YottaDB/DBMS/YDBOcto/issues/171 is fixed)
				break;
		}
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		status = replace_table_references_helper(table_alias->table, old_value, new_value);
		if (0 != status)
			break;
		status = replace_table_references_helper(table_alias->column_list, old_value, new_value);
		break;
	case table_STATEMENT:
		// Nothing to do, but we can get here through a table_alias statement
		break;
	default:
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		status = 1;
		break;
	}
	return status;
}
