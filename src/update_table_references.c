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

int update_table_references_helper(SqlStatement *stmt, int old_unique_id, int new_unique_id);

int update_table_references(SqlStatement *stmt, int old_unique_id, int new_unique_id) {
	return update_table_references_helper(stmt, old_unique_id, new_unique_id);
}

int update_table_references_helper(SqlStatement *stmt, int old_unique_id, int new_unique_id) {
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
		if(table_alias->unique_id == old_unique_id) {
			table_alias->unique_id = new_unique_id;
		}
		status = update_table_references(column_alias->column, old_unique_id, new_unique_id);
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		status = update_table_references_helper(binary->operands[0], old_unique_id, new_unique_id);
		if (0 != status)
			break;
		status = update_table_references_helper(binary->operands[1], old_unique_id, new_unique_id);
		break;
	case set_operation_STATEMENT:
		UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
		status = update_table_references_helper(set_operation->operand[0], old_unique_id, new_unique_id);
		if (0 != status)
			break;
		status = update_table_references_helper(set_operation->operand[1], old_unique_id, new_unique_id);
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		status = update_table_references_helper(unary->operand, old_unique_id, new_unique_id);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(fc, stmt, function_call);
		UNPACK_SQL_STATEMENT(column_list, fc->parameters, column_list);
		status = update_table_references_helper(fc->function_name, old_unique_id, new_unique_id);
		if (0 != status)
			break;
		cur_cl = start_cl = column_list;
		do {
			status = update_table_references_helper(cur_cl->value, old_unique_id, new_unique_id);
			if (0 != status)
				break;
			cur_cl = cur_cl->next;
		} while(cur_cl != start_cl);
		break;
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		status = update_table_references_helper(cas->value, old_unique_id, new_unique_id);
		if (0 != status)
			break;
		status = update_table_references_helper(cas->branches, old_unique_id, new_unique_id);
		if (0 != status)
			break;
		status = update_table_references_helper(cas->optional_else, old_unique_id, new_unique_id);
		break;
	case cas_branch_STATEMENT:
		UNPACK_SQL_STATEMENT(cas_branch, stmt, cas_branch);
		cur_branch = cas_branch;
		do {
			status = update_table_references_helper(cur_branch->condition, old_unique_id, new_unique_id);
			if (0 != status)
				break;
			status = update_table_references_helper(cur_branch->value, old_unique_id, new_unique_id);
			if (0 != status)
				break;
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		break;
	case column_list_STATEMENT:
		UNPACK_SQL_STATEMENT(start_cl, stmt, column_list);
		cur_cl = start_cl;
		do {
			status = update_table_references_helper(cur_cl->value, old_unique_id, new_unique_id);
			if (0 != status)
				break;
			cur_cl = cur_cl->next;
		} while(cur_cl != start_cl);
		break;
	case column_list_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(column_list_alias, stmt, column_list_alias);
		cur_cla = start_cla = column_list_alias;
		do {
			status = update_table_references_helper(cur_cla->column_list, old_unique_id, new_unique_id);
			if (0 != status)
				break;
			cur_cla = cur_cla->next;
		} while(cur_cla != start_cla);
		break;
	case join_STATEMENT:
		UNPACK_SQL_STATEMENT(start_join, stmt, join);
		cur_join = start_join;
		do {
			status = update_table_references_helper(cur_join->value, old_unique_id, new_unique_id);
			if (0 != status)
				break;
			status = update_table_references_helper(cur_join->condition, old_unique_id, new_unique_id);
			if (0 != status)
				break;
			cur_join = cur_join->next;
		} while(cur_join != start_join);
		break;
	case select_STATEMENT:
		UNPACK_SQL_STATEMENT(select, stmt, select);
		status = update_table_references_helper(select->select_list, old_unique_id, new_unique_id);
		if (0 != status)
			break;
		status = update_table_references_helper(select->table_list, old_unique_id, new_unique_id);
		if (0 != status)
			break;
		status = update_table_references_helper(select->where_expression, old_unique_id, new_unique_id);
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
				status = update_table_references_helper(value->v.calculated, old_unique_id, new_unique_id);
				break;
			case COERCE_TYPE:
				status = update_table_references_helper(value->v.coerce_target, old_unique_id, new_unique_id);
				break;
			case UNKNOWN_SqlValueType:
			default:
				// assert(FALSE); (Uncomment once https://gitlab.com/YottaDB/DBMS/YDBOcto/issues/171 is fixed)
				break;
		}
		break;
	case column_STATEMENT:
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		if(table_alias->unique_id == old_unique_id) {
			table_alias->unique_id = new_unique_id;
		}
		status = update_table_references_helper(table_alias->table, old_unique_id, new_unique_id);
		break;
	case table_STATEMENT:
		// Nothing to do here, but we can get here by recursing a table_alias
		break;
	default:
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		status = 1;
		break;
	}
	return status;
}
