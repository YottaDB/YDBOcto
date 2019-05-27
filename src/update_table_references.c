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
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

void update_table_references_helper(SqlStatement *stmt, int old_unique_id, int new_unique_id);

SqlStatement *update_table_references(SqlStatement *stmt, int old_unique_id, int new_unique_id) {
	update_table_references_helper(stmt, old_unique_id, new_unique_id);
	return stmt;
}

void update_table_references_helper(SqlStatement *stmt, int old_unique_id, int new_unique_id) {
	SqlUnaryOperation *unary;
	SqlColumnAlias *column_alias;
	SqlTableAlias *table_alias;
	SqlBinaryOperation *binary;
	SqlFunctionCall *fc;
	SqlCaseStatement *cas;
	SqlCaseBranchStatement *cas_branch, *cur_branch;
	SqlColumnList *column_list;
	SqlColumnList *cur_cl, *start_cl;
	SqlColumnListAlias *cur_cla, *start_cla, *column_list_alias;
	SqlSelectStatement *select;
	SqlJoin *cur_join, *start_join;

	if(stmt == NULL)
		return;

	switch(stmt->type) {
	case column_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		UNPACK_SQL_STATEMENT(table_alias, column_alias->table_alias, table_alias);
		if(table_alias->unique_id == old_unique_id) {
			table_alias->unique_id = new_unique_id;
		}
		update_table_references(column_alias->column, old_unique_id, new_unique_id);
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		update_table_references_helper(binary->operands[0], old_unique_id, new_unique_id);
		update_table_references_helper(binary->operands[1], old_unique_id, new_unique_id);
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		update_table_references_helper(unary->operand, old_unique_id, new_unique_id);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(fc, stmt, function_call);
		UNPACK_SQL_STATEMENT(column_list, fc->parameters, column_list);
		update_table_references_helper(fc->function_name, old_unique_id, new_unique_id);
		cur_cl = start_cl = column_list;
		do {
			update_table_references_helper(cur_cl->value, old_unique_id, new_unique_id);
			cur_cl = cur_cl->next;
		} while(cur_cl != start_cl);
		break;
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		update_table_references_helper(cas->value, old_unique_id, new_unique_id);
		update_table_references_helper(cas->branches, old_unique_id, new_unique_id);
		break;
	case cas_branch_STATEMENT:
		UNPACK_SQL_STATEMENT(cas_branch, stmt, cas_branch);
		cur_branch = cas_branch;
		do {
			update_table_references_helper(cur_branch->condition, old_unique_id, new_unique_id);
			update_table_references_helper(cur_branch->value, old_unique_id, new_unique_id);
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		break;
	case column_list_STATEMENT:
		UNPACK_SQL_STATEMENT(start_cl, stmt, column_list);
		cur_cl = start_cl;
		do {
			update_table_references_helper(cur_cl->value, old_unique_id, new_unique_id);
			cur_cl = cur_cl->next;
		} while(cur_cl != start_cl);
		break;
	case column_list_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(column_list_alias, stmt, column_list_alias);
		cur_cla = start_cla = column_list_alias;
		do {
			update_table_references_helper(cur_cla->column_list, old_unique_id, new_unique_id);
			cur_cla = cur_cla->next;
		} while(cur_cla != start_cla);
		break;
	case join_STATEMENT:
		UNPACK_SQL_STATEMENT(start_join, stmt, join);
		cur_join = start_join;
		do {
			update_table_references_helper(cur_join->condition, old_unique_id, new_unique_id);
			update_table_references_helper(cur_join->value, old_unique_id, new_unique_id);
			cur_join = cur_join->next;
		} while(cur_join != start_join);
		break;
	case select_STATEMENT:
		UNPACK_SQL_STATEMENT(select, stmt, select);
		update_table_references_helper(select->select_list, old_unique_id, new_unique_id);
		update_table_references_helper(select->where_expression, old_unique_id, new_unique_id);
		update_table_references_helper(select->order_expression, old_unique_id, new_unique_id);
		update_table_references_helper(select->table_list, old_unique_id, new_unique_id);
		break;
	case value_STATEMENT:
	case column_STATEMENT:
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		if(table_alias->unique_id == old_unique_id) {
			table_alias->unique_id = new_unique_id;
		}
		update_table_references_helper(table_alias->table, old_unique_id, new_unique_id);
		break;
	case table_STATEMENT:
		// Nothing to do here, but we can get here by recursing a table_alias
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
		break;
	}
}
