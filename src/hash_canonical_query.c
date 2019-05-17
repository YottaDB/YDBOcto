/* Copyright (C) 2018-2019 YottaDB, LLC
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
#include <unistd.h>

#include "octo.h"
#include "octo_types.h"

#include "logical_plan.h"
#include "physical_plan.h"

#include "template_helpers.h"
#include "mmrhash.h"		// YottaDB hash functions

void add_sql_type_hash(hash128_state_t *state, int stmt_type) {
	// Helper function: adds statement type values to hash digest
	ydb_mmrhash_128_ingest(state, (void*)&stmt_type, sizeof(int));
}

void hash_canonical_query(hash128_state_t *state, SqlStatement *stmt) {
	// Prepares SqlStatement for hashing by adding all statement elements to state using ydb_mmr_hash_128_ingest.
	// Assumes state initialized by caller (using HASH128_STATE_INIT macro)

	// Statements
	SqlCaseStatement *cas;
	SqlCaseBranchStatement *cas_branch;
	SqlDropStatement *drop;
	SqlSelectStatement *select;
	SqlSetStatement *set;
	SqlShowStatement *show;

	SqlFunctionCall *function_call;
	SqlJoin *join;
	SqlValue *value;

	// Columns and tables
	SqlColumn *column; // Note singular versus plural
	SqlColumnAlias *column_alias;
	SqlColumnList *column_list;
	SqlColumnListAlias *column_list_alias;
	SqlTable *table;
	SqlTableAlias *table_alias;

	// Operations and keywords
	SqlBinaryOperation *binary;
	SqlSetOperation *set_operation;
	SqlUnaryOperation *unary;
	SqlOptionalKeyword *keyword;

	// Iteration pointers for doubly-linked list traversal
	SqlCaseBranchStatement *cur_cas_branch;
	SqlColumn *cur_column;
	SqlColumnList *cur_column_list;
	SqlColumnListAlias *cur_column_list_alias;
	SqlJoin *cur_join;
	SqlOptionalKeyword *cur_keyword;

	if(stmt == NULL)
		return;

	switch (stmt->type) {
		case begin_STATEMENT:
			add_sql_type_hash(state, begin_STATEMENT);
			break;
		case cas_STATEMENT:
			UNPACK_SQL_STATEMENT(cas, stmt, cas);
			add_sql_type_hash(state, cas_STATEMENT);
			// SqlValue
			hash_canonical_query(state, cas->value);
			// SqlCaseBranchStatement
			hash_canonical_query(state, cas->branches);
			// SqlValue
			hash_canonical_query(state, cas->optional_else);
			break;
		case cas_branch_STATEMENT:
			UNPACK_SQL_STATEMENT(cas_branch, stmt, cas_branch);
			cur_cas_branch = cas_branch->next;
			do {
				add_sql_type_hash(state, cas_branch_STATEMENT);
				// SqlValue
				hash_canonical_query(state, cas_branch->condition);
				// SqlValue
				hash_canonical_query(state, cas_branch->value);
				cur_cas_branch = cur_cas_branch->next;
			} while (cur_cas_branch != cas_branch->next);
			break;
		case commit_STATEMENT:
			add_sql_type_hash(state, commit_STATEMENT);
			break;
		case drop_STATEMENT:
			UNPACK_SQL_STATEMENT(drop, stmt, drop);
			add_sql_type_hash(state, drop_STATEMENT);
			// SqlValue
			hash_canonical_query(state, drop->table_name);
			// SqlOptionalKeyword
			hash_canonical_query(state, drop->optional_keyword);
			break;
		case insert_STATEMENT:
			// NOT IMPLEMENTED
			assert(FALSE);
			break;
		case no_data_STATEMENT:
			add_sql_type_hash(state, no_data_STATEMENT);
			break;
		case select_STATEMENT:
			UNPACK_SQL_STATEMENT(select, stmt, select);
			add_sql_type_hash(state, select_STATEMENT);
			// SqlColumnListAlias
			hash_canonical_query(state, select->select_list);
			// SqlJoin
			hash_canonical_query(state, select->table_list);
			// SqlValue (?)
			hash_canonical_query(state, select->where_expression);
			// SqlValue (?)
			hash_canonical_query(state, select->order_expression);
			// SqlOptionalKeyword
			hash_canonical_query(state, select->optional_words);
			// SqlSetOperation
			hash_canonical_query(state, select->set_operation);
			break;
		case set_STATEMENT:
			UNPACK_SQL_STATEMENT(set, stmt, set);
			add_sql_type_hash(state, set_STATEMENT);
			hash_canonical_query(state, set->variable);
			hash_canonical_query(state, set->value);
			break;
		case show_STATEMENT:
			UNPACK_SQL_STATEMENT(show, stmt, show);
			add_sql_type_hash(state, show_STATEMENT);
			hash_canonical_query(state, show->variable);
			break;
		case function_call_STATEMENT:
			UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
			add_sql_type_hash(state, function_call_STATEMENT);
			// SqlValue
			hash_canonical_query(state, function_call->function_name);
			// SqlColumnList
			hash_canonical_query(state, function_call->parameters);
			break;
		case join_STATEMENT:
			UNPACK_SQL_STATEMENT(join, stmt, join);
			cur_join = join->next;
			do {
				add_sql_type_hash(state, join_STATEMENT);
				// SqlTableAlias
				hash_canonical_query(state, join->value);
				// SqlValue
				hash_canonical_query(state, join->condition);
				cur_join = cur_join->next;
			} while (cur_join != join->next);
			// SqlJoinType
			add_sql_type_hash(state, join->type);
			break;
		case value_STATEMENT:
			UNPACK_SQL_STATEMENT(value, stmt, value);
			add_sql_type_hash(state, value_STATEMENT);
			// SqlValueType
			add_sql_type_hash(state, value->type);
			// SqlDataType
			add_sql_type_hash(state, value->data_type);
			switch(value->type) {
				case (STRING_LITERAL):
					ydb_mmrhash_128_ingest(state, (void*)value->v.string_literal, strlen(value->v.string_literal));
					break;
				case (COLUMN_REFERENCE):
					ydb_mmrhash_128_ingest(state, (void*)value->v.reference, strlen(value->v.reference));
					break;
				case (CALCULATED_VALUE):
					hash_canonical_query(state, value->v.calculated);
					break;
				default:
					assert(FALSE);
					break;
			}
			break;
		case column_STATEMENT:
			UNPACK_SQL_STATEMENT(column, stmt, column);
			cur_column = column;
			do {
				add_sql_type_hash(state, column_STATEMENT);
				hash_canonical_query(state, column->columnName);
				// SqlDataType
				add_sql_type_hash(state, column->type);
				hash_canonical_query(state, column->table);
				hash_canonical_query(state, column->keywords);
				cur_column = cur_column->next;
			} while (cur_column != column);
			break;
		case column_alias_STATEMENT:
			UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
			add_sql_type_hash(state, column_alias_STATEMENT);
			// SqlColumn or SqlColumnListAlias
			hash_canonical_query(state, column_alias->column);
			// SqlTableAlias
			// hash_canonical_query(state, column_alias->table_alias);
			break;
		case column_list_STATEMENT:
			UNPACK_SQL_STATEMENT(column_list, stmt, column_list);
			cur_column_list = column_list;
			do {
				add_sql_type_hash(state, column_list_STATEMENT);
				// SqlValue or SqlColumnAlias
				hash_canonical_query(state, column_list->value);
				cur_column_list = cur_column_list->next;
			} while (cur_column_list != column_list);
			break;
		case column_list_alias_STATEMENT:
			UNPACK_SQL_STATEMENT(column_list_alias, stmt, column_list_alias);
			cur_column_list_alias = column_list_alias;
			do {
				add_sql_type_hash(state, column_list_alias_STATEMENT);
				// SqlColumnList
				hash_canonical_query(state, column_list_alias->column_list);
				// SqlValue
				hash_canonical_query(state, column_list_alias->alias);
				// SqlOptionalKeyword
				hash_canonical_query(state, column_list_alias->keywords);
				// SqlValueType
				add_sql_type_hash(state, column_list_alias->type);
				cur_column_list_alias = cur_column_list_alias->next;
			} while (cur_column_list_alias != column_list_alias);
			break;
		case table_STATEMENT:
			UNPACK_SQL_STATEMENT(table, stmt, table);
			assert(table->tableName->type == value_STATEMENT);
			add_sql_type_hash(state, table_STATEMENT);
			hash_canonical_query(state, table->tableName);
			hash_canonical_query(state, table->source);
			hash_canonical_query(state, table->delim);
			break;
		case table_alias_STATEMENT:
			UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
			add_sql_type_hash(state, table_alias_STATEMENT);
			// SqlTable or SqlSelectStatement
			hash_canonical_query(state, table_alias->table);
			// SqlValue
			hash_canonical_query(state, table_alias->alias);
			// Since unique_id is an int, can use treat it as if it were a type enum
			add_sql_type_hash(state, table_alias->unique_id);
			// SqlColumnListAlias
			hash_canonical_query(state, table_alias->column_list);
			break;
		case binary_STATEMENT:
			UNPACK_SQL_STATEMENT(binary, stmt, binary);
			add_sql_type_hash(state, binary_STATEMENT);
			// BinaryOperations
			add_sql_type_hash(state, binary->operation);
			// SqlStatement (?)
			hash_canonical_query(state, binary->operands[0]);
			// SqlStatement (?)
			hash_canonical_query(state, binary->operands[1]);
			break;
		case set_operation_STATEMENT:
			UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
			add_sql_type_hash(state, set_operation_STATEMENT);
			// SqlSetOperationType
			printf(" set op type: %d ", set_operation->type);
			add_sql_type_hash(state, set_operation->type);
			// SqlStatement (?)
			// Infinite recursion - points to its parent select_STATEMENT
			// hash_canonical_query(state, set_operation->operand[0]);
			// SqlStatement (?)
			hash_canonical_query(state, set_operation->operand[1]);
			break;
		case unary_STATEMENT:
			UNPACK_SQL_STATEMENT(unary, stmt, unary);
			add_sql_type_hash(state, unary_STATEMENT);
			// UnaryOperations
			add_sql_type_hash(state, unary->operation);
			// SqlStatement (?)
			hash_canonical_query(state, unary->operand);
			break;
		case constraint_STATEMENT:
			// NOT IMPLEMENTED
			// UNPACK_SQL_STATEMENT(constraint, stmt, constraint);
			add_sql_type_hash(state, constraint_STATEMENT);
			break;
		case keyword_STATEMENT:
			UNPACK_SQL_STATEMENT(keyword, stmt, keyword);
			cur_keyword = keyword;
			do {
				// add_sql_type_hash(state, keyword_STATEMENT);
				// OptionalKeyword
				add_sql_type_hash(state, keyword->keyword);
				// SqlValue or SqlSelectStatement
				hash_canonical_query(state, keyword->v);
				cur_keyword = cur_keyword->next;
			} while (cur_keyword != keyword);
			break;
		default:
			FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
			break;
	}

}
