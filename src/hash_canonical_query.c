/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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
#include <unistd.h>

#include "octo.h"
#include "octo_types.h"
#include "octo_type_check.h"

#include "logical_plan.h"
#include "physical_plan.h"

#include "template_helpers.h"
#include "mmrhash.h" // YottaDB hash functions

/* 0 OR negative values of "*status" are considered normal. Positive values are considered abnormal. */
#define ABNORMAL_STATUS 1

#define ADD_DATA_TYPE_HASH(STATE, DATA_TYPE_STRUCT)                                        \
	{                                                                                  \
		ADD_INT_HASH(STATE, DATA_TYPE_STRUCT.data_type);                           \
		if (SIZE_OR_PRECISION_UNSPECIFIED != DATA_TYPE_STRUCT.size_or_precision) { \
			ADD_INT_HASH(STATE, DATA_TYPE_STRUCT.size_or_precision);           \
		}                                                                          \
		if (SCALE_UNSPECIFIED != DATA_TYPE_STRUCT.scale) {                         \
			ADD_INT_HASH(STATE, DATA_TYPE_STRUCT.scale);                       \
		}                                                                          \
	}

#define ADD_NON_ZERO_GROUP_BY_NUM_TO_HASH(STATE, GROUP_BY_COLUMN_NUM)                                                            \
	{                                                                                                                        \
		/* We want to add `group_by_column_num` value to the hash to avoid mismatch between code generated and the hash. \
		 * This can happen when the following two queries are executed one after the other.                              \
		 * 1. `SELECT nullif('test','test') FROM names GROUP BY nullif('test','test');`                                  \
		 * 2. `SELECT nullif('soml','loms') FROM names GROUP BY nullif('test','test');`                                  \
		 * Without the addition of `group_by_column_num` the second query would end up using                             \
		 * first query's M routine which is incorrect.                                                                   \
		 */                                                                                                              \
		if (0 < GROUP_BY_COLUMN_NUM) {                                                                                   \
			ADD_INT_HASH(state, GROUP_BY_COLUMN_NUM);                                                                \
		}                                                                                                                \
	}
/* Helper function that is invoked when we have to traverse a "column_list_alias_STATEMENT".
 * Caller passes "do_loop" variable set to TRUE  if they want us to traverse the linked list.
 *                              and set to FALSE if they want us to traverse only the first element in the linked list.
 */
void hash_canonical_query_column_list_alias(hash128_state_t *state, SqlStatement *stmt, int *status, boolean_t do_loop) {
	SqlColumnListAlias *start_cla, *cur_cla;

	/* Note: 0 OR negative values of "*status" are considered normal. Positive values are considered abnormal. */
	if ((NULL == stmt) || (0 < *status))
		return;
	// SqlColumnListAlias
	UNPACK_SQL_STATEMENT(start_cla, stmt, column_list_alias);
	cur_cla = start_cla;
	do {
		int save_status;

		ADD_INT_HASH(state, column_list_alias_STATEMENT);
		hash_canonical_query(state, cur_cla->column_list, status); // SqlColumnList
		// SqlValue
		save_status = *status;
		*status = HASH_LITERAL_VALUES; /* force different alias values to generate different plans */
		hash_canonical_query(state, cur_cla->alias, status);
		if (HASH_LITERAL_VALUES == *status) {
			*status = save_status;
		} else {
			/* *status was changed by the "hash_canonical_query()" call. Retain changed value. */
		}
		// SqlOptionalKeyword
		hash_canonical_query(state, cur_cla->keywords, status);
		// SqlValueType
		if (BOOLEAN_OR_STRING_LITERAL == cur_cla->type) {
			/* See comments in later use of this macro in this file for why this type fixing is needed */
			FIX_TYPE_TO_STRING_LITERAL(cur_cla->type);
		}
		ADD_INT_HASH(state, cur_cla->type);
		// boolean_t
		ADD_INT_HASH(state, cur_cla->user_specified_alias);
		// SqlTableIdColumnId
		assert((!cur_cla->tbl_and_col_id.unique_id && !cur_cla->tbl_and_col_id.column_number)
		       || (cur_cla->tbl_and_col_id.unique_id && cur_cla->tbl_and_col_id.column_number));
		if (cur_cla->tbl_and_col_id.unique_id) {
			ADD_INT_HASH(state, cur_cla->tbl_and_col_id.unique_id);
		}
		if (cur_cla->tbl_and_col_id.column_number) {
			ADD_INT_HASH(state, cur_cla->tbl_and_col_id.column_number);
		}
		cur_cla = cur_cla->next;
	} while (do_loop && (cur_cla != start_cla));
	return;
}

void hash_canonical_query(hash128_state_t *state, SqlStatement *stmt, int *status) {
	// Prepares SqlStatement for hashing by adding all statement elements to state using ydb_mmr_hash_128_ingest.
	// Assumes state initialized by caller (using HASH128_STATE_INIT macro)

	/* Note: 0 OR negative values of "*status" are considered normal. Positive values are considered abnormal. */
	if ((NULL == stmt) || (0 < *status))
		return;
	if (stmt->hash_canonical_query_cycle == hash_canonical_query_cycle) {
		switch (stmt->type) {
		case create_table_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
			SqlTable *table;

			ADD_INT_HASH(state, create_table_STATEMENT);
			UNPACK_SQL_STATEMENT(table, stmt, create_table);
			// On a revisit, just hash the table name and return without retraversing.
			assert(value_STATEMENT == table->tableName->type);
			hash_canonical_query(state, table->tableName, status);
			return;
			break;
		case table_value_STATEMENT:
			ADD_INT_HASH(state, table_value_STATEMENT);
			/* A VALUES clause is not a named object (whereas a user defined table has a name).
			 * Therefore, nothing more to hash before returning without retraversing.
			 */
			return;
			break;
		case keyword_STATEMENT:
			// On a revisit, no need to hash anything. Just return without retraversing.
			return;
			break;
		case column_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
			SqlColumn *column;

			ADD_INT_HASH(state, column_STATEMENT);
			UNPACK_SQL_STATEMENT(column, stmt, column);
			// On a revisit, just hash the column piece # and return without retraversing
			assert(column->column_number);
			ADD_INT_HASH(state, column->column_number);
			return;
			break;
		case table_alias_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
			SqlTableAlias *table_alias;

			ADD_INT_HASH(state, table_alias_STATEMENT);
			// On a revisit, just hash the table alias unique_id # and return without retraversing
			// Since unique_id is an int, we can treat it as if it were a type enum
			UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
			ADD_INT_HASH(state, table_alias->unique_id);
			return;
			break;
		default:
			break;
		}
	}
	stmt->hash_canonical_query_cycle = hash_canonical_query_cycle; /* Note down this node as being visited. This avoids
									* multiple visits down this same node in the same
									* outermost call of "hash_canonical_query".
									*/
	assert(stmt->type < invalid_STATEMENT);

	// Below are variables used in multiple "case" blocks below so are declared before the "switch" statement.
	SqlColumn *    start_column, *cur_column;
	SqlTableAlias *table_alias;

	/* Note: The below switch statement and the flow mirrors that in populate_data_type.c.
	 *       Any change here or there needs to also be done in the other module.
	 */
	switch (stmt->type) {
	case cas_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlCaseStatement *cas;

		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		ADD_INT_HASH(state, cas_STATEMENT);
		ADD_NON_ZERO_GROUP_BY_NUM_TO_HASH(state, cas->group_by_fields.group_by_column_num);
		// SqlValue
		hash_canonical_query(state, cas->value, status);
		// SqlCaseBranchStatement
		hash_canonical_query(state, cas->branches, status);
		// SqlValue
		hash_canonical_query(state, cas->optional_else, status);
		break;
	case cas_branch_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlCaseBranchStatement *cas_branch;
		SqlCaseBranchStatement *cur_cas_branch;

		UNPACK_SQL_STATEMENT(cas_branch, stmt, cas_branch);
		cur_cas_branch = cas_branch;
		do {
			ADD_INT_HASH(state, cas_branch_STATEMENT);
			// SqlValue
			hash_canonical_query(state, cur_cas_branch->condition, status);
			// SqlValue
			hash_canonical_query(state, cur_cas_branch->value, status);
			cur_cas_branch = cur_cas_branch->next;
		} while (cur_cas_branch != cas_branch);
		break;
	case insert_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlInsertStatement *insert;
		SqlTableAlias *	    dst_table_alias;

		UNPACK_SQL_STATEMENT(insert, stmt, insert);
		ADD_INT_HASH(state, insert_STATEMENT);
		/* Note: We care only about the "SqlTable" structure (i.e. "dst_table_alias->table"). Hence the below invocation. */
		UNPACK_SQL_STATEMENT(dst_table_alias, insert->dst_table_alias_stmt, table_alias);
		hash_canonical_query(state, dst_table_alias->table, status); // SqlTable
		hash_canonical_query(state, insert->columns, status);	     // SqlColumnList
		/* TRUE as last parameter above indicates "traverse entire linked list", not just "first element" */
		hash_canonical_query(state, insert->src_table_alias_stmt, status); // SqlTableAlias
		hash_canonical_query(state, insert->optional_words, status);	   // SqlOptionalKeyword
		break;
	case delete_from_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlDeleteFromStatement *delete;

		UNPACK_SQL_STATEMENT(delete, stmt, delete_from);
		ADD_INT_HASH(state, delete_from_STATEMENT);
		hash_canonical_query(state, delete->src_join, status); // SqlJoin
		hash_canonical_query(state, delete->where_clause, status);
		break;
	case update_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlUpdateStatement *update;

		UNPACK_SQL_STATEMENT(update, stmt, update);
		ADD_INT_HASH(state, update_STATEMENT);
		hash_canonical_query(state, update->src_join, status); // SqlTable
		hash_canonical_query(state, update->where_clause, status);

		SqlUpdateColumnValue *ucv, *ucv_head;
		ucv_head = update->col_value_list;
		ucv = ucv_head;
		do {
			hash_canonical_query(state, ucv->col_name, status);
			hash_canonical_query(state, ucv->col_value, status);
			ucv = ucv->next;
		} while (ucv != ucv_head);
		break;
	case select_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlSelectStatement *select;

		UNPACK_SQL_STATEMENT(select, stmt, select);
		ADD_INT_HASH(state, select_STATEMENT);
		// SqlColumnListAlias that is a linked list
		hash_canonical_query_column_list_alias(state, select->select_list, status, TRUE);
		// SqlJoin
		hash_canonical_query(state, select->table_list, status);
		// SqlValue (?)
		hash_canonical_query(state, select->where_expression, status);
		// SqlColumnListAlias that is a linked list
		hash_canonical_query_column_list_alias(state, select->group_by_expression, status, TRUE);
		// SqlValue (?)
		hash_canonical_query(state, select->having_expression, status);
		// SqlColumnListAlias that is a linked list
		hash_canonical_query_column_list_alias(state, select->order_by_expression, status, TRUE);
		// SqlOptionalKeyword
		hash_canonical_query(state, select->optional_words, status);
		break;
	case coalesce_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlCoalesceCall *coalesce_call;

		UNPACK_SQL_STATEMENT(coalesce_call, stmt, coalesce);
		ADD_INT_HASH(state, coalesce_STATEMENT);
		hash_canonical_query(state, coalesce_call->arguments, status); // SqlColumnList
		break;
	case greatest_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlGreatest *greatest_call;

		UNPACK_SQL_STATEMENT(greatest_call, stmt, greatest);
		ADD_INT_HASH(state, greatest_STATEMENT);
		hash_canonical_query(state, greatest_call->arguments, status); // SqlColumnList
		break;
	case least_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlLeast *least_call;

		UNPACK_SQL_STATEMENT(least_call, stmt, least);
		ADD_INT_HASH(state, least_STATEMENT);
		hash_canonical_query(state, least_call->arguments, status); // SqlColumnList
		break;
	case null_if_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlNullIf *null_if;

		UNPACK_SQL_STATEMENT(null_if, stmt, null_if);
		ADD_INT_HASH(state, null_if_STATEMENT);
		// SqlColumnList
		hash_canonical_query(state, null_if->left, status);
		hash_canonical_query(state, null_if->right, status);
		break;
	case function_call_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlFunctionCall *function_call;

		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		ADD_INT_HASH(state, function_call_STATEMENT);
		/* Currently, only some functions behave differently depending on Postgres vs MySQL emulation mode.
		 * Hence we include the emulation flag in the hash only for function calls instead of once per query.
		 * This keeps the generated plan the same for both emulation modes in case the query does not use function calls.
		 */
		ADD_INT_HASH(state, config->database_emulation);
		// SqlValue
		hash_canonical_query(state, function_call->function_name, status);
		// SqlValue
		hash_canonical_query(state, function_call->function_schema->v.create_function->extrinsic_function, status);
		hash_canonical_query(state, function_call->parameters, status); // SqlColumnList
		break;
	case aggregate_function_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlAggregateFunction *aggregate_function;

		UNPACK_SQL_STATEMENT(aggregate_function, stmt, aggregate_function);
		ADD_INT_HASH(state, aggregate_function_STATEMENT);
		// SqlAggregateType
		ADD_INT_HASH(state, aggregate_function->type);
		hash_canonical_query(state, aggregate_function->parameter, status); // SqlColumnList
		break;
	case join_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlJoin *cur_join;
		SqlJoin *join;

		UNPACK_SQL_STATEMENT(join, stmt, join);
		cur_join = join;
		do {
			ADD_INT_HASH(state, join_STATEMENT);
			// SqlJoinType
			ADD_INT_HASH(state, cur_join->type);
			// SqlTableAlias
			hash_canonical_query(state, cur_join->value, status);
			// SqlValue
			hash_canonical_query(state, cur_join->condition, status);
			cur_join = cur_join->next;
		} while (cur_join != join);
		break;
	case value_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlValue *value;

		UNPACK_SQL_STATEMENT(value, stmt, value);
		ADD_INT_HASH(state, value_STATEMENT);
		if (BOOLEAN_OR_STRING_LITERAL == value->type) {
			/* All literals with this type that have not already been cast to a BOOLEAN_VALUE type
			 * should default back to a STRING_LITERAL. This is because later stages (e.g. logical plan etc.
			 * rely on never seeing BOOLEAN_OR_STRING_LITERAL type.
			 */
			FIX_TYPE_TO_STRING_LITERAL(value->type);
		}
		// SqlValueType
		ADD_INT_HASH(state, value->type);
		switch (value->type) {
		case CALCULATED_VALUE:
			ADD_NON_ZERO_GROUP_BY_NUM_TO_HASH(state, value->group_by_fields.group_by_column_num);
			hash_canonical_query(state, value->v.calculated, status);
			break;
		case PARAMETER_VALUE:
			// PARAMETER_VALUEs are just literals representing positional parameters, i.e. $1, $2, etc.
		case BOOLEAN_VALUE:
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case STRING_LITERAL:
		case DELIM_VALUE:
		case NUL_VALUE:
			if (HASH_LITERAL_VALUES != *status) {
				// Ignore literals to prevent redundant plans
				break;
			} else {
				/* Caller has asked for literals to be hashed. So do that */
			}
			/* Note: Below comment is needed to avoid gcc [-Wimplicit-fallthrough=] warning */
			/* fall through */
		case FUNCTION_NAME:
		case TABLE_ASTERISK:
		case COLUMN_REFERENCE:
			ydb_mmrhash_128_ingest(state, (void *)value->v.reference, strlen(value->v.reference));
			break;
		case COERCE_TYPE:
			/* Note that we do NOT want to use the ADD_DATA_TYPE_HASH macro here as we want two queries that
			 * use type coercion (say "SELECT 1::NUMERIC(1);" vs "SELECT 1::NUMERIC(2);") to hash to the
			 * same plan if the only difference is in the use of the SIZE/PRECISION and/or SCALE parameters.
			 * The ADD_DATA_TYPE_HASH macro hashes the size/precision and/or scale values too which we do not
			 * want in this case. Hence the simple hash of just the data type ("NUMERIC" in the example case).
			 */
			ADD_INT_HASH(state, value->u.coerce_type.coerced_type.data_type);
			ADD_NON_ZERO_GROUP_BY_NUM_TO_HASH(state, value->group_by_fields.group_by_column_num);
			/* We want to generate different plans for two queries where one specifies a SIZE/PRECISION and/or
			 * SCALE and one does not. For example, "SELECT 1::NUMERIC(1);" vs "SELECT 1::NUMERIC(1,0);".
			 * Hence the additional hash below in the unspecified case. In the unspecified case, we skip this
			 * hash and that will ensure a different plan gets created.
			 */
			if (SIZE_OR_PRECISION_UNSPECIFIED != value->u.coerce_type.coerced_type.size_or_precision) {
				ADD_INT_HASH(state, SIZE_OR_PRECISION_UNSPECIFIED);
			}
			if (SCALE_UNSPECIFIED != value->u.coerce_type.coerced_type.scale) {
				assert(SIZE_OR_PRECISION_UNSPECIFIED != value->u.coerce_type.coerced_type.size_or_precision);
				ADD_INT_HASH(state, SCALE_UNSPECIFIED);
			}
			assert(IS_LITERAL_PARAMETER(value->u.coerce_type.pre_coerced_type)
			       || IS_NUL_VALUE(value->u.coerce_type.pre_coerced_type));
			ADD_INT_HASH(state, value->u.coerce_type.pre_coerced_type);
			hash_canonical_query(state, value->v.coerce_target, status);
			break;
		default:
			assert(FALSE);
			break;
		}
		break;
	case column_alias_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlColumnAlias *column_alias;

		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		ADD_INT_HASH(state, column_alias_STATEMENT);
		// SqlValue (TABLE_ASTERISK) or SqlColumn or SqlColumnListAlias
		hash_canonical_query(state, column_alias->column, status);
		// SqlTableAlias
		hash_canonical_query(state, column_alias->table_alias_stmt, status);
		break;
	case column_list_STATEMENT:;
		SqlColumnList *start_cl, *cur_cl;

		UNPACK_SQL_STATEMENT(start_cl, stmt, column_list);
		cur_cl = start_cl;
		do {
			ADD_INT_HASH(state, column_list_STATEMENT);
			hash_canonical_query(state, cur_cl->value, status); // SqlValue or SqlColumnAlias
			cur_cl = cur_cl->next;
		} while (cur_cl != start_cl);
		break;
	case column_list_alias_STATEMENT:
		hash_canonical_query_column_list_alias(state, stmt, status, FALSE); // FALSE so we do not loop
		break;
	case create_table_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlTable *table;

		UNPACK_SQL_STATEMENT(table, stmt, create_table);
		assert(table->tableName->type == value_STATEMENT);
		ADD_INT_HASH(state, create_table_STATEMENT);
		hash_canonical_query(state, table->tableName, status);
		UNPACK_SQL_STATEMENT(start_column, table->columns, column);
		cur_column = start_column;
		do {
			hash_canonical_query(state, cur_column->columnName, status);
			/* We want to hash two queries using table columns defined as NUMERIC(3) vs NUMERIC(4) into two
			 * different plans. Hence we invoke ADD_DATA_TYPE_HASH macro which takes not just the type but
			 * also any optional size/precision and/or scale if specified.
			 */
			ADD_DATA_TYPE_HASH(state, cur_column->data_type_struct);
			assert(stmt == cur_column->table);
			hash_canonical_query(state, cur_column->delim, status);
			hash_canonical_query(state, cur_column->keywords, status);
			cur_column = cur_column->next;
		} while ((cur_column != start_column));
		hash_canonical_query(state, table->source, status);
		hash_canonical_query(state, table->delim, status);
		/* Note: No need to hash "readwrite" as it is a derived value (derived from various other keywords in the
		 * table that are already part of the hash so it is redundant to hash this one too).
		 *	hash_canonical_query(state, table->readwrite, status);
		 */
		hash_canonical_query(state, table->aim_type, status);
		break;
	case table_value_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlTableValue *table_value;

		UNPACK_SQL_STATEMENT(table_value, stmt, table_value);
		ADD_INT_HASH(state, table_value_STATEMENT);
		start_column = table_value->column;
		cur_column = start_column;
		do {
			hash_canonical_query(state, cur_column->columnName, status);
			/* We want to hash two queries using table columns defined as NUMERIC(3) vs NUMERIC(4) into two
			 * different plans. Hence we invoke ADD_DATA_TYPE_HASH macro which takes not just the type but
			 * also any optional size/precision and/or scale if specified.
			 */
			ADD_DATA_TYPE_HASH(state, cur_column->data_type_struct);
			assert(NULL == cur_column->delim);
			assert(NULL == cur_column->keywords);
			cur_column = cur_column->next;
		} while (cur_column != start_column);

		SqlRowValue *row_value, *start_row_value;

		UNPACK_SQL_STATEMENT(row_value, table_value->row_value_stmt, row_value);
		start_row_value = row_value;
		do {
			hash_canonical_query(state, row_value->value_list, status); // SqlColumnList
			row_value = row_value->next;
		} while (row_value != start_row_value);
		break;
	case drop_function_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlDropFunctionStatement *drop_function;

		/* DROP FUNCTION statements are only hashed for the purpose of looking up the function version targeted for
		 * deletion. Accordingly, we use the statement type used at the time of creation, i.e. create_function_STATEMENT.
		 */
		UNPACK_SQL_STATEMENT(drop_function, stmt, drop_function);
		assert(drop_function->function_name->type == value_STATEMENT);
		hash_canonical_query(state, drop_function->function_name, status);
		hash_canonical_query(state, drop_function->parameter_type_list, status);
		break;
	case create_function_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlFunction *function;

		UNPACK_SQL_STATEMENT(function, stmt, create_function);
		assert(function->function_name->type == value_STATEMENT);
		hash_canonical_query(state, function->function_name, status);
		hash_canonical_query(state, function->parameter_type_list, status);
		break;
	case parameter_type_list_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlParameterTypeList *parameter_type_list;
		SqlParameterTypeList *cur_parameter_type_list;

		UNPACK_SQL_STATEMENT(parameter_type_list, stmt, parameter_type_list);
		cur_parameter_type_list = parameter_type_list;
		do {
			SqlDataType data_type;

			assert(data_type_struct_STATEMENT == cur_parameter_type_list->data_type_struct->type);
			/* Convert the SQL data type to internal SqlValueType for compatibility with function call parameter types
			 * inferred in populate_data_type. This conversion is acceptable here since the only purpose of hashing
			 * these values at all is for matching function calls to previously CREATEd functions which may have
			 * overloaded function names.
			 */
			data_type = cur_parameter_type_list->data_type_struct->v.data_type_struct.data_type;
			ADD_INT_HASH(state, get_sqlvaluetype_from_sqldatatype(data_type, FALSE));
			cur_parameter_type_list = cur_parameter_type_list->next;
		} while (cur_parameter_type_list != parameter_type_list);
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		ADD_INT_HASH(state, table_alias_STATEMENT);
		// SqlTable or SqlSelectStatement
		hash_canonical_query(state, table_alias->table, status);
		// SqlValue
		hash_canonical_query(state, table_alias->alias, status);
		// Since unique_id is an int, can use treat it as if it were a type enum
		ADD_INT_HASH(state, table_alias->unique_id);
		// SqlColumnListAlias
		/* If table_alias->table is of type "select_STATEMENT", we can skip "table_alias->column_list"
		 *	as that would have been already traversed as part of "table_alias->table->v.select->select_list" above.
		 * If table_alias->table is of type "create_table_STATEMENT", then the "table_alias->column_list" is derived
		 *	from the list of all available columns in the corresponding SqlTable. In this case too, there is no need
		 *	to go through all the available columns in the table. We are interested only in the columns that this
		 *	query is interested in which would already be part of the SELECT column list or other parts of the query.
		 * If table_alias->table is of type "table_value_STATEMENT", then the "table_alias->column_list" is derived
		 *	from the list of all available columns in the corresponding "table_alias->table" structure.
		 * And since these are the only 3 types possible, no need to traverse "table_alias->column_list" as part of
		 *	"hash_canonical_query". This is asserted below.
		 */
		assert((select_STATEMENT == table_alias->table->type) || (create_table_STATEMENT == table_alias->table->type)
		       || (table_value_STATEMENT == table_alias->table->type));
		assert((select_STATEMENT != table_alias->table->type)
		       || (table_alias->table->v.select->select_list == table_alias->column_list));
		// hash_canonical_query(state, table_alias->column_list, status);
		break;
	case binary_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlBinaryOperation *binary;
		BinaryOperations    binary_operation;

		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		ADD_INT_HASH(state, binary_STATEMENT);
		ADD_NON_ZERO_GROUP_BY_NUM_TO_HASH(state, binary->group_by_fields.group_by_column_num);
		// BinaryOperations
		binary_operation = binary->operation;
		ADD_INT_HASH(state, binary_operation);
		// SqlStatement (?)
		hash_canonical_query(state, binary->operands[0], status);
		hash_canonical_query(state, binary->operands[1], status);
		break;
	case set_operation_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlSetOperation *set_operation;

		UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
		ADD_INT_HASH(state, set_operation_STATEMENT);
		// SqlSetOperationType
		ADD_INT_HASH(state, set_operation->type);
		// SqlStatement (?)
		hash_canonical_query(state, set_operation->operand[0], status);
		// SqlStatement (?)
		hash_canonical_query(state, set_operation->operand[1], status);
		break;
	case unary_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlUnaryOperation *unary;

		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		ADD_INT_HASH(state, unary_STATEMENT);
		ADD_NON_ZERO_GROUP_BY_NUM_TO_HASH(state, unary->group_by_fields.group_by_column_num);
		// UnaryOperations
		ADD_INT_HASH(state, unary->operation);
		// SqlStatement (?)
		hash_canonical_query(state, unary->operand, status);
		break;
	case array_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlArray *array;

		UNPACK_SQL_STATEMENT(array, stmt, array);
		ADD_INT_HASH(state, array_STATEMENT);
		hash_canonical_query(state, array->argument, status);
		break;
	case keyword_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		// This is a valid case in "hash_canonical_query" but is not a case in "populate_data_type"
		SqlOptionalKeyword *keyword;
		SqlOptionalKeyword *cur_keyword;

		UNPACK_SQL_STATEMENT(keyword, stmt, keyword);
		cur_keyword = keyword;
		do {
			int save_status;

			/* Skip hashing NO_KEYWORD (as it creates different physical plans for otherwise identical queries).
			 * Skip hashing OPTIONAL_KEY_NUM keyword as it is only used internally (PRIMARY KEY constraint
			 * is exposed to the user externally in the CREATE TABLE command instead).
			 */
			if ((NO_KEYWORD != cur_keyword->keyword) && (OPTIONAL_KEY_NUM != cur_keyword->keyword)) {
				ADD_INT_HASH(state, keyword_STATEMENT);
				// OptionalKeyword
				ADD_INT_HASH(state, cur_keyword->keyword);
				/* SqlValue or SqlSelectStatement.
				 * 1) Literals in "value_STATEMENT" are in general not hashed but in this case, we could have
				 *    keywords like "OPTIONAL_PIECE" which hold the column piece # information. We do want that
				 *    hashed or else we could have different queries hashing to the same plan. Hence the set of
				 *    "*status" to "HASH_LITERAL_VALUES" so "hash_canonical_query()" knows that keyword literals
				 *    need to be hashed.
				 * 2) In addition, keywords like "OPTIONAL_CHECK_CONSTRAINT" point to boolean expressions, not
				 *    just literals. But literals inside those boolean expressions should be hashed that way
				 *    different check constraints that differ only in literal values end up with different hashes.
				 * 3) An exception is "OPTIONAL_LIMIT" keyword where we we want multiple queries that differ only
				 *    in the LIMIT value (e.g. "SELECT * from names LIMIT 1" vs "SELECT * from names LIMIT 2")
				 *    to hash to the same plan. So skip the HASH_LITERAL_VALUES set for this case.
				 */
				save_status = *status;
				if (OPTIONAL_LIMIT != cur_keyword->keyword) {
					*status = HASH_LITERAL_VALUES;
				}
				hash_canonical_query(state, cur_keyword->v, status);
				if (HASH_LITERAL_VALUES == *status) {
					*status = save_status;
				} else {
					/* *status was changed by the "hash_canonical_query()" call. Retain changed value. */
				}
			}
			cur_keyword = cur_keyword->next;
		} while (cur_keyword != keyword);
		break;
	case column_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		// This is a valid case in "hash_canonical_query" but is not a case in "populate_data_type"
		SqlColumn *column;

		UNPACK_SQL_STATEMENT(column, stmt, column);
		ADD_INT_HASH(state, column_STATEMENT);
		hash_canonical_query(state, column->columnName, status);
		// SqlDataType
		ADD_INT_HASH(state, column->data_type_struct.data_type);
		hash_canonical_query(state, column->table, status);
		hash_canonical_query(state, column->keywords, status);
		break;
	case constraint_STATEMENT:;
		// This is a valid case in "hash_canonical_query" but is not a case in "populate_data_type"
		SqlConstraint *constraint;

		UNPACK_SQL_STATEMENT(constraint, stmt, constraint);
		ADD_INT_HASH(state, constraint_STATEMENT);
		/* No need to hash the constraint type as it would have been already hashed as part of
		 * processing the SqlKeyword structure that contained this constraint. That has a "keyword" member
		 * which is identical to "constraint->type".
		 *	hash_canonical_query(state, constraint->type, status);
		 */
		hash_canonical_query(state, constraint->name, status);
		hash_canonical_query(state, constraint->definition, status);
		/* "constraint->v.check_columns" and "constraint->v.uniq_gblname" is information derived from
		 * "constraint->definition" and so no need to run the below.
		 *
		 * When OPTIONAL_CHECK_CONSTRAINT == constraint->type
		 *	hash_canonical_query(state, constraint->v.check_columns, status);
		 * When UNIQUE_CONSTRAINT == constraint->type
		 *	hash_canonical_query(state, constraint->v.uniq_gblname, status);
		 * When PRIMARY_KEY == constraint->type
		 *	Nothing to hash
		 */
		break;
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		*status = ABNORMAL_STATUS;
		break;
	}
}
