/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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

#include "logical_plan.h"
#include "physical_plan.h"

#include "template_helpers.h"
#include "mmrhash.h" // YottaDB hash functions

/* 0 OR negative values of "*status" are considered normal. Positive values are considered abnormal. */
#define HASH_LITERAL_VALUES -1
#define ABNORMAL_STATUS	    1

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

// Helper function that is invoked when we have to traverse a "column_list_alias_STATEMENT".
// Caller passes "do_loop" variable set to TRUE  if they want us to traverse the linked list.
//                              and set to FALSE if they want us to traverse only the first element in the linked list.
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
		// SqlColumnList
		hash_canonical_query(state, cur_cla->column_list, status);
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

// Helper function that is invoked when we have to traverse a "column_list_STATEMENT".
// Caller passes "do_loop" variable set to TRUE  if they want us to traverse the linked list.
//                              and set to FALSE if they want us to traverse only the first element in the linked list.
void hash_canonical_query_column_list(hash128_state_t *state, SqlStatement *stmt, int *status, boolean_t do_loop) {
	SqlColumnList *column_list, *cur_column_list;

	/* Note: 0 OR negative values of "*status" are considered normal. Positive values are considered abnormal. */
	if ((NULL == stmt) || (0 < *status))
		return;
	// SqlColumnList
	UNPACK_SQL_STATEMENT(column_list, stmt, column_list);
	cur_column_list = column_list;
	do {
		ADD_INT_HASH(state, column_list_STATEMENT);
		// SqlValue or SqlColumnAlias
		hash_canonical_query(state, cur_column_list->value, status);
		cur_column_list = cur_column_list->next;
	} while (do_loop && (cur_column_list != column_list));
	return;
}

void hash_canonical_query(hash128_state_t *state, SqlStatement *stmt, int *status) {
	// Prepares SqlStatement for hashing by adding all statement elements to state using ydb_mmr_hash_128_ingest.
	// Assumes state initialized by caller (using HASH128_STATE_INIT macro)

	// Statements
	SqlCaseStatement *	cas;
	SqlCaseBranchStatement *cas_branch;
	SqlSelectStatement *	select;
	SqlInsertStatement *	insert;

	SqlFunctionCall *function_call;
	SqlCoalesceCall *coalesce_call;
	SqlGreatest *	 greatest_call;
	SqlLeast *	 least_call;
	SqlNullIf *	 null_if;

	SqlAggregateFunction *aggregate_function;
	SqlJoin *	      join;
	SqlValue *	      value;

	// Columns and tables
	SqlColumn *	column, *start_column, *cur_column;
	SqlColumnAlias *column_alias;
	SqlTable *	table;
	SqlTableAlias * table_alias;

	SqlFunction *		  function;
	SqlDropFunctionStatement *drop_function;
	SqlParameterTypeList *	  parameter_type_list;

	SqlArray *array;

	// Operations and keywords
	SqlBinaryOperation *binary;
	SqlSetOperation *   set_operation;
	SqlUnaryOperation * unary;
	SqlOptionalKeyword *keyword;

	// Iteration pointers for doubly-linked list traversal
	SqlCaseBranchStatement *cur_cas_branch;
	SqlJoin *		cur_join;
	SqlOptionalKeyword *	cur_keyword;
	SqlParameterTypeList *	cur_parameter_type_list;
	BinaryOperations	binary_operation;
	SqlTableValue *		table_value;
	SqlRowValue *		row_value, *start_row_value;

	/* Note: 0 OR negative values of "*status" are considered normal. Positive values are considered abnormal. */
	if ((NULL == stmt) || (0 < *status))
		return;
	if (stmt->hash_canonical_query_cycle == hash_canonical_query_cycle) {
		switch (stmt->type) {
		case create_table_STATEMENT:
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
		case column_STATEMENT:
			ADD_INT_HASH(state, column_STATEMENT);
			UNPACK_SQL_STATEMENT(column, stmt, column);
			// On a revisit, just hash the column piece # and return without retraversing
			assert(column->column_number);
			ADD_INT_HASH(state, column->column_number);
			return;
			break;
		case table_alias_STATEMENT:
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
	// Note: The below switch statement and the flow mirrors that in populate_data_type.c.
	//       Any change here or there needs to also be done in the other module.
	switch (stmt->type) {
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		ADD_INT_HASH(state, cas_STATEMENT);
		// SqlValue
		hash_canonical_query(state, cas->value, status);
		// SqlCaseBranchStatement
		hash_canonical_query(state, cas->branches, status);
		// SqlValue
		hash_canonical_query(state, cas->optional_else, status);
		break;
	case cas_branch_STATEMENT:
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
	case insert_STATEMENT:
		UNPACK_SQL_STATEMENT(insert, stmt, insert);
		ADD_INT_HASH(state, insert_STATEMENT);
		/* Note: We care only about the "SqlTable" structure (i.e. "dst_table_alias->table"). No other fields
		 * inside "dst_table_alias" are used (even in the later logical plan stage). Hence the below invocation.
		 */
		hash_canonical_query(state, insert->dst_table_alias->table, status);	// SqlTable
		hash_canonical_query_column_list(state, insert->columns, status, TRUE); // SqlColumnList
		/* TRUE as last parameter above indicates "traverse entire linked list", not just "first element" */
		hash_canonical_query(state, insert->src_table_alias_stmt, status); // SqlTableAlias
		break;
	case select_STATEMENT:
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
	case coalesce_STATEMENT:
		UNPACK_SQL_STATEMENT(coalesce_call, stmt, coalesce);
		ADD_INT_HASH(state, coalesce_STATEMENT);
		// SqlColumnList
		hash_canonical_query_column_list(state, coalesce_call->arguments, status, TRUE);
		break;
	case greatest_STATEMENT:
		UNPACK_SQL_STATEMENT(greatest_call, stmt, greatest);
		ADD_INT_HASH(state, greatest_STATEMENT);
		// SqlColumnList
		hash_canonical_query_column_list(state, greatest_call->arguments, status, TRUE);
		break;
	case least_STATEMENT:
		UNPACK_SQL_STATEMENT(least_call, stmt, least);
		ADD_INT_HASH(state, least_STATEMENT);
		// SqlColumnList
		hash_canonical_query_column_list(state, least_call->arguments, status, TRUE);
		break;
	case null_if_STATEMENT:
		UNPACK_SQL_STATEMENT(null_if, stmt, null_if);
		ADD_INT_HASH(state, null_if_STATEMENT);
		// SqlColumnList
		hash_canonical_query(state, null_if->left, status);
		hash_canonical_query(state, null_if->right, status);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		ADD_INT_HASH(state, function_call_STATEMENT);
		// SqlValue
		hash_canonical_query(state, function_call->function_name, status);
		// SqlValue
		hash_canonical_query(state, function_call->function_schema->v.create_function->extrinsic_function, status);
		// SqlColumnList
		hash_canonical_query_column_list(state, function_call->parameters, status, TRUE);
		break;
	case aggregate_function_STATEMENT:
		UNPACK_SQL_STATEMENT(aggregate_function, stmt, aggregate_function);
		ADD_INT_HASH(state, aggregate_function_STATEMENT);
		// SqlAggregateType
		ADD_INT_HASH(state, aggregate_function->type);
		// SqlColumnList : table.* case will have multiple nodes so loop through it
		hash_canonical_query_column_list(state, aggregate_function->parameter, status, TRUE);
		break;
	case join_STATEMENT:
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
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		ADD_INT_HASH(state, value_STATEMENT);
		// SqlValueType
		ADD_INT_HASH(state, value->type);
		switch (value->type) {
		case CALCULATED_VALUE:
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
			ADD_INT_HASH(state, value->coerced_type.data_type);
			/* We want to generate different plans for two queries where one specifies a SIZE/PRECISION and/or
			 * SCALE and one does not. For example, "SELECT 1::NUMERIC(1);" vs "SELECT 1::NUMERIC(1,0);".
			 * Hence the additional hash below in the unspecified case. In the unspecified case, we skip this
			 * hash and that will ensure a different plan gets created.
			 */
			if (SIZE_OR_PRECISION_UNSPECIFIED != value->coerced_type.size_or_precision) {
				ADD_INT_HASH(state, SIZE_OR_PRECISION_UNSPECIFIED);
			}
			if (SCALE_UNSPECIFIED != value->coerced_type.scale) {
				assert(SIZE_OR_PRECISION_UNSPECIFIED != value->coerced_type.size_or_precision);
				ADD_INT_HASH(state, SCALE_UNSPECIFIED);
			}
			assert(IS_LITERAL_PARAMETER(value->pre_coerced_type) || (NUL_VALUE == value->pre_coerced_type));
			ADD_INT_HASH(state, value->pre_coerced_type);
			hash_canonical_query(state, value->v.coerce_target, status);
			break;
		default:
			assert(FALSE);
			break;
		}
		break;
	case column_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		ADD_INT_HASH(state, column_alias_STATEMENT);
		// SqlValue (TABLE_ASTERISK) or SqlColumn or SqlColumnListAlias
		hash_canonical_query(state, column_alias->column, status);
		// SqlTableAlias
		hash_canonical_query(state, column_alias->table_alias_stmt, status);
		break;
	case column_list_STATEMENT:
		hash_canonical_query_column_list(state, stmt, status, FALSE); // FALSE so we do not loop
		break;
	case column_list_alias_STATEMENT:
		hash_canonical_query_column_list_alias(state, stmt, status, FALSE); // FALSE so we do not loop
		break;
	case create_table_STATEMENT:
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
		break;
	case table_value_STATEMENT:
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
		UNPACK_SQL_STATEMENT(row_value, table_value->row_value_stmt, row_value);
		start_row_value = row_value;
		do {
			SqlColumnList *start_column_list, *cur_column_list;

			UNPACK_SQL_STATEMENT(start_column_list, row_value->value_list, column_list);
			cur_column_list = start_column_list;
			do {
				hash_canonical_query(state, cur_column_list->value, status);
				cur_column_list = cur_column_list->next;
			} while ((cur_column_list != start_column_list));
			row_value = row_value->next;
		} while (row_value != start_row_value);
		break;
	case drop_function_STATEMENT:
		/* DROP FUNCTION statements are only hashed for the purpose of looking up the function version targeted for
		 * deletion. Accordingly, we use the statement type used at the time of creation, i.e. create_function_STATEMENT.
		 */
		UNPACK_SQL_STATEMENT(drop_function, stmt, drop_function);
		assert(drop_function->function_name->type == value_STATEMENT);
		hash_canonical_query(state, drop_function->function_name, status);
		hash_canonical_query(state, drop_function->parameter_type_list, status);
		break;
	case create_function_STATEMENT:
		UNPACK_SQL_STATEMENT(function, stmt, create_function);
		assert(function->function_name->type == value_STATEMENT);
		hash_canonical_query(state, function->function_name, status);
		hash_canonical_query(state, function->parameter_type_list, status);
		break;
	case parameter_type_list_STATEMENT:
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
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		ADD_INT_HASH(state, binary_STATEMENT);
		// BinaryOperations
		binary_operation = binary->operation;
		ADD_INT_HASH(state, binary_operation);
		// SqlStatement (?)
		hash_canonical_query(state, binary->operands[0], status);
		if (((BOOLEAN_IN == binary_operation) || (BOOLEAN_NOT_IN == binary_operation))
		    && (column_list_STATEMENT == binary->operands[1]->type)) { // SqlColumnList
			hash_canonical_query_column_list(state, binary->operands[1], status, TRUE);
		} else { // SqlStatement (?)
			hash_canonical_query(state, binary->operands[1], status);
		}
		break;
	case set_operation_STATEMENT:
		UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
		ADD_INT_HASH(state, set_operation_STATEMENT);
		// SqlSetOperationType
		ADD_INT_HASH(state, set_operation->type);
		// SqlStatement (?)
		hash_canonical_query(state, set_operation->operand[0], status);
		// SqlStatement (?)
		hash_canonical_query(state, set_operation->operand[1], status);
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		ADD_INT_HASH(state, unary_STATEMENT);
		// UnaryOperations
		ADD_INT_HASH(state, unary->operation);
		// SqlStatement (?)
		hash_canonical_query(state, unary->operand, status);
		break;
	case array_STATEMENT:
		UNPACK_SQL_STATEMENT(array, stmt, array);
		ADD_INT_HASH(state, array_STATEMENT);
		hash_canonical_query(state, array->argument, status);
		break;
	case keyword_STATEMENT: // This is a valid case in "hash_canonical_query" but is not a case in "populate_data_type"
		UNPACK_SQL_STATEMENT(keyword, stmt, keyword);
		cur_keyword = keyword;
		do {
			int save_status;

			/* Skip hashing NO_KEYWORD (as it creates different physical plans for otherwise identical queries) */
			if (NO_KEYWORD != cur_keyword->keyword) {
				ADD_INT_HASH(state, keyword_STATEMENT);
				// OptionalKeyword
				ADD_INT_HASH(state, cur_keyword->keyword);
				/* SqlValue or SqlSelectStatement.
				 * Literals in "value_STATEMENT" are in general not hashed but in this case, we could have keywords
				 * like "OPTIONAL_PIECE" which hold the column piece # information. We do want that hashed or else
				 * we could have different queries hashing to the same plan. Hence the set of "*status" to
				 * "HASH_LITERAL_VALUES" so "hash_canonical_query()" knows that keyword literals need to be hashed.
				 * An exception is "OPTIONAL_LIMIT" keyword where we we want multiple queries that differ only in
				 * the LIMIT value (e.g. SELECT * from names LIMIT 1 vs SELECT * from names LIMIT 2) to hash to the
				 * same plan. So skip the HASH_LITERAL_VALUES set for this case.
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
	case column_STATEMENT: // This is a valid case in "hash_canonical_query" but is not a case in "populate_data_type"
		UNPACK_SQL_STATEMENT(column, stmt, column);
		ADD_INT_HASH(state, column_STATEMENT);
		hash_canonical_query(state, column->columnName, status);
		// SqlDataType
		ADD_INT_HASH(state, column->data_type_struct.data_type);
		hash_canonical_query(state, column->table, status);
		hash_canonical_query(state, column->keywords, status);
		break;
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		*status = ABNORMAL_STATUS;
		break;
	}
}
