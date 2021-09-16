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
#include <string.h>

#include "octo.h"
#include "octo_types.h"

/* Below is the maximum length of the string consisting of type names for each of an SQL function's parameters.
 * The type name can be "INTEGER", "NUMERIC", "VARCHAR" etc. so we use 16 as an upper bound for this.
 * The maximum number of parameters for a SQL function is YDB_MAX_PARMS (checked in "src/store_function_in_pg_proc.c").
 * Therefore the below provides a good maximum.
 */
#define MAX_FUNC_TYPES_LEN (YDB_MAX_PARMS * 16)

#define ISSUE_TYPE_COMPATIBILITY_ERROR(CHILD_TYPE, OPERATION, OPERAND, RESULT)                       \
	{                                                                                            \
		ERROR(ERR_TYPE_NOT_COMPATIBLE, get_user_visible_type_string(CHILD_TYPE), OPERATION); \
		yyerror(NULL, NULL, OPERAND, NULL, NULL, NULL);                                      \
		RESULT = 1;                                                                          \
	}

#define MAP_TYPE_TO_PARAMETER_VALUE(TYPE_1, TYPE_2, RESULT, PARSE_CONTEXT)                                                        \
	{                                                                                                                         \
		if ((PARAMETER_VALUE == TYPE_2) && (PARAMETER_VALUE == TYPE_1)) {                                                 \
			ERROR(ERR_FEATURE_NOT_IMPLEMENTED, "comparison between literal parameters, e.g. $1 = $2");                \
			RESULT = 1;                                                                                               \
		} else {                                                                                                          \
			TYPE_1 = TYPE_2;                                                                                          \
			if (PARSE_CONTEXT->cur_param_num) {                                                                       \
				/* A placeholder parameter was seen in the query. Now we know what its type should                \
				 * be based on the placement of this parameter in the query (e.g. it is being compared            \
				 * against another operand of a known type etc.). Fill in this parameter's type                   \
				 * so it can be later used in response to a "Describe" message in "handle_describe.c"             \
				 * (in a call to "make_parameter_description.c").                                                 \
				 */                                                                                               \
				if (PARSE_CONTEXT->cur_param_num >= PARSE_CONTEXT->types_size) {                                  \
					if (PARSE_CONTEXT->cur_param_num > (2 * PARSE_CONTEXT->types_size)) {                     \
						/* Sync types to cur_param_num */                                                 \
						EXPAND_ARRAY_ALLOCATION(PARSE_CONTEXT->types, PARSE_CONTEXT->types_size,          \
									PARSE_CONTEXT->cur_param_num, PSQL_TypeOid);              \
						TRACE(INFO_MEM_REALLOCATION, "expanded", "PARSE_CONTEXT->types");                 \
					}                                                                                         \
					DOUBLE_ARRAY_ALLOCATION(PARSE_CONTEXT->types, PARSE_CONTEXT->types_size, PSQL_TypeOid,    \
								INT16_MAX);                                                       \
					TRACE(INFO_MEM_REALLOCATION, "doubled", "PARSE_CONTEXT->types");                          \
				}                                                                                                 \
				PARSE_CONTEXT->types[PARSE_CONTEXT->cur_param_num - 1] = get_psql_type_from_sqlvaluetype(TYPE_2); \
			}                                                                                                         \
		}                                                                                                                 \
	}

// Coverts ambiguous SqlValueTypes to determinate types.
// Specifically:
//	1. Uses DDL-specified types for prepared statement parameter types if not specified by client
//	2. Converts INTEGER_LITERALs to NUMERIC_LITERALs, as they are equivalent internally within Octo
#define CAST_AMBIGUOUS_TYPES(TYPE1, TYPE2, RESULT, PARSE_CONTEXT)                 \
	if ((PARAMETER_VALUE == TYPE1) || (NUL_VALUE == TYPE1)) {                 \
		MAP_TYPE_TO_PARAMETER_VALUE(TYPE1, TYPE2, RESULT, PARSE_CONTEXT); \
	} else if ((PARAMETER_VALUE == TYPE2) || (NUL_VALUE == TYPE2)) {          \
		MAP_TYPE_TO_PARAMETER_VALUE(TYPE2, TYPE1, RESULT, PARSE_CONTEXT); \
	} else if ((INTEGER_LITERAL == TYPE1) && (NUMERIC_LITERAL == TYPE2)) {    \
		TYPE1 = TYPE2;                                                    \
	} else if ((INTEGER_LITERAL == TYPE2) && (NUMERIC_LITERAL == TYPE1)) {    \
		TYPE2 = TYPE1;                                                    \
	}

// Compare TYPE1 and TYPE2 and throw ERR_TYPE if not equal
#define CHECK_TYPE_AND_BREAK_ON_MISMATCH(TYPE1, TYPE2, ERR_TYPE, CUR_BRANCH_VALUE, NEXT_BRANCH_VALUE, RESULT)            \
	{                                                                                                                \
		if ((TYPE1) != (TYPE2)) {                                                                                \
			ERROR((ERR_TYPE), get_user_visible_type_string((TYPE1)), get_user_visible_type_string((TYPE2))); \
			yyerror(NULL, NULL, (CUR_BRANCH_VALUE), NULL, NULL, NULL);                                       \
			if (NULL != (NEXT_BRANCH_VALUE))                                                                 \
				yyerror(NULL, NULL, (NEXT_BRANCH_VALUE), NULL, NULL, NULL);                              \
			RESULT = 1;                                                                                      \
			break;                                                                                           \
		}                                                                                                        \
	}

SqlValueType get_sqlvaluetype_from_psql_type(PSQL_TypeOid type) {
	switch (type) {
	case PSQL_TypeOid_int4:
		return INTEGER_LITERAL;
		break;
	case PSQL_TypeOid_numeric:
		return NUMERIC_LITERAL;
		break;
	case PSQL_TypeOid_varchar:
		return STRING_LITERAL;
		break;
	case PSQL_TypeOid_unknown:
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		return UNKNOWN_SqlValueType;
		break;
	}
}

PSQL_TypeOid get_psql_type_from_sqlvaluetype(SqlValueType type) {
	switch (type) {
	case BOOLEAN_VALUE:
		return PSQL_TypeOid_bool;
		break;
	case INTEGER_LITERAL:
		return PSQL_TypeOid_int4;
		break;
	case NUMERIC_LITERAL:
		return PSQL_TypeOid_numeric;
		break;
	case STRING_LITERAL:
		return PSQL_TypeOid_varchar;
		break;
	case PARAMETER_VALUE:
		/* Needed for extended query case where we generate a plan without knowing the type of one or more literal
		 * parameters. Since these are inferred at Bind time rather than at Parse time (when the plan is generated), we
		 * cannot specify a concrete type. However, we also don't want plan generation to fail for this reason, so simply
		 * specify that the type is unknown until it is latter inferred from concrete values.
		 */
		return PSQL_TypeOid_unknown;
		break;
	case NUL_VALUE:
		return PSQL_TypeOid_unknown;
		break;
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		return PSQL_TypeOid_unknown;
		break;
	}
}

// Helper function that is invoked when we have to traverse a "column_list_alias_STATEMENT".
// Caller passes "do_loop" variable set to TRUE  if they want us to traverse the linked list.
//                              and set to FALSE if they want us to traverse only the first element in the linked list.
int populate_data_type_column_list_alias(SqlStatement *v, SqlValueType *type, boolean_t do_loop, ParseContext *parse_context) {
	SqlColumnListAlias *column_list_alias, *cur_column_list_alias;
	SqlValueType	    child_type1;
	int		    result;

	result = 0;
	*type = UNKNOWN_SqlValueType;
	if (NULL != v) {
		// SqlColumnListAlias
		UNPACK_SQL_STATEMENT(column_list_alias, v, column_list_alias);
		cur_column_list_alias = column_list_alias;
		do {
			child_type1 = UNKNOWN_SqlValueType;
			// SqlColumnList
			result |= populate_data_type(cur_column_list_alias->column_list, &child_type1, parse_context);
			if (UNKNOWN_SqlValueType == cur_column_list_alias->type) {
				cur_column_list_alias->type = child_type1;
			} else if (cur_column_list_alias->type != child_type1) {
				// This is currently possible if a column is explicitly typecast
				// Disable the below code until #304 is fixed at which point it is possible
				//	this code can be re-enabled.
				// assert(FALSE);
				// result |= 1;
			}
			*type = child_type1;
			cur_column_list_alias = cur_column_list_alias->next;
		} while (do_loop && (cur_column_list_alias != column_list_alias));
	}
	return result;
}

// Helper function that is invoked when we have to traverse a "column_list_STATEMENT".
// Caller passes "do_loop" variable set to TRUE  if they want us to traverse the linked list.
//                              and set to FALSE if they want us to traverse only the first element in the linked list.
// If `do_loop` is set and `callback` is not NULL, it will be called for each type in the list.
typedef int (*DataTypeCallback)(SqlValueType *existing, SqlValueType *new, SqlStatement *stmt, ParseContext *parse_context);
int populate_data_type_column_list(SqlStatement *v, SqlValueType *type, boolean_t do_loop, DataTypeCallback callback,
				   ParseContext *parse_context) {
	SqlColumnList *column_list, *cur_column_list;
	SqlValueType   current_type;
	int	       result;

	result = 0;
	*type = UNKNOWN_SqlValueType;
	if (NULL != v) {
		// SqlColumnList
		UNPACK_SQL_STATEMENT(column_list, v, column_list);
		cur_column_list = column_list;
		do {
			// SqlValue or SqlColumnAlias
			current_type = UNKNOWN_SqlValueType;
			result |= populate_data_type(cur_column_list->value, &current_type, parse_context);
			if ((NULL != callback) && (UNKNOWN_SqlValueType != *type)) {
				result |= callback(type, &current_type, cur_column_list->value, parse_context);
			}
			cur_column_list = cur_column_list->next;
			*type = current_type;
		} while (do_loop && (cur_column_list != column_list));
	}
	return result;
}

// NOTE: might also perform type promotion on both types inside the CAST_AMBIGUOUS_TYPES macro.
int ensure_same_type(SqlValueType *existing, SqlValueType *new, SqlStatement *stmt, ParseContext *parse_context) {
	int result;

	result = 0;
	CAST_AMBIGUOUS_TYPES(*existing, *new, result, parse_context);
	if (*existing != *new) {
		ERROR(ERR_TYPE_MISMATCH, get_user_visible_type_string(*existing), get_user_visible_type_string(*new));
		yyerror(NULL, NULL, &stmt, NULL, parse_context, NULL);
		result = 1;
	}
	return result;
}

int populate_data_type(SqlStatement *v, SqlValueType *type, ParseContext *parse_context) {
	SqlBinaryOperation *	binary = NULL;
	SqlCaseBranchStatement *cas_branch, *cur_branch;
	SqlCaseStatement *	cas;
	SqlColumn *		column, *start_column;
	SqlColumnList *		cur_column_list, *start_column_list;
	SqlFunctionCall *	function_call;
	SqlFunction *		function;
	SqlCoalesceCall *	coalesce_call;
	SqlArray *		array;
	SqlGreatest *		greatest_call;
	SqlLeast *		least_call;
	SqlNullIf *		null_if;
	SqlAggregateFunction *	aggregate_function;
	SqlSetOperation *	set_operation;
	SqlTableAlias *		table_alias;
	SqlUnaryOperation *	unary = NULL;
	SqlValue *		value = NULL;
	SqlValueType		child_type1, child_type2;
	SqlSelectStatement *	select;
	int			written, result, function_parm_types_len = 0, status = 0, function_hash_len = MAX_ROUTINE_LEN;
	char *			c, function_hash[MAX_ROUTINE_LEN], function_parm_types[MAX_FUNC_TYPES_LEN];
	hash128_state_t		state;
	SqlDataType		data_type;
	SqlTableValue *		table_value;
	SqlRowValue *		row_value, *start_row_value;
	int			num_columns;
	SqlValueType *		type_array;
	int			colno;
	SqlInsertStatement *	insert;

	result = 0;
	if (v == NULL || v->v.select == NULL)
		return 0;
	// Note: The below switch statement and the flow mirrors that in hash_canonical_query.c.
	//       Any change here or there needs to also be done in the other module.
	switch (v->type) {
	case cas_STATEMENT:
		*type = UNKNOWN_SqlValueType;
		UNPACK_SQL_STATEMENT(cas, v, cas);
		// We expect type to get overriden here; only the last type matters
		result |= populate_data_type(cas->value, type, parse_context);
		// Pass CASE value type to the next call of populate_data_type to compare
		// CASE value and WHEN condition result type.
		child_type1 = *type;
		result |= populate_data_type(cas->branches, &child_type1, parse_context);
		if (NULL != cas->optional_else) { // No need to validate types if ELSE not present
			result |= populate_data_type(cas->optional_else, &child_type2, parse_context);
			// SQL NULL values are acceptable in CASE branches so CAST them appropriately
			CAST_AMBIGUOUS_TYPES(child_type1, child_type2, result, parse_context);
			CHECK_TYPE_AND_BREAK_ON_MISMATCH(child_type1, child_type2, ERR_CASE_BRANCH_TYPE_MISMATCH,
							 &cas->branches->v.cas_branch->value, &cas->optional_else, result);
		}
		*type = child_type1;
		break;
	case cas_branch_STATEMENT:
		// CASE value type is stored in *type
		// UNKNOWN_SqlValueType is a possible type for CASE value as it is optional in case_STATEMENTS.
		// In such a case consider it to be equal to BOOLEAN_VALUE.
		if (UNKNOWN_SqlValueType == *type)
			*type = BOOLEAN_VALUE;
		UNPACK_SQL_STATEMENT(cas_branch, v, cas_branch);
		cur_branch = cas_branch;
		result |= populate_data_type(cur_branch->value, &child_type1, parse_context);
		do {
			result |= populate_data_type(cur_branch->condition, &child_type2, parse_context);
			assert(UNKNOWN_SqlValueType != child_type2);
			// SQL NULL values are acceptable for CASE value and WHEN condition type so CAST them appropriately
			CAST_AMBIGUOUS_TYPES(*type, child_type2, result, parse_context);
			CHECK_TYPE_AND_BREAK_ON_MISMATCH(child_type2, *type, ERR_CASE_VALUE_TYPE_MISMATCH, &cur_branch->condition,
							 NULL, result);
			if (cas_branch != cur_branch->next) {
				result |= populate_data_type(cur_branch->next->value, &child_type2, parse_context);
				// SQL NULL values are acceptable in CASE branches so CAST them appropriately
				CAST_AMBIGUOUS_TYPES(child_type1, child_type2, result, parse_context);
				CHECK_TYPE_AND_BREAK_ON_MISMATCH(child_type1, child_type2, ERR_CASE_BRANCH_TYPE_MISMATCH,
								 &cur_branch->value, &cur_branch->next->value, result);
				child_type1 = child_type2;
			}
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		*type = child_type1;
		break;
	case insert_STATEMENT:
		UNPACK_SQL_STATEMENT(insert, v, insert);
		result |= populate_data_type(insert->src_table_alias_stmt, &child_type1, parse_context);
		result |= check_column_lists_for_type_match(v);
		break;
	case delete_from_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlDeleteFromStatement *delete;

		UNPACK_SQL_STATEMENT(delete, v, delete_from);
		result |= populate_data_type(delete->src_join, type, parse_context);
		if (NULL != delete->where_clause) {
			result |= populate_data_type(delete->where_clause, &child_type1, parse_context);
			if (!result && (BOOLEAN_VALUE != child_type1) && (NUL_VALUE != child_type1)) {
				ISSUE_TYPE_COMPATIBILITY_ERROR(child_type1, "boolean operations", &delete->where_clause, result);
			}
		}
		break;
	case select_STATEMENT:
		UNPACK_SQL_STATEMENT(select, v, select);
		// SqlJoin
		result |= populate_data_type(select->table_list, &child_type1, parse_context);
		// SqlColumnListAlias that is a linked list
		result |= populate_data_type_column_list_alias(select->select_list, &child_type1, TRUE, parse_context);
		*type = child_type1;
		// SqlValue (?)
		if (NULL != select->where_expression) {
			result |= populate_data_type(select->where_expression, &child_type1, parse_context);
			if (!result && (BOOLEAN_VALUE != child_type1) && (NUL_VALUE != child_type1)) {
				ISSUE_TYPE_COMPATIBILITY_ERROR(child_type1, "boolean operations", &select->where_expression,
							       result);
			}
		}
		// SqlColumnListAlias that is a linked list
		result |= populate_data_type_column_list_alias(select->group_by_expression, &child_type1, TRUE, parse_context);
		// SqlValue (?)
		if (NULL != select->having_expression) {
			result |= populate_data_type(select->having_expression, &child_type1, parse_context);
			if (!result && (BOOLEAN_VALUE != child_type1) && (NUL_VALUE != child_type1)) {
				ISSUE_TYPE_COMPATIBILITY_ERROR(child_type1, "boolean operations", &select->having_expression,
							       result);
			}
		}
		// SqlColumnListAlias that is a linked list
		result |= populate_data_type_column_list_alias(select->order_by_expression, &child_type1, TRUE, parse_context);
		/* While we are anyways descending down the query and subqueries (if any), take this opportunity to do some
		 * parse tree optimization if possible. Optimize subqueries first before doing the parent query. Hence the
		 * placement of this at the very end of the "case" block.
		 */
		parse_tree_optimize(select);
		break;
	case function_call_STATEMENT:
		UNPACK_SQL_STATEMENT(function_call, v, function_call);
		// SqlColumnList
		result |= populate_data_type_column_list(function_call->parameters, type, TRUE, NULL, parse_context);
		if (0 != result) {
			break;
		}

		/* Hash the function name and parameters to determine which version of the given function is desired. This behavior
		 * replaces the previous, explicit parameter type and number checks as the hash lookup process does this implicitly.
		 *
		 * Note that this hashing code mirrors that for create_function_STATEMENTs in hash_canonical_query.c,
		 * so any changes there will also need to be reflected here.
		 */
		HASH128_STATE_INIT(state, 0);
		assert(value_STATEMENT == function_call->function_name->type);
		ADD_INT_HASH(&state, function_call->function_name->type);
		value = function_call->function_name->v.value;
		ADD_INT_HASH(&state, value->type);
		ydb_mmrhash_128_ingest(&state, (void *)value->v.reference, strlen(value->v.reference));
		c = function_parm_types;
		// Identify the type of each function parameter and add it to the hash
		cur_column_list = start_column_list = function_call->parameters->v.column_list;
		if (NULL != cur_column_list->value) { // Only iterate over parameters if there are any
			do {
				result |= populate_data_type(cur_column_list->value, &child_type1, parse_context);
				if (value_STATEMENT == cur_column_list->value->type) {
					*type = child_type1;
				} else if (column_alias_STATEMENT == cur_column_list->value->type) {
					SqlStatement *column_stmt;

					column_stmt = cur_column_list->value->v.column_alias->column;
					assert((column_STATEMENT == column_stmt->type)
					       || (column_list_alias_STATEMENT == column_stmt->type));
					if (column_STATEMENT == column_stmt->type) {
						data_type = column_stmt->v.column->data_type_struct.data_type;
						*type = get_sqlvaluetype_from_sqldatatype(data_type, FALSE);
					} else {
						*type = column_stmt->v.column_list_alias->type;
					}
				} else {
					*type = child_type1;
				}
				ADD_INT_HASH(&state, *type);
				written = snprintf(c, MAX_FUNC_TYPES_LEN - function_parm_types_len, "%s",
						   get_user_visible_type_string(*type));
				if ((MAX_FUNC_TYPES_LEN - function_parm_types_len) <= written) {
					ERROR(ERR_BUFFER_TOO_SMALL, "Function parameter type");
					result = 1;
					break;
				} else {
					c += written;
					function_parm_types_len += written;
				}
				cur_column_list = cur_column_list->next;
				if (cur_column_list != start_column_list) {
					written = snprintf(c, MAX_FUNC_TYPES_LEN - function_parm_types_len, ", ");
					if ((MAX_FUNC_TYPES_LEN - function_parm_types_len) <= written) {
						ERROR(ERR_BUFFER_TOO_SMALL, "Function parameter type");
						result = 1;
						break;
					} else {
						c += written;
					}
				}
			} while (cur_column_list != start_column_list);
		} else {
			snprintf(c, MAX_FUNC_TYPES_LEN, OCTOLIT_NONE);
		}
		status = generate_routine_name(&state, function_hash, function_hash_len, FunctionHash);
		if (1 == status) {
			result = 1;
			break;
		}
		// Retrieve the DDL for the given function
		SQL_STATEMENT(function_call->function_schema, create_function_STATEMENT);
		/* Note that find_table() is called from the parser as every table name in the query is encountered but
		 * find_function() is not called whenever a function name is encountered in the parser. It is instead called
		 * much later from populate_data_type() after the entire query has been parsed and qualified. The reason for this is
		 * that find_function() needs type information of the actual function parameters to determine which one of the
		 * potentially many function definitions matches the current usage. And this type information is not available until
		 * all column references in the query are qualified (which only happens in qualify_query() after the entire query
		 * has been parsed and just before populate_data_type() is called).
		 */
		function = function_call->function_schema->v.create_function
		    = find_function(function_call->function_name->v.value->v.string_literal, function_hash);
		// Issue syntax error and abort if function doesn't exist. UNKNOWN_FUNCTION error will be issued by find_function.
		if (NULL == function) {
			ERROR(ERR_UNKNOWN_FUNCTION, function_call->function_name->v.value->v.string_literal, function_parm_types);
			yyerror(&(function_call->function_name->loc), NULL, NULL, NULL, NULL, NULL);
			result = 1;
			break;
		}
		data_type = function->return_type->v.data_type_struct.data_type;
		*type = get_sqlvaluetype_from_sqldatatype(data_type, FALSE);
		break;
	case array_STATEMENT:
		UNPACK_SQL_STATEMENT(array, v, array);
		result |= populate_data_type(array->argument, type, parse_context);
		break;
	case coalesce_STATEMENT:
		UNPACK_SQL_STATEMENT(coalesce_call, v, coalesce);
		// SqlColumnList
		result |= populate_data_type_column_list(coalesce_call->arguments, type, TRUE, ensure_same_type, parse_context);
		// NOTE: if the types of the parameters do not match, no error is issued.
		// This matches the behavior of sqlite but not that of Postgres and Oracle DB.
		break;
	case greatest_STATEMENT:
		UNPACK_SQL_STATEMENT(greatest_call, v, greatest);
		// SqlColumnList
		result |= populate_data_type_column_list(greatest_call->arguments, type, TRUE, ensure_same_type, parse_context);
		break;
	case least_STATEMENT:
		UNPACK_SQL_STATEMENT(least_call, v, least);
		// SqlColumnList
		result |= populate_data_type_column_list(least_call->arguments, type, TRUE, ensure_same_type, parse_context);
		break;
	case null_if_STATEMENT: {
		SqlValueType tmp;

		UNPACK_SQL_STATEMENT(null_if, v, null_if);
		// SqlColumnList
		result |= populate_data_type(null_if->left, type, parse_context);
		result |= populate_data_type(null_if->right, &tmp, parse_context);
		ensure_same_type(type, &tmp, null_if->right, parse_context);
		break;
	}
	case aggregate_function_STATEMENT:
		UNPACK_SQL_STATEMENT(aggregate_function, v, aggregate_function);
		// SqlColumnList : table.* usage will have more than one node so loop through
		result |= populate_data_type_column_list(aggregate_function->parameter, type, TRUE, NULL, parse_context);
		// Note that COUNT(...) is always an INTEGER type even though ... might be a string type column.
		switch (aggregate_function->type) {
		case COUNT_AGGREGATE_DISTINCT:
			assert(TABLE_ASTERISK != (*type));
			/* The above assert is valid as count(DISTINCT table.*) value would have been expanded at
			 * qualify_statement() aggregate_function_STATEMENT case to column_list of column_alias values
			 * by "process_table_asterisk_cl()" call. And this is why we are also guaranteed that "*type"
			 * would be initialized in the "populate_data_type_column_list()" call above as there is at least
			 * one column in the list that would have been processed and "*type" would be initialized to the
			 * type of the last column in that list. For example, in the "COUNT_ASTERISK_AGGREGATE" case below,
			 * we are not guaranteed "*type" is initialized and hence cannot have a similar assert.
			 */
			*type = INTEGER_LITERAL;
			break;
		case COUNT_ASTERISK_AGGREGATE:
		case COUNT_AGGREGATE:
			*type = INTEGER_LITERAL;
			break;
		case AVG_AGGREGATE:
		case AVG_AGGREGATE_DISTINCT:
		case SUM_AGGREGATE:
		case SUM_AGGREGATE_DISTINCT:
			if ((TABLE_ASTERISK == *type) || (STRING_LITERAL == *type) || (BOOLEAN_VALUE == *type)) {
				/* TABLE_ASTERISK or STRING or BOOLEAN type cannot be input for the AVG or SUM function so signal
				 * an error in that case.
				 */
				if (TABLE_ASTERISK == *type) {
					ERROR(ERR_MISTYPED_FUNCTION_TABLE_ASTERISK,
					      get_aggregate_func_name(aggregate_function->type),
					      get_user_visible_type_string(*type));
				} else {
					ERROR(ERR_MISTYPED_FUNCTION, get_aggregate_func_name(aggregate_function->type),
					      get_user_visible_type_string(*type));
				}
				yyerror(NULL, NULL, &aggregate_function->parameter, NULL, NULL, NULL);
				result = 1;
			}
			break;
		case MIN_AGGREGATE:
		case MAX_AGGREGATE:
			if ((TABLE_ASTERISK == *type) || (BOOLEAN_VALUE == *type)) {
				/* TABLE_ASTERISK or BOOLEAN type cannot be input for the MIN or MAX function so signal an error in
				 * that case. */
				ERROR(ERR_MISTYPED_FUNCTION, get_aggregate_func_name(aggregate_function->type),
				      get_user_visible_type_string(*type));
				yyerror(NULL, NULL, &aggregate_function->parameter, NULL, NULL, NULL);
				result = 1;
				break;
			}
			/* In this case, *type should be the same as the type of "aggregate_function->parameter"
			 * which is already set above so copy that over for later use in physical plan.
			 */
			aggregate_function->param_type = *type;
			break;
		default:
			assert(FALSE);
			break;
		}
		break;
	case join_STATEMENT:; /* semicolon for empty statement so we can declare variables in case block */
		SqlJoin *start_join, *cur_join;

		UNPACK_SQL_STATEMENT(start_join, v, join);
		cur_join = start_join;
		do {
			result |= populate_data_type(cur_join->value, type, parse_context);
			if (NULL != cur_join->condition) {
				result |= populate_data_type(cur_join->condition, type, parse_context);
				if (!result && (BOOLEAN_VALUE != *type) && (NUL_VALUE != *type)) {
					ISSUE_TYPE_COMPATIBILITY_ERROR(*type, "boolean operations", &cur_join->condition, result);
				}
			}
			cur_join = cur_join->next;
		} while (cur_join != start_join);
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, v, value);
		switch (value->type) {
		case CALCULATED_VALUE:
			result |= populate_data_type(value->v.calculated, &child_type1, parse_context);
			*type = child_type1;
			break;
		case BOOLEAN_VALUE:
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case STRING_LITERAL:
		case NUL_VALUE:
		case FUNCTION_NAME:
		case UNKNOWN_SqlValueType: // Note: This is a possibility in "populate_data_type" but not in "hash_canonical_query"
			/* If lexer determined that the value is an integer literal, use that more specific INTEGER_LITERAL type
			 * instead of a more generic NUMERIC_LITERAL type.
			 */
			*type = value->type;
			break;
		case PARAMETER_VALUE: { // Note: This is a possibility in "populate_data_type" but not in "hash_canonical_query"
			long tmp_long;
			int  param_num;

			/* Set the type to "PARAMETER_VALUE" by default. Check if this is an extended query and if query
			 * parameter types have been specified in the "Parse" message. If so use that type instead.
			 */
			*type = PARAMETER_VALUE;
			if (parse_context->is_extended_query) {
				tmp_long = strtol(value->v.string_literal + 1, NULL, 10); /* + 1 to skip '$' prefix in '$1' etc. */
				if ((LONG_MIN != tmp_long) && (LONG_MAX != tmp_long)) {
					param_num = (int)tmp_long;
					parse_context->cur_param_num = param_num;
					if ((0 < param_num) && (param_num <= parse_context->num_bind_parm_types)) {
						/* Note down parameter number corresponding to this "SqlValue" structure
						 * for later use inside MAP_TYPE_TO_PARAMETER_VALUE macro.
						 */
						*type = get_sqlvaluetype_from_psql_type(parse_context->types[param_num - 1]);
					}
				} else {
					ERROR(ERR_LIBCALL, "strtol");
					result = 1;
				}
			}
			break;
		}
		case TABLE_ASTERISK:
			/* We should not reach this case as TABLE_ASTERISK can only occur in aggregate function count usage
			 * and aggregate function has TABLE_ASTERISK enclosed in COLUMN_ALIAS, and COLUMN_ALIAS case itself
			 * takes care of returning the type.
			 */
			assert(FALSE);
			result = 1;
			break;
		case COLUMN_REFERENCE:
			/* If this happens it probably means it wasn't an extended reference
			 * which is not something we want to happen, the parser should expand
			 * all column references to be fully qualified
			 */
			assert(FALSE);
			result = 1;
			break;
		case COERCE_TYPE:
			result |= populate_data_type(value->v.coerce_target, &child_type1, parse_context);
			/* Note down type of target before coerce */
			value->pre_coerced_type = child_type1;
			/* At this time (Jan 2020), we allow any type to be coerced to any other type at parser time.
			 * Errors in type conversion, if any, should show up at run-time based on the actual values.
			 * But since our run-time is M and we currently only allow INTEGER/NUMERIC/STRING types,
			 * M will allow for converting between either of these types without any errors which is
			 * different from Postgres (where `'Zero'::integer` will cause an error). We will deal with
			 * this if users complain about this incompatibility with Postgres.
			 */
			*type = get_sqlvaluetype_from_sqldatatype(value->coerced_type.data_type, FALSE);
			break;
		default:
			assert(FALSE);
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			break;
		}
		break;
	case column_alias_STATEMENT:
		if (is_stmt_table_asterisk(v->v.column_alias->column)) {
			/* When table.* is used with aggregate function other than COUNT
			 * we can come across this case. Retain type for aggregate_function_STATEMENT case
			 * to throw appropriate error.
			 */
			*type = TABLE_ASTERISK;
		} else if (column_list_alias_STATEMENT == v->v.column_alias->column->type) {
			result |= populate_data_type(v->v.column_alias->column, type, parse_context);
		} else {
			UNPACK_SQL_STATEMENT(column, v->v.column_alias->column, column);
			*type = get_sqlvaluetype_from_sqldatatype(column->data_type_struct.data_type, FALSE);
		}
		break;
	case column_list_STATEMENT:
		// Note: We do not loop through the list (just like is done in "hash_canonical_query")
		result |= populate_data_type_column_list(v, type, FALSE, NULL, parse_context);
		break;
	case column_list_alias_STATEMENT:
		// Note: We do not loop through the list (just like is done in "hash_canonical_query")
		result |= populate_data_type_column_list_alias(v, type, FALSE, parse_context);
		break;
	case create_table_STATEMENT:
		// Do nothing; we got here through a table_alias
		break;
	case table_value_STATEMENT:
		/* For a table constructed using the VALUES clause, go through each value specified and determine
		 * its type. Verify all rows have same type for each column.
		 */
		UNPACK_SQL_STATEMENT(table_value, v, table_value);
		UNPACK_SQL_STATEMENT(row_value, table_value->row_value_stmt, row_value);
		start_row_value = row_value;
		num_columns = row_value->num_columns;
		assert(num_columns);
		type_array = (SqlValueType *)calloc(num_columns, sizeof(SqlValueType));
		do {
			SqlColumnList *start_column_list, *cur_column_list;

			UNPACK_SQL_STATEMENT(start_column_list, row_value->value_list, column_list);
			cur_column_list = start_column_list;
			colno = 0;
			do {
				SqlValueType current_type;

				// SqlValue or SqlColumnAlias
				result |= populate_data_type(cur_column_list->value, &current_type, parse_context);
				if (start_row_value == row_value) {
					/* This is the first row. We don't have any type to compare against.
					 * Just record this type for now.
					 */
					assert(UNKNOWN_SqlValueType == type_array[colno]);
					assert(UNKNOWN_SqlValueType != current_type);
					type_array[colno] = current_type;
				} else {
					/* Compare type determined for this row against noted down type from previous row */
					assert(UNKNOWN_SqlValueType != type_array[colno]);
					result |= ensure_same_type(&type_array[colno], &current_type, cur_column_list->value,
								   parse_context);
				}
				colno++;
				cur_column_list = cur_column_list->next;
			} while ((cur_column_list != start_column_list));
			assert(colno == num_columns); /* every row should have same number of columns as first row */
			row_value = row_value->next;
		} while (row_value != start_row_value);
		/* Now that we have scanned all rows of the VALUES table, store the final determined column type.
		 * Note that if one row had a column type of NUL_VALUE and the next row had a type of NUMERIC_LITERAL,
		 * the final column type would end up being NUMERIC_LITERAL.
		 */
		start_column = table_value->column;
		column = start_column;
		colno = 0;
		do {
			assert(UNKNOWN_SqlDataType == column->data_type_struct.data_type);
			column->data_type_struct.data_type = get_sqldatatype_from_sqlvaluetype(type_array[colno]);
			column->data_type_struct.size_or_precision = SIZE_OR_PRECISION_UNSPECIFIED;
			column->data_type_struct.scale = SCALE_UNSPECIFIED;
			column->data_type_struct.size_or_precision_parameter_index = 0;
			column->data_type_struct.scale_parameter_index = 0;
			column = column->next;
			colno++;
		} while (column != start_column);
		assert(colno == num_columns);
		*type = type_array[0]; /* Return the type of the first column in the VALUES clause */
		free(type_array);
		break;
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, v, table_alias);
		result |= populate_data_type(table_alias->table, type, parse_context);
		assert((select_STATEMENT != table_alias->table->type)
		       || (table_alias->table->v.select->select_list == table_alias->column_list));
		if (select_STATEMENT != table_alias->table->type) {
			/* Note: In case "table_alias->table" is of type "table_value_STATEMENT", the above call would have
			 * determined the type of the "SqlColumn" structures making up that table based on the actual values
			 * data supplied. That will then need to be propagated to the associated "SqlColumnListAlias" structures
			 * in the below call.
			 */
			result |= populate_data_type_column_list_alias(table_alias->column_list, &child_type1, TRUE, parse_context);
		}
		break;
	case binary_STATEMENT:
		UNPACK_SQL_STATEMENT(binary, v, binary);
		result |= populate_data_type(binary->operands[0], &child_type1, parse_context);
		if (((BOOLEAN_IN == binary->operation) || (BOOLEAN_NOT_IN == binary->operation))
		    && (column_list_STATEMENT == binary->operands[1]->type)) { // SqlColumnList
			result |= populate_data_type_column_list(binary->operands[1], &child_type2, TRUE, NULL, parse_context);
		} else {
			// SqlStatement (?)
			result |= populate_data_type(binary->operands[1], &child_type2, parse_context);
		}
		CAST_AMBIGUOUS_TYPES(child_type1, child_type2, result, parse_context);
		switch (binary->operation) {
		case ADDITION:
		case SUBTRACTION:
		case DIVISION:
		case MULTIPLICATION:
		case MODULO:
			if (!result) {
				int	     i;
				SqlValueType child_type;

				for (i = 0; i < 2; i++) {
					child_type = ((0 == i) ? child_type1 : child_type2);
					switch (child_type) {
					case INTEGER_LITERAL:
					case NUMERIC_LITERAL:
					case NUL_VALUE:
						/* These types are acceptable for arithmetic operations */
						break;
					default:
						ISSUE_TYPE_COMPATIBILITY_ERROR(child_type, "arithmetic operations",
									       &binary->operands[i], result);
					}
				}
			}
			*type = child_type1;
			break;
		case CONCAT:
			/* Postgres allows || operator as long as at least one operand is STRING type or both operands are NULLs.
			 * Otherwise it issues an error. Do the same in Octo for compatibility.
			 */
			if ((STRING_LITERAL != child_type1) && (NUL_VALUE != child_type1) && (STRING_LITERAL != child_type2)
			    && (NUL_VALUE != child_type2)) {
				if (!result) {
					int	     i;
					SqlValueType child_type;

					for (i = 0; i < 2; i++) {
						child_type = ((0 == i) ? child_type1 : child_type2);
						ISSUE_TYPE_COMPATIBILITY_ERROR(child_type, "|| operator", &binary->operands[i],
									       result);
					}
				}
			} else {
				SqlStatement **target;
				SqlValueType   child_type;

				/* If one operand is BOOLEAN_VALUE, add type cast operator (::string) to it.
				 * Not needed for INTEGER_LITERAL or NUMERIC_LITERAL as M handles this fine.
				 */
				if (BOOLEAN_VALUE == child_type1) {
					assert(BOOLEAN_VALUE != child_type2);
					child_type = child_type1;
					target = &binary->operands[0];
				} else if (BOOLEAN_VALUE == child_type2) {
					assert(BOOLEAN_VALUE != child_type1);
					child_type = child_type2;
					target = &binary->operands[1];
				} else {
					target = NULL;
				}
				if (NULL != target) {
					SqlStatement *sql_stmt;

					SQL_STATEMENT(sql_stmt, value_STATEMENT);
					MALLOC_STATEMENT(sql_stmt, value, SqlValue);
					UNPACK_SQL_STATEMENT(value, sql_stmt, value);
					value->type = COERCE_TYPE;
					value->coerced_type.data_type = STRING_TYPE;
					value->coerced_type.size_or_precision = SIZE_OR_PRECISION_UNSPECIFIED;
					value->coerced_type.scale = SCALE_UNSPECIFIED;
					value->coerced_type.size_or_precision_parameter_index = 0;
					value->coerced_type.scale_parameter_index = 0;
					value->pre_coerced_type = child_type;
					value->v.coerce_target = *target;
					*target = sql_stmt;
				}
			}
			child_type1 = child_type2 = *type = STRING_LITERAL;
			break;
		case BOOLEAN_OR:
		case BOOLEAN_AND:
			if (!result) {
				int	     i;
				SqlValueType child_type;

				for (i = 0; i < 2; i++) {
					child_type = ((0 == i) ? child_type1 : child_type2);
					if ((BOOLEAN_VALUE != child_type) && (NUL_VALUE != child_type)) {
						ISSUE_TYPE_COMPATIBILITY_ERROR(child_type, "boolean operations",
									       &binary->operands[i], result);
					}
				}
			}
			*type = BOOLEAN_VALUE;
			break;
		default:
			*type = BOOLEAN_VALUE;
			break;
		}
		if (!result && (child_type1 != child_type2)) {
			int i;

			ERROR(ERR_TYPE_MISMATCH, get_user_visible_type_string(child_type1),
			      get_user_visible_type_string(child_type2));
			for (i = 0; i < 2; i++) {
				yyerror(NULL, NULL, &binary->operands[i], NULL, NULL, NULL);
			}
			result = 1;
		}
		break;
	case set_operation_STATEMENT:
		UNPACK_SQL_STATEMENT(set_operation, v, set_operation);
		result |= populate_data_type(set_operation->operand[0], &child_type1, parse_context);
		*type = child_type1;
		result |= populate_data_type(set_operation->operand[1], &child_type2, parse_context);
		/* Now that the types of operands to the SET operation have been populated, do some more checks of
		 * whether the # and types of columns on both operands match. If not issue error.
		 */
		result |= check_column_lists_for_type_match(v);
		break;
	case unary_STATEMENT:
		UNPACK_SQL_STATEMENT(unary, v, unary);
		result |= populate_data_type(unary->operand, &child_type1, parse_context);
		/* Check for type mismatches */
		if (((FORCE_NUM == unary->operation) || (NEGATIVE == unary->operation))
		    && ((INTEGER_LITERAL != child_type1) && (NUMERIC_LITERAL != child_type1))) {
			/* Unary + and - operators cannot be used on non-numeric or non-integer types */
			ERROR(ERR_INVALID_INPUT_SYNTAX, get_user_visible_type_string(child_type1));
			yyerror(NULL, NULL, &unary->operand, NULL, NULL, NULL);
			result = 1;
			break;
		}
		/* If the unary operation is EXISTS or IS NULL, then set the type of the result to BOOLEAN,
		 * not to the type inherited from the sub-query passed to EXISTS.
		 */
		assert(BOOLEAN_NOT_EXISTS != unary->operation);
		switch (unary->operation) {
		case BOOLEAN_EXISTS:
		case BOOLEAN_IS_NULL:
		case BOOLEAN_IS_NOT_NULL:
			*type = BOOLEAN_VALUE;
			break;
		default:
			*type = child_type1;
			break;
		}
		assert((BOOLEAN_NOT != unary->operation) || (NUL_VALUE == *type) || IS_LITERAL_PARAMETER(*type));
		break;
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		result = 1;
		break;
	}
	return result;
}
