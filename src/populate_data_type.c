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

#define MAP_TYPE_TO_PARAMETER_VALUE(TYPE_1, TYPE_2)                                                                               \
	{                                                                                                                         \
		/* Map the column's type to the prepared statement parameter for ParameterDescriptions and Bind handling */       \
		if (parse_context->is_extended_query && (PARAMETER_VALUE == TYPE_1)) {                                            \
			if (NULL != parse_context->types) {                                                                       \
				if (parse_context->cur_type < parse_context->num_bind_parm_types) {                               \
					/* If type specified in Parse message, use that */                                        \
					TYPE_1 = get_sqlvaluetype_from_psql_type(parse_context->types[parse_context->cur_type]);  \
					parse_context->cur_type++;                                                                \
				} else {                                                                                          \
					/* If not, use the one assigned by the parser from the DDL */                             \
					if ((PARAMETER_VALUE == TYPE_2) || (NUL_VALUE == TYPE_2)) {                               \
						ERROR(ERR_FEATURE_NOT_IMPLEMENTED,                                                \
						      "comparison between literal parameters, e.g. $1 = $2");                     \
						result = 1;                                                                       \
					} else {                                                                                  \
						if (parse_context->cur_type >= parse_context->types_size) {                       \
							if (parse_context->cur_type > (2 * parse_context->types_size)) {          \
								/* Sync types to cur_type */                                      \
								EXPAND_ARRAY_ALLOCATION(parse_context->types,                     \
											parse_context->types_size,                \
											parse_context->cur_type, PSQL_TypeOid);   \
								TRACE(INFO_MEM_REALLOCATION, "expanded", "parse_context->types"); \
							}                                                                         \
							DOUBLE_ARRAY_ALLOCATION(parse_context->types, parse_context->types_size,  \
										PSQL_TypeOid);                                    \
							TRACE(INFO_MEM_REALLOCATION, "doubled", "parse_context->types");          \
						}                                                                                 \
						parse_context->types[parse_context->cur_type]                                     \
						    = get_psql_type_from_sqlvaluetype(TYPE_2);                                    \
						parse_context->cur_type++;                                                        \
						TYPE_1 = TYPE_2;                                                                  \
					}                                                                                         \
				}                                                                                                 \
			}                                                                                                         \
		} else {                                                                                                          \
			TYPE_1 = TYPE_2;                                                                                          \
		}                                                                                                                 \
	}

// Coverts ambiguous SqlValueTypes to determinate types.
// Specifically:
//	1. Uses DDL-specified types for prepared statement parameter types if not specified by client
//	2. Converts INTEGER_LITERALs to NUMERIC_LITERALs, as they are equivalent internally within Octo
#define CAST_AMBIGUOUS_TYPES(TYPE1, TYPE2)                                     \
	if ((PARAMETER_VALUE == TYPE1) || (NUL_VALUE == TYPE1)) {              \
		MAP_TYPE_TO_PARAMETER_VALUE(TYPE1, TYPE2);                     \
	} else if ((PARAMETER_VALUE == TYPE2) || (NUL_VALUE == TYPE2)) {       \
		MAP_TYPE_TO_PARAMETER_VALUE(TYPE2, TYPE1);                     \
	} else if ((INTEGER_LITERAL == TYPE1) && (NUMERIC_LITERAL == TYPE2)) { \
		TYPE1 = TYPE2;                                                 \
	} else if ((INTEGER_LITERAL == TYPE2) && (NUMERIC_LITERAL == TYPE1)) { \
		TYPE2 = TYPE1;                                                 \
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
	int		    result = 0;

	*type = UNKNOWN_SqlValueType;
	if ((NULL != v) && (NULL != v->v.select)) {
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
	int	       result = 0;

	*type = UNKNOWN_SqlValueType;
	if ((NULL != v) && (NULL != v->v.select)) {
		// SqlColumnList
		UNPACK_SQL_STATEMENT(column_list, v, column_list);
		cur_column_list = column_list;
		do {
			// SqlValue or SqlColumnAlias
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
	int result = 0;
	CAST_AMBIGUOUS_TYPES(*existing, *new);
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
	SqlColumn *		column;
	SqlColumnList *		cur_column_list, *start_column_list;
	SqlFunctionCall *	function_call;
	SqlFunction *		function;
	SqlCoalesceCall *	coalesce_call;
	SqlGreatest *		greatest_call;
	SqlLeast *		least_call;
	SqlNullIf *		null_if;
	SqlAggregateFunction *	aggregate_function;
	SqlJoin *		start_join, *cur_join;
	SqlSetOperation *	set_operation;
	SqlTableAlias *		table_alias;
	SqlUnaryOperation *	unary = NULL;
	SqlValue *		value = NULL;
	SqlValueType		child_type1, child_type2;
	YYLTYPE			location;
	SqlSelectStatement *	select;
	int			written, result = 0, function_parm_types_len = 0, status = 0, function_hash_len = MAX_ROUTINE_LEN;
	char *			c, function_hash[MAX_ROUTINE_LEN], function_parm_types[MAX_FUNC_TYPES_LEN];
	hash128_state_t		state;
	SqlDataType		data_type;

	*type = UNKNOWN_SqlValueType;
	if (v == NULL || v->v.select == NULL)
		return 0;
	// Note: The below switch statement and the flow mirrors that in hash_canonical_query.c.
	//       Any change here or there needs to also be done in the other module.
	switch (v->type) {
	case cas_STATEMENT:
		UNPACK_SQL_STATEMENT(cas, v, cas);
		// We expect type to get overriden here; only the last type matters
		result |= populate_data_type(cas->value, type, parse_context);
		result |= populate_data_type(cas->branches, &child_type1, parse_context);
		if (NULL != cas->optional_else) { // No need to validate types if ELSE not present
			result |= populate_data_type(cas->optional_else, &child_type2, parse_context);
			CAST_AMBIGUOUS_TYPES(child_type1, child_type2);
			// SQL NULL values are acceptable in CASE branches
			if (child_type1 != child_type2) {
				ERROR(ERR_CASE_BRANCH_TYPE_MISMATCH, get_user_visible_type_string(child_type1),
				      get_user_visible_type_string(child_type2));
				yyerror(NULL, NULL, &cas->branches->v.cas_branch->value, NULL, NULL, NULL);
				yyerror(NULL, NULL, &cas->optional_else, NULL, NULL, NULL);
				result = 1;
			}
		}
		*type = child_type1;
		break;
	case cas_branch_STATEMENT:
		UNPACK_SQL_STATEMENT(cas_branch, v, cas_branch);
		cur_branch = cas_branch;
		result |= populate_data_type(cur_branch->value, &child_type1, parse_context);
		do {
			result |= populate_data_type(cur_branch->condition, type, parse_context);
			if (cur_branch != cur_branch->next) {
				result |= populate_data_type(cur_branch->next->value, &child_type2, parse_context);
				// SQL NULL values are acceptable in CASE branches
				CAST_AMBIGUOUS_TYPES(child_type1, child_type2);
				if (child_type1 != child_type2) {
					ERROR(ERR_CASE_BRANCH_TYPE_MISMATCH, get_user_visible_type_string(child_type1),
					      get_user_visible_type_string(child_type2));
					yyerror(NULL, NULL, &cur_branch->value, NULL, NULL, NULL);
					yyerror(NULL, NULL, &cur_branch->next->value, NULL, NULL, NULL);
					result = 1;
					break;
				}
				child_type1 = child_type2;
			}
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		*type = child_type1;
		break;
	case select_STATEMENT:
		UNPACK_SQL_STATEMENT(select, v, select);
		// SqlColumnListAlias that is a linked list
		result |= populate_data_type_column_list_alias(select->select_list, &child_type1, TRUE, parse_context);
		*type = child_type1;
		// SqlJoin
		result |= populate_data_type(select->table_list, &child_type1, parse_context);
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
					value = cur_column_list->value->v.value;
					*type = ((TRUE == value->is_int) ? INTEGER_LITERAL : child_type1);
				} else if (column_alias_STATEMENT == cur_column_list->value->type) {
					SqlStatement *column_stmt;

					column_stmt = cur_column_list->value->v.column_alias->column;
					assert((column_STATEMENT == column_stmt->type)
					       || (column_list_alias_STATEMENT == column_stmt->type));
					if (column_STATEMENT == column_stmt->type) {
						data_type = column_stmt->v.column->data_type_struct.data_type;
						*type = get_sqlvaluetype_from_sqldatatype(data_type);
					} else {
						*type = column_stmt->v.column_list_alias->type;
					}
				} else {
					value = cur_column_list->value->v.unary->operand->v.value;
					*type = ((TRUE == value->is_int) ? INTEGER_LITERAL : child_type1);
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
			snprintf(c, MAX_FUNC_TYPES_LEN, "none");
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
		*type = get_sqlvaluetype_from_sqldatatype(data_type);
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
		// SqlColumnList : We have only one parameter to aggregate functions so no loop needed hence FALSE used below.
		result |= populate_data_type_column_list(aggregate_function->parameter, type, FALSE, NULL, parse_context);
		// Note that COUNT(...) is always an INTEGER type even though ... might be a string type column.
		// Hence the if check below.
		switch (aggregate_function->type) {
		case COUNT_ASTERISK_AGGREGATE:
		case COUNT_AGGREGATE:
		case COUNT_AGGREGATE_DISTINCT:
			*type = INTEGER_LITERAL;
			break;
		case AVG_AGGREGATE:
		case AVG_AGGREGATE_DISTINCT:
		case SUM_AGGREGATE:
		case SUM_AGGREGATE_DISTINCT:
			if ((STRING_LITERAL == *type) || (BOOLEAN_VALUE == *type)) {
				/* STRING or BOOLEAN type cannot be input for the AVG or SUM function so signal
				 * an error in that case.
				 */
				ERROR(ERR_MISTYPED_FUNCTION, get_aggregate_func_name(aggregate_function->type),
				      get_user_visible_type_string(*type));
				yyerror(NULL, NULL, &aggregate_function->parameter, NULL, NULL, NULL);
				result = 1;
			}
			break;
		case MIN_AGGREGATE:
		case MAX_AGGREGATE:
			if (BOOLEAN_VALUE == *type) {
				/* BOOLEAN type cannot be input for the MIN or MAX function so signal an error in that case. */
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
	case join_STATEMENT:
		UNPACK_SQL_STATEMENT(start_join, v, join);
		cur_join = start_join;
		do {
			result |= populate_data_type(cur_join->value, type, parse_context);
			result |= populate_data_type(cur_join->condition, type, parse_context);
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
		case PARAMETER_VALUE:	   // Note: This is a possibility in "populate_data_type" but not in "hash_canonical_query"
		case UNKNOWN_SqlValueType: // Note: This is a possibility in "populate_data_type" but not in "hash_canonical_query"
			*type = value->type;
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
			*type = value->coerced_type;
			break;
		default:
			assert(FALSE);
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			break;
		}
		break;
	case column_alias_STATEMENT:
		if (column_list_alias_STATEMENT == v->v.column_alias->column->type) {
			result |= populate_data_type(v->v.column_alias->column, type, parse_context);
		} else {
			UNPACK_SQL_STATEMENT(column, v->v.column_alias->column, column);
			switch (column->data_type_struct.data_type) {
			case BOOLEAN_TYPE:
				*type = BOOLEAN_VALUE;
				break;
			case INTEGER_TYPE:
				*type = INTEGER_LITERAL;
				break;
			case NUMERIC_TYPE:
				*type = NUMERIC_LITERAL;
				break;
			case STRING_TYPE:
				*type = STRING_LITERAL;
				break;
			case UNKNOWN_SqlDataType:
				// This could be a column that came in from a sub-query before when the sub-query column
				// was qualified (in "qualify_statement.c"). But by now we would have finished qualifying
				// all columns so we should be able to find out the column type at this point. Fix it now.
				assert(NULL != column->pre_qualified_cla);
				result |= populate_data_type(column->pre_qualified_cla->column_list, type, parse_context);
				/* This is a column that is formed in the middle of the query.
				 * So consider size/precision/scale as unspecified in this case.
				 */
				column->data_type_struct.size_or_precision = SIZE_OR_PRECISION_UNSPECIFIED;
				column->data_type_struct.scale = SCALE_UNSPECIFIED;
				switch (*type) {
				case BOOLEAN_VALUE:
					column->data_type_struct.data_type = BOOLEAN_TYPE;
					break;
				case INTEGER_LITERAL:
					column->data_type_struct.data_type = INTEGER_TYPE;
					break;
				case NUMERIC_LITERAL:
					column->data_type_struct.data_type = NUMERIC_TYPE;
					break;
				case STRING_LITERAL:
					column->data_type_struct.data_type = STRING_TYPE;
					break;
				case NUL_VALUE:
					/* NULL values need to be treated as some known type. We choose STRING_TYPE
					 * as this corresponds to the TEXT type of postgres to be compatible with it.
					 * See https://doxygen.postgresql.org/parse__coerce_8c.html#l01373 for more details.
					 */
					column->data_type_struct.data_type = STRING_TYPE;
					break;
				default:
					assert(FALSE);
					break;
				}
				break;
			default:
				assert(FALSE);
				ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
				result = 1;
				break;
			}
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
	case table_alias_STATEMENT:
		UNPACK_SQL_STATEMENT(table_alias, v, table_alias);
		result |= populate_data_type(table_alias->table, type, parse_context);
		assert((select_STATEMENT != table_alias->table->type)
		       || (table_alias->table->v.select->select_list == table_alias->column_list));
		if (select_STATEMENT != table_alias->table->type)
			result |= populate_data_type(table_alias->column_list, &child_type1, parse_context);
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
		CAST_AMBIGUOUS_TYPES(child_type1, child_type2);
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
					value->coerced_type = STRING_LITERAL;
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
		{
			SqlTableAlias *	    table_alias[2];
			SqlColumnListAlias *cur_cla[2], *start_cla[2];
			SqlStatement *	    sql_stmt;
			boolean_t	    terminate_loop[2] = {FALSE, FALSE};
			SqlColumnListAlias *type_mismatch_cla[2] = {NULL, NULL};
			SqlColumnListAlias *cur_set_cla, *start_set_cla;
			SqlSetOperation *   set_operand;
			int		    i;

			for (i = 0; i < 2; i++) {
				sql_stmt = set_operation->operand[i];
				if (table_alias_STATEMENT == sql_stmt->type) {
					UNPACK_SQL_STATEMENT(table_alias[i], sql_stmt, table_alias);
					assert(NULL != table_alias[i]->column_list);
					UNPACK_SQL_STATEMENT(start_cla[i], table_alias[i]->column_list, column_list_alias);
				} else {
					assert(set_operation_STATEMENT == sql_stmt->type);
					UNPACK_SQL_STATEMENT(set_operand, sql_stmt, set_operation);
					start_cla[i] = set_operand->col_type_list;
				}
				assert(NULL != start_cla[i]);
				cur_cla[i] = start_cla[i];
			}
			assert(NULL == set_operation->col_type_list);
			start_set_cla = NULL;
			do {
				SqlValueType left_type, right_type;
				boolean_t    is_type_mismatch;

				left_type = cur_cla[0]->type;
				right_type = cur_cla[1]->type;
				/* Assert all possible valid types. This is used to simplify the `if` checks below
				 * that determine the value of `is_type_mismatch`.
				 */
				assert((BOOLEAN_VALUE == left_type) || (INTEGER_LITERAL == left_type)
				       || (NUMERIC_LITERAL == left_type) || (STRING_LITERAL == left_type)
				       || (NUL_VALUE == left_type));
				assert((BOOLEAN_VALUE == right_type) || (INTEGER_LITERAL == right_type)
				       || (NUMERIC_LITERAL == right_type) || (STRING_LITERAL == right_type)
				       || (NUL_VALUE == right_type));
				/* If not yet found any type mismatch, check for one. If already found one, keep just that.
				 * In general, all types are compatible with only themselves.
				 * Exception is that
				 *	a) NUMERIC and INTEGER are compatible with each other and no other type.
				 *	b) NULL is compatible with any type.
				 */
				if (NULL == type_mismatch_cla[0]) {
					switch (left_type) {
					case BOOLEAN_VALUE:
						is_type_mismatch = ((NUL_VALUE != right_type) && (BOOLEAN_VALUE != right_type));
						break;
					case INTEGER_LITERAL:
					case NUMERIC_LITERAL:
						is_type_mismatch = ((NUL_VALUE != right_type) && (INTEGER_LITERAL != right_type)
								    && (NUMERIC_LITERAL != right_type));
						break;
					case STRING_LITERAL:
						is_type_mismatch = ((NUL_VALUE != right_type) && (STRING_LITERAL != right_type));
						break;
					case NUL_VALUE:
						is_type_mismatch = FALSE;
						break;
					default:
						assert(FALSE);
						FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
						// This line exists to prevent a -Wmaybe-uninitialized compiler warning and is
						// unreachable after the preceding assert (Debug builds) or FATAL (Release builds)
						is_type_mismatch = TRUE;
						break;
					}
					if (is_type_mismatch) {
						/* Record the first type mismatch location */
						type_mismatch_cla[0] = cur_cla[0];
						type_mismatch_cla[1] = cur_cla[1];
					}
				}
				/* Construct `column_list` for `set_operation` (needed by caller `populate_data_type`) */
				OCTO_CMALLOC_STRUCT(cur_set_cla, SqlColumnListAlias);
				if (NUL_VALUE != left_type) {
					/* Left side column is not NULL, inherit that type for outer SET operation */
					*cur_set_cla = *cur_cla[0];
				} else {
					/* Left side column is NULL, inherit right side column type
					 * for outer SET operation.
					 */
					*cur_set_cla = *cur_cla[1];
				}
				dqinit(cur_set_cla);
				if (NULL != start_set_cla) {
					dqappend(start_set_cla, cur_set_cla);
				} else {
					start_set_cla = cur_set_cla;
				}
				for (i = 0; i < 2; i++) {
					cur_cla[i] = cur_cla[i]->next;
					if (cur_cla[i] == start_cla[i])
						terminate_loop[i] = TRUE;
				}
			} while (!terminate_loop[0] && !terminate_loop[1]);
			if (terminate_loop[0] != terminate_loop[1]) {
				// The # of columns in the two operands (of the SET operation) do not match. Issue error.
				ERROR(ERR_SETOPER_NUMCOLS_MISMATCH, get_set_operation_string(set_operation->type));
				location = ((!terminate_loop[0]) ? cur_cla[0]->column_list->loc : cur_cla[1]->column_list->loc);
				yyerror(&location, NULL, NULL, NULL, NULL, NULL);
				result = 1;
			} else if (NULL != type_mismatch_cla[0]) {
				// The type of one column in the two operands (of the SET operation) do not match. Issue error.
				ERROR(ERR_SETOPER_TYPE_MISMATCH, get_set_operation_string(set_operation->type),
				      get_user_visible_type_string(type_mismatch_cla[0]->type),
				      get_user_visible_type_string(type_mismatch_cla[1]->type));
				location = type_mismatch_cla[0]->column_list->loc;
				yyerror(&location, NULL, NULL, NULL, NULL, NULL);
				location = type_mismatch_cla[1]->column_list->loc;
				yyerror(&location, NULL, NULL, NULL, NULL, NULL);
				result = 1;
			}
			assert(NULL != start_set_cla);
			set_operation->col_type_list = start_set_cla;
		}
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
		assert((BOOLEAN_NOT != unary->operation) || (BOOLEAN_VALUE == *type) || (NUL_VALUE == *type)
		       || (INTEGER_LITERAL == *type) || (NUMERIC_LITERAL == *type) || (STRING_LITERAL == *type));
		break;
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		result = 1;
		break;
	}
	return result;
}
