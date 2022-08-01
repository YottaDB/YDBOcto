/****************************************************************
 *								*
 * Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

/* This header file contains macros, typedefs, prototypes etc. that are commonly used by various files.
 * They are not bundled inside "octo.h" to keep that file from being a dumping ground for everything.
 */

/* Note: This macro can be invoked with a NULL value of PARSE_CONTEXT if it is called by "ensure_same_type()"
 * from "qualify_check_constraint.c" hence the check for `NULL != PARSE_CONTEXT` below. For all callers of this
 * macro in "populate_data_type.c", we are guaranteed PARSE_CONTEXT is non-NULL.
 */
#define MAP_TYPE_TO_PARAMETER_VALUE(TYPE_1, TYPE_2, RESULT, PARSE_CONTEXT)                                                        \
	{                                                                                                                         \
		if ((PARAMETER_VALUE == TYPE_2) && (PARAMETER_VALUE == TYPE_1)) {                                                 \
			ERROR(ERR_FEATURE_NOT_IMPLEMENTED, "comparison between literal parameters, e.g. $1 = $2");                \
			RESULT = 1;                                                                                               \
		} else {                                                                                                          \
			TYPE_1 = TYPE_2;                                                                                          \
			if ((NULL != PARSE_CONTEXT) && (PARSE_CONTEXT->cur_param_num)) {                                          \
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

/* Converts ambiguous SqlValueTypes to determinate types.
 * Specifically:
 *	1. Uses DDL-specified types for prepared statement parameter types if not specified by client
 *	2. Converts INTEGER_LITERALs to NUMERIC_LITERALs, as they are equivalent internally within Octo
 * Note: This macro can be invoked with a NULL value of PARSE_CONTEXT if it is called by "ensure_same_type()"
 * from "qualify_check_constraint.c". The MAP_TYPE_TO_PARAMETER_VALUE macro handles that (see comment there).
 */
#define CAST_AMBIGUOUS_TYPES(TYPE1, TYPE2, RESULT, PARSE_CONTEXT)                 \
	if ((PARAMETER_VALUE == TYPE1) || IS_NUL_VALUE(TYPE1)) {                  \
		MAP_TYPE_TO_PARAMETER_VALUE(TYPE1, TYPE2, RESULT, PARSE_CONTEXT); \
	} else if ((PARAMETER_VALUE == TYPE2) || IS_NUL_VALUE(TYPE2)) {           \
		MAP_TYPE_TO_PARAMETER_VALUE(TYPE2, TYPE1, RESULT, PARSE_CONTEXT); \
	} else if ((INTEGER_LITERAL == TYPE1) && (NUMERIC_LITERAL == TYPE2)) {    \
		TYPE1 = TYPE2;                                                    \
	} else if ((INTEGER_LITERAL == TYPE2) && (NUMERIC_LITERAL == TYPE1)) {    \
		TYPE2 = TYPE1;                                                    \
	} else if (BOOLEAN_OR_STRING_LITERAL == TYPE1) {                          \
		switch (TYPE2) {                                                  \
		case BOOLEAN_VALUE:                                               \
			FIX_TYPE_TO_BOOLEAN_VALUE(TYPE1);                         \
			break;                                                    \
		case STRING_LITERAL:                                              \
			FIX_TYPE_TO_STRING_LITERAL(TYPE1);                        \
			break;                                                    \
		default:                                                          \
			break;                                                    \
		}                                                                 \
	} else if (BOOLEAN_OR_STRING_LITERAL == TYPE2) {                          \
		switch (TYPE1) {                                                  \
		case BOOLEAN_VALUE:                                               \
			FIX_TYPE_TO_BOOLEAN_VALUE(TYPE2);                         \
			break;                                                    \
		case STRING_LITERAL:                                              \
			FIX_TYPE_TO_STRING_LITERAL(TYPE2);                        \
			break;                                                    \
		default:                                                          \
			break;                                                    \
		}                                                                 \
	}

#define FIX_TYPE_TO_BOOLEAN_VALUE(TYPE)                    \
	{                                                  \
		assert(BOOLEAN_OR_STRING_LITERAL == TYPE); \
		TYPE = BOOLEAN_VALUE;                      \
	}

#define FIX_TYPE_TO_STRING_LITERAL(TYPE)                   \
	{                                                  \
		assert(BOOLEAN_OR_STRING_LITERAL == TYPE); \
		TYPE = STRING_LITERAL;                     \
	}

#define ISSUE_TYPE_COMPATIBILITY_ERROR(CHILD_TYPE, OPERATION, OPERAND, RESULT)                       \
	{                                                                                            \
		ERROR(ERR_TYPE_NOT_COMPATIBLE, get_user_visible_type_string(CHILD_TYPE), OPERATION); \
		yyerror(NULL, NULL, OPERAND, NULL, NULL, NULL);                                      \
		RESULT = 1;                                                                          \
	}

// Compare TYPE1 and TYPE2 and throw ERR_TYPE if not equal
#define CHECK_TYPE_AND_BREAK_ON_MISMATCH(TYPE1, TYPE2, ERR_TYPE, CUR_BRANCH_VALUE, NEXT_BRANCH_VALUE, RESULT)            \
	{                                                                                                                \
		if ((TYPE1) != (TYPE2)) {                                                                                \
			/* If it possible one of the types is BOOLEAN_OR_STRING_LITERAL. In that case, the user visible  \
			 * type is STRING and so modify the type before calling "get_user_visible_type_string()" below.  \
			 */                                                                                              \
			if (BOOLEAN_OR_STRING_LITERAL == TYPE1) {                                                        \
				TYPE1 = STRING_LITERAL;                                                                  \
			}                                                                                                \
			if (BOOLEAN_OR_STRING_LITERAL == TYPE2) {                                                        \
				TYPE2 = STRING_LITERAL;                                                                  \
			}                                                                                                \
			ERROR((ERR_TYPE), get_user_visible_type_string((TYPE1)), get_user_visible_type_string((TYPE2))); \
			yyerror(NULL, NULL, (CUR_BRANCH_VALUE), NULL, NULL, NULL);                                       \
			if (NULL != (NEXT_BRANCH_VALUE)) {                                                               \
				yyerror(NULL, NULL, (NEXT_BRANCH_VALUE), NULL, NULL, NULL);                              \
			}                                                                                                \
			RESULT = 1;                                                                                      \
			break;                                                                                           \
		}                                                                                                        \
	}

typedef int (*DataTypeCallback)(SqlValueType *left_type, SqlValueType *right_type, SqlStatement *left_stmt,
				SqlStatement *right_stmt, ParseContext *parse_context);
int ensure_same_type(SqlValueType *left_type, SqlValueType *right_type, SqlStatement *left_stmt, SqlStatement *right_stmt,
		     ParseContext *parse_context);
int binary_operation_data_type_check(SqlBinaryOperation *binary, SqlValueType child_type[2], SqlValueType *type,
				     ParseContext *parse_context);
int unary_operation_data_type_check(SqlUnaryOperation *unary, SqlValueType child_type[2], SqlValueType *type);
int function_call_data_type_check(SqlStatement *fc_stmt, SqlValueType *type, ParseContext *parse_context, SqlTable *table);
