/****************************************************************
 *								*
 * Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	*
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
#include "octo_type_check.h"

/* Note: might also perform type promotion on both types inside the CAST_AMBIGUOUS_TYPES macro.
 * parse_context can be NULL in case it is called from "qualify_check_constraint.c" whereas it is guaranteed
 * to be non-NULL in case of calls from "populate_data_type.c" hence the CAST_AMBIGUOUS_TYPES macro handles both cases.
 */
int ensure_same_type(SqlValueType *left_type, SqlValueType *right_type, SqlStatement *left_stmt, SqlStatement *right_stmt,
		     ParseContext *parse_context) {
	int result;

	result = 0;
	CAST_AMBIGUOUS_TYPES(*left_type, *right_type, result, parse_context);
	if (result) {
		return result;
	}
	if (*left_type != *right_type) {
		if (BOOLEAN_OR_STRING_LITERAL == *left_type) {
			FIX_TYPE_TO_STRING_LITERAL(*left_type);
		}
		if (BOOLEAN_OR_STRING_LITERAL == *right_type) {
			FIX_TYPE_TO_STRING_LITERAL(*right_type);
		}
		// check_column_lists_for_type_match.c has similar checks any change to the below code should also reflect there
		if ((DATE_LITERAL == *left_type) && IS_TIMESTAMP(*right_type)) {
			// Convert DATE to TIMESTAMP
			ADD_DATE_TIME_TIMESTAMP_CAST_STMT(left_stmt, *right_type, *left_type);
			*left_type = *right_type;
			return result; // This is ok
		}
		if (IS_TIMESTAMP(*left_type) && (DATE_LITERAL == *right_type)) {
			// Convert DATE to TIMESTAMP
			ADD_DATE_TIME_TIMESTAMP_CAST_STMT(right_stmt, *left_type, *right_type);
			*right_type = *left_type;
			return result; // This is ok
		}
		if ((TIMESTAMP_WITH_TIME_ZONE_LITERAL == *left_type) && (TIMESTAMP_LITERAL == *right_type)) {
			// Convert TIMESTAMP to TIMESTAMP WITH TIME ZONE
			ADD_DATE_TIME_TIMESTAMP_CAST_STMT(left_stmt, *left_type, *right_type);
			*right_type = *left_type;
			return result; // This is ok
		}
		if ((TIMESTAMP_LITERAL == *left_type) && (TIMESTAMP_WITH_TIME_ZONE_LITERAL == *right_type)) {
			// Convert TIMESTAMP to TIMESTAMP WITH TIME ZONE
			ADD_DATE_TIME_TIMESTAMP_CAST_STMT(left_stmt, *right_type, *left_type);
			*left_type = *right_type;
			return result; // This is ok
		}
		if ((TIME_LITERAL == *left_type) && (TIME_WITH_TIME_ZONE_LITERAL == *right_type)) {
			// Convert TIME to TIME WITH TIME ZONE
			ADD_DATE_TIME_TIMESTAMP_CAST_STMT(left_stmt, *right_type, *left_type);
			*left_type = *right_type;
			return result; // This is ok
		}
		if ((TIME_WITH_TIME_ZONE_LITERAL == *left_type) && (TIME_LITERAL == *right_type)) {
			// Convert TIME to TIME WITH TIME ZONE
			ADD_DATE_TIME_TIMESTAMP_CAST_STMT(right_stmt, *left_type, *right_type);
			*right_type = *left_type;
			return result; // This is ok
		}
		if ((STRING_LITERAL == *left_type) && IS_DATE_TIME_TYPE(*right_type)) {
			// Convert STRING to date/time
			ADD_DATE_TIME_TIMESTAMP_CAST_STMT(left_stmt, *right_type, *left_type);
			*left_type = *right_type;
			return result; // This is ok
		}
		if (IS_DATE_TIME_TYPE(*left_type) && (STRING_LITERAL == *right_type)) {
			// Convert STRING to date/time
			ADD_DATE_TIME_TIMESTAMP_CAST_STMT(right_stmt, *left_type, *right_type);
			*right_type = *left_type;
			return result; // This is ok
		}
		ERROR(ERR_TYPE_MISMATCH, get_user_visible_type_string(*left_type), get_user_visible_type_string(*right_type));
		yyerror(&left_stmt->loc, NULL, NULL, NULL, parse_context, NULL);
		yyerror(&right_stmt->loc, NULL, NULL, NULL, parse_context, NULL);
		result = 1;
	}
	return result;
}
