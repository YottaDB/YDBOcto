/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
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
	if (*left_type != *right_type) {
		ERROR(ERR_TYPE_MISMATCH, get_user_visible_type_string(*left_type), get_user_visible_type_string(*right_type));
		yyerror(&left_stmt->loc, NULL, NULL, NULL, parse_context, NULL);
		yyerror(&right_stmt->loc, NULL, NULL, NULL, parse_context, NULL);
		result = 1;
	}
	return result;
}
