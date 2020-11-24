/****************************************************************
 *                                                              *
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.      *
 * All rights reserved.                                         *
 *                                                              *
 *      This source code contains the intellectual property     *
 *      of its copyright holder(s), and is made available       *
 *      under a license.  If you do not know the terms of       *
 *      the license, please stop and do not read further.       *
 *                                                              *
 ****************************************************************/

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

boolean_t is_stmt_table_asterisk(SqlStatement *stmt) {
	SqlValue *	    value;
	SqlColumnListAlias *cla;
	SqlColumnList *	    cl;
	SqlColumnAlias *    ca;
	boolean_t	    ret;

	ret = FALSE;
	for (; NULL != stmt;) {
		switch (stmt->type) {
		case value_STATEMENT:
			UNPACK_SQL_STATEMENT(value, stmt, value);
			if (TABLE_ASTERISK == value->type)
				ret = TRUE;
			stmt = NULL;
			break;
		case column_list_alias_STATEMENT:
			UNPACK_SQL_STATEMENT(cla, stmt, column_list_alias);
			UNPACK_SQL_STATEMENT(cl, cla->column_list, column_list);
			stmt = ((column_alias_STATEMENT == cl->value->type) || (value_STATEMENT == cl->value->type)) ? cl->value
														     : NULL;
			break;
		case column_alias_STATEMENT:
			UNPACK_SQL_STATEMENT(ca, stmt, column_alias);
			stmt = ca->column;
			break;
		default:
			stmt = NULL;
			break;
		}
	}
	return ret;
}
