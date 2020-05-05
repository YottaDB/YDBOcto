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

#include "logical_plan.h"

/* Note: ambiguous is an output parameter. `*ambiguous` is set to TRUE if multiple columns in the input
 * `table_alias` match the input `column_name`.
 */
SqlColumnListAlias *match_column_in_table(SqlTableAlias *table_alias, char *column_name, int column_name_len,
						boolean_t *ambiguous, boolean_t issue_error)
{
	SqlColumnListAlias	*cur_column_list, *start_column_list, *ret;

	// If there is no column list for this table alias, we won't match anything
	if (NULL == table_alias->column_list)
		return NULL;
	UNPACK_SQL_STATEMENT(start_column_list, table_alias->column_list, column_list_alias);
	cur_column_list = start_column_list;
	ret = NULL;
	*ambiguous = FALSE;
	do {
		if (NULL != cur_column_list->alias) {
			int		value_len;
			SqlValue	*value;
#			ifndef NDEBUG
			SqlColumnList	*column_list;

			UNPACK_SQL_STATEMENT(column_list, cur_column_list->column_list, column_list);
			assert(column_list == column_list->next);
			assert(column_list == column_list->prev);
#			endif
			UNPACK_SQL_STATEMENT(value, cur_column_list->alias, value);
			value_len = strlen(value->v.string_literal);
			if ((value_len == column_name_len)
					&& memcmp(value->v.string_literal, column_name, column_name_len) == 0) {
				if (NULL != ret) {
					/* We found at least 2 matching columns. Signal ambiguous reference
					 * so caller can issue error. We can break out of the loop now.
					 */
					*ambiguous = TRUE;
					if (issue_error) {
						ERROR(ERR_AMBIGUOUS_COLUMN_NAME, column_name);
					}
					break;
				}
				ret = cur_column_list;
				/* Note: We cannot break out of the loop yet as we could have multiple columns with the same name.
				 * We can find that out only by checking for the rest of the columns.
				 */
			}
		}
		cur_column_list = cur_column_list->next;
	} while (cur_column_list != start_column_list);
	return ret;
}
