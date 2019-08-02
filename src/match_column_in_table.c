/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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

SqlStatement *match_column_in_table(SqlTableAlias *table_alias, char *column_name, int column_name_len,
		boolean_t match_qualified_columns)
{
	SqlColumnListAlias	*cur_column_list, *start_column_list;
	SqlValue		*value;
	SqlStatement		*ret = NULL;
	int			value_len;

	// If there is no column list for this table alias, we won't match anything
	if(table_alias->column_list == NULL)
		return NULL;
	UNPACK_SQL_STATEMENT(start_column_list, table_alias->column_list, column_list_alias);
	cur_column_list = start_column_list;
	do {
		if (NULL != cur_column_list->alias) {
			SqlColumnList	*column_list;
			UNPACK_SQL_STATEMENT(column_list, cur_column_list->column_list, column_list);
			assert(column_list == column_list->next);
			assert(column_list == column_list->prev);
			/* If match_qualified_columns is TRUE, we will return an input column name as a valid name
			 *    only if it matches the name of an existing column in the table whose column name has already
			 *    been qualified by a prior call to the "qualify_column_name" function (this is checked by
			 *    the "column_alias_STATEMENT == column_list->value->type" check below.
			 * If match_qualified_columns is FALSE, we will return an input column name as a valid name
			 *    as long as it matches the name of an existing column in the table.
			 */
			if (!match_qualified_columns || (column_alias_STATEMENT == column_list->value->type))
			{	/* Only in this case are we guaranteed that the owning column has already been qualified */
				UNPACK_SQL_STATEMENT(value, cur_column_list->alias, value);
				value_len = strlen(value->v.string_literal);
				if ((value_len == column_name_len)
						&& memcmp(value->v.string_literal, column_name, column_name_len) == 0) {
					PACK_SQL_STATEMENT(ret, cur_column_list, column_list_alias);
					break;
				}
			}
		}
		cur_column_list = cur_column_list->next;
	} while(cur_column_list != start_column_list);

	return ret;
}
