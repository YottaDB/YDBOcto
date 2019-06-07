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

/**
 * Creates a list of columns given a column_list_alias; useful for passing
 *  ownership up a level
 */
SqlColumn *column_list_alias_to_columns(SqlTableAlias *table_alias) {
	SqlColumnListAlias *cla_start, *cla_cur;
	SqlColumn *start_column = NULL, *cur_column, *t_column;

	if(table_alias->column_list == NULL)
		return NULL;
	UNPACK_SQL_STATEMENT(cla_start, table_alias->column_list, column_list_alias);
	cla_cur = cla_start;
	do {
		cur_column = (SqlColumn*)octo_cmalloc(memory_chunks, sizeof(SqlColumn));
		memset(cur_column, 0, sizeof(SqlColumn));
		cur_column->table = table_alias->table;
		if(cla_cur != NULL) {
			cur_column->columnName = cla_cur->alias;
		}
		SQL_STATEMENT(cur_column->keywords, keyword_STATEMENT);
		MALLOC_STATEMENT(cur_column->keywords, keyword, SqlOptionalKeyword);
		dqinit(cur_column->keywords->v.keyword);
		switch(cla_cur->type) {
		case UNKNOWN_SqlValueType:
		case NUMBER_LITERAL:
			cur_column->type = INTEGER_TYPE;
			break;
		case STRING_LITERAL:
			cur_column->type = CHARACTER_STRING_TYPE;
			break;
		default:
			FATAL(ERR_FEATURE_NOT_IMPLEMENTED, "");
			break;
		}
		dqinit(cur_column);
		if(start_column == NULL) {
			start_column = cur_column;
		} else {
			dqinsert(cur_column, start_column, t_column);
		}
		cla_cur = cla_cur->next;
	} while(cla_start != cla_cur);
	return start_column;
}
