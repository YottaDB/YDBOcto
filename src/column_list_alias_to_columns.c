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
	SqlColumnListAlias	*cla_start, *cla_cur;
	SqlColumn		*start_column = NULL, *cur_column;
	SqlOptionalKeyword	*keyword;
	SqlStatement		*stmt;
	int			column_number;

	if(table_alias->column_list == NULL)
		return NULL;
	UNPACK_SQL_STATEMENT(cla_start, table_alias->column_list, column_list_alias);
	cla_cur = cla_start;
	column_number = 0;
	do {
		column_number++;
		OCTO_CMALLOC_STRUCT(cur_column, SqlColumn);
		cur_column->table = table_alias->table;
		cur_column->columnName = cla_cur->alias;

		SQL_STATEMENT(cur_column->keywords, keyword_STATEMENT);
		MALLOC_STATEMENT(cur_column->keywords, keyword, SqlOptionalKeyword);
		dqinit(cur_column->keywords->v.keyword);
		switch(cla_cur->type) {
		case UNKNOWN_SqlValueType:
			cur_column->type = UNKNOWN_SqlDataType;
			cur_column->pre_qualified_cla = cla_cur;
			break;
		case NUMERIC_LITERAL:
			cur_column->type = NUMERIC_TYPE;
			break;
		case INTEGER_LITERAL:
			cur_column->type = INTEGER_TYPE;
			break;
		case STRING_LITERAL:
			cur_column->type = CHARACTER_STRING_TYPE;
			break;
		default:
			assert(FALSE);
			ERROR(ERR_FEATURE_NOT_IMPLEMENTED, "");
			return NULL;
		}
		// Initialize COLUMN # field
		keyword	= add_optional_piece_keyword_to_sql_column(column_number);
		PACK_SQL_STATEMENT(stmt, keyword, keyword);
		cur_column->keywords = stmt;
		cur_column->column_number = column_number;
		// Initialize remaining fields
		dqinit(cur_column);
		if(start_column == NULL) {
			start_column = cur_column;
		} else {
			dqappend(cur_column, start_column);
		}
		cla_cur = cla_cur->next;
	} while(cla_start != cla_cur);
	return start_column;
}
