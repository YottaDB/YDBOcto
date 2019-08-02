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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

SqlTable *copy_sql_table(SqlTable *table) {
	SqlTable *ret;
	SqlStatement *stmt;
	SqlColumn *cur_column, *start_column, *new_column, *t_column;
	SqlOptionalKeyword *keyword;

	OCTO_CMALLOC_STRUCT(ret, SqlTable);
	memset(ret, 0, sizeof(SqlTable));

	if(table->tableName) {
		ret->tableName = copy_sql_statement(table->tableName);
	}
	if(table->source) {
		ret->source = copy_sql_statement(table->source);
	}
	assert(table->columns != NULL);
	SQL_STATEMENT(stmt, column_STATEMENT);
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		OCTO_CMALLOC_STRUCT(new_column, SqlColumn);
		*new_column = *cur_column;
		dqinit(new_column);
		new_column->columnName = copy_sql_statement(cur_column->columnName);
		// This gives us trouble with a table copying the column itself;
		//  just setup a pointer
		//new_column->table = copy_sql_statement(cur_column->table);
		//SQL_STATEMENT(new_column->table, table_STATEMENT);
		//new_column->table->v.table = cur_column->table->v.table;
		//new_column->table = cur_column->table;
		new_column->keywords = copy_sql_statement(cur_column->keywords);
		if(stmt->v.column == NULL) {
			MALLOC_STATEMENT(stmt, column, SqlColumn);
			stmt->v.column = new_column;
		} else {
			dqinsert(new_column, stmt->v.column, t_column);
		}
		cur_column = cur_column->next;
	} while(cur_column != start_column);
	ret->columns = stmt;
	//ret->columns = copy_sql_statement(table->columns);
	PACK_SQL_STATEMENT(stmt, ret, table);
	assign_table_to_columns(stmt);
	if(table->delim) {
		if(ret->source != NULL) {
			UNPACK_SQL_STATEMENT(keyword, ret->source, keyword);
			keyword = get_keyword_from_keywords(keyword, OPTIONAL_DELIM);
			PACK_SQL_STATEMENT(table->delim, keyword, keyword);
		}
		if(ret->delim == NULL) {
			ret->delim = copy_sql_statement(table->delim);
		}
	}
	dqinit(ret);

	return ret;
}
