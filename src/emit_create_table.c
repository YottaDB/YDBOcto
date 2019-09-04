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

#include "octo.h"
#include "octo_types.h"

// Emits DDL specification for the given table
// Args:
//	FILE *output: output file to write DDL to
//	SqlStatement *stmt: a SqlTable type SqlStatement
// Returns:
//	0 for success, 1 for error
int emit_create_table(FILE *output, struct SqlStatement *stmt)
{
	int status = 0;
	SqlColumn *start_column, *cur_column;
	SqlTable *table;
	SqlValue *value;
	SqlOptionalKeyword *keyword;
	char buffer[MAX_STR_CONST];
	if(stmt == NULL)
		return 0;
	table = stmt->v.table;
	assert(table->tableName);
	assert(table->columns);
	UNPACK_SQL_STATEMENT(value, table->tableName, value);
	fprintf(output, "CREATE TABLE `%s` (", value->v.reference);
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		status = emit_column_specification(buffer, MAX_STR_CONST, cur_column);
		if (0 > status) {
			return 1;
		}
		fprintf(output, "%s", buffer);
		cur_column = cur_column->next;
		if(start_column != cur_column)
			fprintf(output, ", ");
	} while(start_column != cur_column);
	fprintf(output, ")");
	if(table->source) {
		UNPACK_SQL_STATEMENT(keyword, table->source, keyword);
		UNPACK_SQL_STATEMENT(value, keyword->v, value);
		m_escape_string2(buffer, MAX_STR_CONST, value->v.reference);
		fprintf(output, " GLOBAL \"%s\"", buffer);
	}
	if(table->delim) {
		UNPACK_SQL_STATEMENT(keyword, table->delim, keyword);
		UNPACK_SQL_STATEMENT(value, keyword->v, value);
		m_escape_string2(buffer, MAX_STR_CONST, value->v.reference);
		fprintf(output, " DELIM \"%s\"", buffer);
	}
	fprintf(output, ";");
	return 0;
}
