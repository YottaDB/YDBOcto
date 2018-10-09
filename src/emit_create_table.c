/* Copyright (C) 2018 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

/**
 * Emits DDL specification for the given table
 */
void emit_create_table(FILE *output, struct SqlStatement *stmt)
{
	SqlColumn *start_column, *cur_column;
	SqlTable *table;
	SqlValue *value;
	SqlOptionalKeyword *keyword, *cur_keyword, *start_keyword;
	char buffer[MAX_STR_CONST];
	if(stmt == NULL)
		return;
	table = stmt->v.table;
	assert(table->tableName);
	assert(table->columns);
	UNPACK_SQL_STATEMENT(value, table->tableName, value);
	fprintf(output, "CREATE TABLE %s (", value->v.reference);
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		emit_column_specification(buffer, MAX_STR_CONST, cur_column);
		fprintf(output, "%s", buffer);
		cur_column = cur_column->next;
		if(start_column != cur_column)
			fprintf(output, ", ");
	} while(start_column != cur_column);
	assert(table->source);
	UNPACK_SQL_STATEMENT(keyword, table->source, keyword);
	UNPACK_SQL_STATEMENT(value, keyword->v, value);
	fprintf(output, ") GLOBAL \"%s\"", value->v.reference);
	assert(table->curse);
	UNPACK_SQL_STATEMENT(keyword, table->curse, keyword);
	UNPACK_SQL_STATEMENT(value, keyword->v, value);
	fprintf(output, " CURSOR \"%s\"", value->v.reference);
	assert(table->start);
	UNPACK_SQL_STATEMENT(keyword, table->start, keyword);
	UNPACK_SQL_STATEMENT(value, keyword->v, value);
	fprintf(output, " UNPACK \"%s\"", value->v.reference);
	assert(table->end);
	UNPACK_SQL_STATEMENT(keyword, table->end, keyword);
	UNPACK_SQL_STATEMENT(value, keyword->v, value);
	fprintf(output, " END \"%s\"", value->v.reference);
	assert(table->delim);
	UNPACK_SQL_STATEMENT(keyword, table->delim, keyword);
	UNPACK_SQL_STATEMENT(value, keyword->v, value);
	fprintf(output, " DELIM \"%s\"", value->v.reference);
	assert(table->pack);
	UNPACK_SQL_STATEMENT(keyword, table->pack, keyword);
	UNPACK_SQL_STATEMENT(value, keyword->v, value);
	fprintf(output, " PACK \"%s\";", value->v.reference);
}
