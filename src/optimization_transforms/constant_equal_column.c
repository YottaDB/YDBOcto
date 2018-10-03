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
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

/**
 * When we have a constant equal to a column, and the column is a key, we can change the cursor to fix the key
 */
int constant_equal_column(SqlStatement *stmt, SqlJoin *join) {
	SqlBinaryOperation *binary;
	SqlValue *value;
	SqlValue *column_reference = NULL, *string_literal = NULL;
	SqlColumn *column;
	SqlTable *table;
	SqlOptionalKeyword *keyword, *new_keyword, *t_keyword;
	SqlJoin *cur_join, *start_join;
	int len;

	UNPACK_SQL_STATEMENT(binary, stmt, binary);
	if(binary->operation != BOOLEAN_EQUALS)
		return 0;
	if(binary->operands[0]->type != value_STATEMENT || binary->operands[1]->type != value_STATEMENT)
		return 0;
	UNPACK_SQL_STATEMENT(value, binary->operands[0], value);
	if(value->type == COLUMN_REFERENCE)
		column_reference = value;
	else if(value->type == NUMBER_LITERAL || value->type == STRING_LITERAL)
		string_literal = value;
	else
		return 0;
	UNPACK_SQL_STATEMENT(value, binary->operands[1], value);
	if(value->type == COLUMN_REFERENCE) {
		if(column_reference != NULL)
			return 0;
		column_reference = value;
	}
	else if(value->type == NUMBER_LITERAL || value->type == STRING_LITERAL) {
		if(string_literal != NULL)
			return 0;
		string_literal = value;
	}
	else
		return 0;
	// If the column referenced is a key for this table, we can replace the key in the cursor for this table
	/// TODO: macro to assert the the column name is qualified
	if(find_qualified_column(column_reference, &table, &column) != 0)
		return 0;
	keyword = get_keyword(column, PRIMARY_KEY);
	if(keyword == NULL)
		keyword = get_keyword(column, OPTIONAL_KEY_NUM);
	/// TODO: we should create a new table with this item as a key, an xref table, and try again
	if(keyword == NULL)
		return 0;
	/// TODO: we should do a deep clone of the table to replace it with some value
	keyword = get_keyword(column, OPTIONAL_ADVANCE);
	if(keyword == NULL) {
		new_keyword = (SqlOptionalKeyword *)malloc(sizeof(SqlOptionalKeyword));
		new_keyword->keyword = OPTIONAL_ADVANCE;
		SQL_STATEMENT(new_keyword->v, value_STATEMENT);
		MALLOC_STATEMENT(new_keyword->v, value, SqlValue);
		dqinit(new_keyword);
		UNPACK_SQL_STATEMENT(keyword, column->keywords, keyword);
		dqinsert(keyword, new_keyword, t_keyword);
		keyword = new_keyword;
	}
	if(keyword->v->v.value->v.string_literal)
		free(keyword->v->v.value->v.string_literal);
	len = strlen(string_literal->v.string_literal);
	keyword->v->v.value->v.string_literal = malloc(len + 4);
	*keyword->v->v.value->v.string_literal = '"';
	memcpy(keyword->v->v.value->v.string_literal + 1, string_literal->v.string_literal, len);
	*(keyword->v->v.value->v.string_literal + len + 1) = '"';
	*(keyword->v->v.value->v.string_literal + len + 2) = '\0';
	return 1;
}
