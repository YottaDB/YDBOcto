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
 * Returns a buffer containing some M code which can be used to retrieve columns from
 *  the specified global
 *
 * WARNING: caller is responsible for freeing the buffer
 */
void emit_simple_select(char *output, const SqlTable *table, const char *column_name, char *source_raw)
{
	SqlValue *tmp_value;
	SqlColumn *cur_column, *start_column;
	SqlOptionalKeyword *start_keyword, *cur_keyword, *keyword;
	char *temp, *source = source_raw, *tableName;
	const char *c, *column = column_name;
	char *delim="|", *piece_string = NULL;
	int piece_number;

	/* Assert that this is a qualified references for this table */
	for(c = column; *c != '.' && *c != '\0'; c++) {
		// Intentionally left blank
	}
	if(*c == '.')
		column = (c+1);
	else
		column = column_name;

	//char *m_template = "NEW temporaryVar,key SET temporaryVar=$INCREMENT(%s),key=temporaryVar";

	assert(table != NULL);

	piece_number = 1;
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		UNPACK_SQL_STATEMENT(tmp_value, cur_column->columnName, value);
		if(strcmp(column, tmp_value->v.reference) == 0)
			break;
		piece_number++;
		cur_column = cur_column->next;
	} while(cur_column != start_column);
	keyword = get_keyword(cur_column, OPTIONAL_EXTRACT);
	if(keyword != NULL) {
		UNPACK_SQL_STATEMENT(tmp_value, keyword->v, value);
		temp = m_unescape_string(tmp_value->v.string_literal);
		snprintf(output, MAX_EXPRESSION_LENGTH, "%s", temp);
		free(temp);
		return;
	}
	UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
	cur_keyword = start_keyword;
	do {
		switch(cur_keyword->keyword) {
		case OPTIONAL_EXTRACT:
			// This should be handled above since it has priority
			assert(FALSE);
			break;
		case OPTIONAL_PIECE:
			UNPACK_SQL_STATEMENT(tmp_value, cur_keyword->v, value);
			piece_string = m_unescape_string(tmp_value->v.string_literal);
			break;
		case OPTIONAL_SOURCE:
			UNPACK_SQL_STATEMENT(tmp_value, cur_keyword->v, value);
			source = m_unescape_string(tmp_value->v.string_literal);
			break;
		case OPTIONAL_KEY_NUM:
		case PRIMARY_KEY:
			/// TODO: we should refector this to use generate_key_name, but there is a performance cost with the current design
			UNPACK_SQL_STATEMENT(tmp_value, table->tableName, value);
			tableName = tmp_value->v.reference;
			snprintf(output, MAX_EXPRESSION_LENGTH, "keys(\"%s\",\"%s\")", tableName, column);
			return;
		case NOT_NULL:
		case UNIQUE_CONSTRAINT:
		case OPTIONAL_ADVANCE:
		case NO_KEYWORD:
			/* These are known states that have nothing to do with rendering the SELECT code */
			break;
		default:
			FATAL(ERR_UNKNOWN_KEYWORD_STATE);
			break;
		}
		cur_keyword = cur_keyword->next;
	} while(cur_keyword != start_keyword);
	if(piece_string)
		snprintf(output, MAX_EXPRESSION_LENGTH, "$PIECE(%s,\"%s\",%s)", source, delim, piece_string);
	else
		snprintf(output, MAX_EXPRESSION_LENGTH, "$PIECE(%s,\"%s\",%d)", source, delim, piece_number);
}
