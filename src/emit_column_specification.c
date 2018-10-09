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

int emit_column_specification(char *buffer, int buffer_size, SqlColumn *cur_column) {
	SqlValue *value;
	SqlOptionalKeyword *cur_keyword, *start_keyword;
	char *buff_ptr = buffer;
	UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
	buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "%s", value->v.reference);
	switch(cur_column->type)
       	{
	case INTEGER_TYPE:
		buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " INTEGER");
		break;
	case CHARACTER_STRING_TYPE:
		// We should determine the actual size based on the constraint
		buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " VARCHAR(%d)", 25);
		break;
	default:
		FATAL(ERR_UNKNOWN_KEYWORD_STATE);
		break;
	}
	UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
	cur_keyword = start_keyword;
	do {
		switch(cur_keyword->keyword)
		{
		case PRIMARY_KEY:
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " PRIMARY KEY");
			break;
		case NOT_NULL:
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " NOT NULL");
			break;
		case UNIQUE_CONSTRAINT:
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " UNIQUE");
			break;
		case OPTIONAL_EXTRACT:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " EXTRACT \"%s\"", value->v.reference);
			break;
		case OPTIONAL_PIECE:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " PIECE \"%s\"", value->v.reference);
			break;
		case OPTIONAL_SOURCE:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " GLOBAL \"%s\"", value->v.reference);
			break;
		case OPTIONAL_DELIM:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " GLOBAL \"%s\"", value->v.reference);
			break;
		case OPTIONAL_KEY_NUM:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " KEY NUM \"%s\"", value->v.reference);
			break;
		case OPTIONAL_ADVANCE:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " ADVANCE \"%s\"", value->v.reference);
			break;
		case NO_KEYWORD:
			break;
		default:
			FATAL(ERR_UNKNOWN_KEYWORD_STATE);
			break;
		}
		cur_keyword = cur_keyword->next;
	} while(cur_keyword != start_keyword);
	assert(buff_ptr - buffer < buffer_size);
	*buff_ptr = '\0';
	// We don't count the null character we added for ease of use
	return buff_ptr - buffer;
}
