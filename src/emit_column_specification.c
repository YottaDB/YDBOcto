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

int emit_column_specification(char *buffer, int buffer_size, SqlColumn *cur_column) {
	SqlValue *value;
	SqlOptionalKeyword *cur_keyword, *start_keyword;
	char *buff_ptr = buffer;
	char buffer2[MAX_STR_CONST];
	UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
	buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "`%s`", value->v.reference);
	switch(cur_column->type)
       	{
	case NUMERIC_TYPE:
		buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " NUMERIC");
		break;
	case INTEGER_TYPE:
		buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " INTEGER");
		break;
	case CHARACTER_STRING_TYPE:
		// We should determine the actual size based on the constraint
		buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " VARCHAR(%d)", 25);
		break;
	default:
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		return -1;
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
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " EXTRACT \"%s\"", buffer2);
			break;
		case OPTIONAL_PIECE:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " PIECE \"%s\"", buffer2);
			break;
		case OPTIONAL_SOURCE:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " GLOBAL \"%s\"", buffer2);
			break;
		case OPTIONAL_DELIM:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " GLOBAL \"%s\"", buffer2);
			break;
		case OPTIONAL_KEY_NUM:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " KEY NUM \"%s\"", buffer2);
			break;
		case OPTIONAL_ADVANCE:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " ADVANCE \"%s\"", buffer2);
			break;
		case NO_KEYWORD:
			break;
		case OPTIONAL_START:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " START \"%s\"", buffer2);
			break;
		case OPTIONAL_END:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " END \"%s\"", buffer2);
			break;
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			return -1;
			break;
		}
		cur_keyword = cur_keyword->next;
	} while(cur_keyword != start_keyword);
	assert(buff_ptr - buffer < buffer_size);
	*buff_ptr = '\0';
	// We don't count the null character we added for ease of use
	return buff_ptr - buffer;
}
