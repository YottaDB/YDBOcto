/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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
	SqlValue *	    value;
	SqlOptionalKeyword *cur_keyword, *start_keyword;
	char		    ch, *delim;
	char *		    buff_ptr = buffer;
	char		    buffer2[MAX_STR_CONST];
	DEBUG_ONLY(boolean_t piece_seen = FALSE);
	DEBUG_ONLY(boolean_t empty_delim_seen = FALSE);

	UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
	buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "`%s`", value->v.reference);
	switch (cur_column->data_type_struct.data_type) {
	case BOOLEAN_TYPE:
		/* For BOOLEAN, neither PRECISION nor SCALE apply. Assert that. */
		assert(SIZE_OR_PRECISION_UNSPECIFIED == cur_column->data_type_struct.size_or_precision);
		assert(SCALE_UNSPECIFIED == cur_column->data_type_struct.scale);
		buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " BOOLEAN");
		break;
	case INTEGER_TYPE:
		/* For INTEGER, only PRECISION may apply. Assert that. */
		assert(SCALE_UNSPECIFIED == cur_column->data_type_struct.scale);
		buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " INTEGER");
		if (SIZE_OR_PRECISION_UNSPECIFIED != cur_column->data_type_struct.size_or_precision) {
			/* SIZE was specified (e.g. INTEGER(8)). In that case, write out the "8" here */
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "(%d)",
					     cur_column->data_type_struct.size_or_precision);
		}
		break;
	case NUMERIC_TYPE:
		/* For NUMERIC, both PRECISION and SCALE may apply. Check both. */
		buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " NUMERIC");
		if (SIZE_OR_PRECISION_UNSPECIFIED != cur_column->data_type_struct.size_or_precision) {
			if (SCALE_UNSPECIFIED != cur_column->data_type_struct.scale) {
				/* PRECISION and SCALE were both specified (e.g. NUMERIC(8,4)).
				 * In that case, write out the "(8,4)" here.
				 */
				buff_ptr
				    += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "(%d,%d)",
						cur_column->data_type_struct.size_or_precision, cur_column->data_type_struct.scale);
			} else {
				/* Only PRECISION was specified (e.g. NUMERIC(8)).
				 * In that case, write out the "(8)" here.
				 */
				buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "(%d)",
						     cur_column->data_type_struct.size_or_precision);
			}
		} else {
			assert(SCALE_UNSPECIFIED == cur_column->data_type_struct.scale);
			/* Neither PRECISION nor SCALE were specified. No need to write anything more. */
		}
		break;
	case STRING_TYPE:
		/* For STRING, only SIZE may apply. Assert that. */
		assert(SCALE_UNSPECIFIED == cur_column->data_type_struct.scale);
		buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " VARCHAR");
		if (SIZE_OR_PRECISION_UNSPECIFIED != cur_column->data_type_struct.size_or_precision) {
			/* SIZE was specified (e.g. VARCHAR(30)). In that case, write out the "30" here */
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "(%d)",
					     cur_column->data_type_struct.size_or_precision);
		}
		break;
	default:
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		assert(FALSE);
		return -1;
		break;
	}
	UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
	cur_keyword = start_keyword;
	do {
		switch (cur_keyword->keyword) {
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
			DEBUG_ONLY(assert(!empty_delim_seen));
			DEBUG_ONLY(piece_seen = TRUE);
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " PIECE %s", buffer2);
			break;
		case OPTIONAL_SOURCE:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " GLOBAL \"%s\"", buffer2);
			break;
		case OPTIONAL_DELIM:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			delim = value->v.reference;
			ch = *delim;
			delim++; /* Skip first byte to get actual delimiter */
			assert((DELIM_IS_DOLLAR_CHAR == ch) || (DELIM_IS_LITERAL == ch));
			if (DELIM_IS_LITERAL == ch) {
				DEBUG_ONLY(empty_delim_seen = ('\0' == *delim));
				DEBUG_ONLY(assert(!empty_delim_seen || !piece_seen));
				m_escape_string2(buffer2, MAX_STR_CONST, delim);
				buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " DELIM \"%s\"", buffer2);
			} else {
				assert(!MEMCMP_LIT(delim, "$CHAR(")); /* this is added in parser.y */
				delim += sizeof("$CHAR") - 1;	      /* Skip "$CHAR" */
				buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " DELIM %s", delim);
			}
			break;
		case OPTIONAL_KEY_NUM:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " KEY NUM %s", buffer2);
			break;
		case NO_KEYWORD:
			break;
		case OPTIONAL_START:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " START \"%s\"", buffer2);
			break;
		case OPTIONAL_STARTINCLUDE:
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " STARTINCLUDE");
			break;
		case OPTIONAL_END:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(buffer2, MAX_STR_CONST, value->v.reference);
			buff_ptr += snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), " END \"%s\"", buffer2);
			break;
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			assert(FALSE);
			return -1;
			break;
		}
		cur_keyword = cur_keyword->next;
	} while (cur_keyword != start_keyword);
	assert(buff_ptr - buffer < buffer_size);
	*buff_ptr = '\0';
	// We don't count the null character we added for ease of use
	return buff_ptr - buffer;
}
