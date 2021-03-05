/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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

#define INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(BUFFER, BUFFER_SIZE, BUFF_PTR, ...)                           \
	{                                                                                                         \
		int index;                                                                                        \
                                                                                                                  \
		index = BUFF_PTR - *BUFFER; /* Save current index into buffer to apply after resize, if needed */ \
		assert((0 <= index) && (*BUFFER_SIZE >= index));                                                  \
		/* Attempt to print value into buffer. If it won't fit, expand buffer and try again */            \
		BUFF_PTR += snprintf(BUFF_PTR, *BUFFER_SIZE - index, ##__VA_ARGS__);                              \
		while (0 >= (*BUFFER_SIZE - (BUFF_PTR - *BUFFER))) {                                              \
			char *tmp;                                                                                \
			int   new_size;                                                                           \
                                                                                                                  \
			new_size = *BUFFER_SIZE * 2;                                                              \
			tmp = (char *)malloc(sizeof(char) * new_size);                                            \
			memcpy(tmp, *BUFFER, *BUFFER_SIZE);                                                       \
			free(*BUFFER);                                                                            \
			*BUFFER = tmp;                                                                            \
			*BUFFER_SIZE = new_size;                                                                  \
			assert((0 <= index) && (*BUFFER_SIZE >= index));                                          \
			BUFF_PTR = *BUFFER + index;                                                               \
			BUFF_PTR += snprintf(BUFF_PTR, *BUFFER_SIZE - index, ##__VA_ARGS__);                      \
		}                                                                                                 \
	}

/* Returns
 *	> 0 if something was emitted for this column (most common case).
 *	  0 if this column was skipped (e.g. hidden key column)
 *	 -1 if there was an error
 */
int emit_column_specification(char **buffer, int *buffer_size, SqlColumn *cur_column) {
	SqlValue *	    value;
	SqlOptionalKeyword *cur_keyword, *start_keyword;
	char		    ch, *delim;
	char *		    buff_ptr = *buffer;
	char *		    buffer2;
	int		    buffer2_size;

	DEBUG_ONLY(boolean_t piece_seen = FALSE);
	DEBUG_ONLY(boolean_t empty_delim_seen = FALSE);

	if (cur_column->is_hidden_keycol) {
		/* This is a hidden key column. Do not emit it in the text table definition as it can confuse the user.
		 * Only emit user specified columns in the text definition.
		 */
		return 0;
	}
	UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
	INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "`%s`", value->v.reference);
	switch (cur_column->data_type_struct.data_type) {
	case BOOLEAN_TYPE:
		/* For BOOLEAN, neither PRECISION nor SCALE apply. Assert that. */
		assert(SIZE_OR_PRECISION_UNSPECIFIED == cur_column->data_type_struct.size_or_precision);
		assert(SCALE_UNSPECIFIED == cur_column->data_type_struct.scale);
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " BOOLEAN");
		break;
	case INTEGER_TYPE:
		/* For INTEGER, only PRECISION may apply. Assert that. */
		assert(SCALE_UNSPECIFIED == cur_column->data_type_struct.scale);
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " INTEGER");
		if (SIZE_OR_PRECISION_UNSPECIFIED != cur_column->data_type_struct.size_or_precision) {
			/* SIZE was specified (e.g. INTEGER(8)). In that case, write out the "8" here */
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "(%d)",
								    cur_column->data_type_struct.size_or_precision);
		}
		break;
	case NUMERIC_TYPE:
		/* For NUMERIC, both PRECISION and SCALE may apply. Check both. */
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " NUMERIC");
		if (SIZE_OR_PRECISION_UNSPECIFIED != cur_column->data_type_struct.size_or_precision) {
			if (SCALE_UNSPECIFIED != cur_column->data_type_struct.scale) {
				/* PRECISION and SCALE were both specified (e.g. NUMERIC(8,4)).
				 * In that case, write out the "(8,4)" here.
				 */
				INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "(%d,%d)",
									    cur_column->data_type_struct.size_or_precision,
									    cur_column->data_type_struct.scale);
			} else {
				/* Only PRECISION was specified (e.g. NUMERIC(8)).
				 * In that case, write out the "(8)" here.
				 */
				INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "(%d)",
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
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " VARCHAR");
		if (SIZE_OR_PRECISION_UNSPECIFIED != cur_column->data_type_struct.size_or_precision) {
			/* SIZE was specified (e.g. VARCHAR(30)). In that case, write out the "30" here */
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "(%d)",
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
	buffer2_size = OCTO_INIT_BUFFER_LEN;
	buffer2 = (char *)malloc(sizeof(char) * buffer2_size);
	do {
		switch (cur_keyword->keyword) {
		case PRIMARY_KEY:
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " PRIMARY KEY");
			break;
		case NOT_NULL:
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " NOT NULL");
			break;
		case UNIQUE_CONSTRAINT:
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " UNIQUE");
			break;
		case OPTIONAL_EXTRACT:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(&buffer2, &buffer2_size, value->v.reference);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " EXTRACT \"%s\"", buffer2);
			break;
		case OPTIONAL_PIECE:
			DEBUG_ONLY(assert(!empty_delim_seen));
			DEBUG_ONLY(piece_seen = TRUE);
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(&buffer2, &buffer2_size, value->v.reference);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " PIECE %s", buffer2);
			break;
		case OPTIONAL_SOURCE:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(&buffer2, &buffer2_size, value->v.reference);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " GLOBAL \"%s\"", buffer2);
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
				m_escape_string2(&buffer2, &buffer2_size, delim);
				INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " DELIM \"%s\"",
									    buffer2);
			} else {
				assert(!MEMCMP_LIT(delim, "$CHAR(")); /* this is added in parser.y */
				delim += sizeof("$CHAR") - 1;	      /* Skip "$CHAR" */
				INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " DELIM %s", delim);
			}
			break;
		case OPTIONAL_KEY_NUM:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(&buffer2, &buffer2_size, value->v.reference);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " KEY NUM %s", buffer2);
			break;
		case NO_KEYWORD:
			break;
		case OPTIONAL_START:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(&buffer2, &buffer2_size, value->v.reference);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " START \"%s\"", buffer2);
			break;
		case OPTIONAL_STARTINCLUDE:
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " STARTINCLUDE");
			break;
		case OPTIONAL_END:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(&buffer2, &buffer2_size, value->v.reference);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " END \"%s\"", buffer2);
			break;
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			assert(FALSE);
			free(buffer2);
			return -1;
			break;
		}
		cur_keyword = cur_keyword->next;
	} while (cur_keyword != start_keyword);
	assert(buff_ptr - *buffer < *buffer_size);
	*buff_ptr = '\0';
	// We don't count the null character we added for ease of use
	free(buffer2);
	return buff_ptr - *buffer;
}
