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

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"
#include "template_strings.h"

#define SOURCE	 (1 << 0)
#define DELIM	 (1 << 1)
#define NULLCHAR (1 << 2)

#define EXPAND_BUFFER_IF_NEEDED(FMT_STR, ARG)                                                 \
	while ((buffer_size - (buff_ptr - buffer)) <= (total_copied + copied)) {              \
		char *tmp;                                                                    \
		int   new_buffer_size;                                                        \
                                                                                              \
		new_buffer_size = buffer_size * 2;                                            \
		tmp = (char *)malloc(sizeof(char) * new_buffer_size);                         \
		memcpy(tmp, buffer, total_copied);                                            \
		free(buffer);                                                                 \
		buffer = tmp;                                                                 \
		buffer_size = new_buffer_size;                                                \
		buff_ptr = buffer + total_copied;                                             \
		copied = snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), FMT_STR, ARG); \
	}

#define DQDEL_AND_CONTINUE(CUR_KEYWORD, START_KEYWORD, NEXT_KEYWORD) \
	{                                                            \
		dqdel(CUR_KEYWORD);                                  \
		if (START_KEYWORD == CUR_KEYWORD) {                  \
			START_KEYWORD = NEXT_KEYWORD;                \
		}                                                    \
		CUR_KEYWORD = NEXT_KEYWORD;                          \
		continue;                                            \
	}

/* 0 return value implies table create was successful
 * non-zero return value implies error while creating table
 */
int create_table_defaults(SqlStatement *table_statement, SqlStatement *keywords_statement) {
	SqlTable *	    table;
	SqlOptionalKeyword *keyword, *cur_keyword, *start_keyword, *t_keyword;
	SqlColumn *	    key_columns[MAX_KEY_COUNT], *cur_column, *start_column;
	SqlStatement *	    statement;
	SqlValue *	    value;
	char *		    out_buffer;
	size_t		    str_len;
	int		    max_key = 0, copied, i, len;
	unsigned int	    options = 0;

	assert(NULL != keywords_statement);

	UNPACK_SQL_STATEMENT(start_keyword, keywords_statement, keyword);
	UNPACK_SQL_STATEMENT(table, table_statement, create_table);

	/* Check for duplicate column names. If so issue error. */
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	for (cur_column = start_column; start_column != cur_column->next; cur_column = cur_column->next) {
		SqlColumn *cur_column2;
		SqlValue * columnName1;

		UNPACK_SQL_STATEMENT(columnName1, cur_column->columnName, value);
		for (cur_column2 = cur_column->next; start_column != cur_column2; cur_column2 = cur_column2->next) {
			SqlValue *columnName2;

			UNPACK_SQL_STATEMENT(columnName2, cur_column2->columnName, value);
			if (!strcmp(columnName1->v.string_literal, columnName2->v.string_literal)) {
				ERROR(ERR_DUPLICATE_COLUMN, columnName1->v.string_literal);
				return 1; // non-zero return value is an error (i.e causes YYABORT in caller)
			}
		}
	}
	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn *));
	max_key = get_key_columns(table, key_columns);
	/* max_key >= 0 is the number of key columns found
	 * -1 indicates no keys found to make all columns keys
	 * -2 indicates there was some error in the key columns
	 */
	if (max_key == -1) {
		char key_num_buffer[INT32_TO_STRING_MAX];

		cur_column = start_column;
		i = 0;
		do {
			// Construct the key num keyword
			SQL_STATEMENT(statement, keyword_STATEMENT);
			MALLOC_STATEMENT(statement, keyword, SqlOptionalKeyword);
			keyword = statement->v.keyword;
			keyword->keyword = OPTIONAL_KEY_NUM;
			// key num value is index of key in table
			copied = snprintf(key_num_buffer, INT32_TO_STRING_MAX, "%d", i);
			assert(INT32_TO_STRING_MAX > copied);
			UNUSED(copied); // Only used for asserts, so use macro to prevent compiler warnings in RelWithDebInfo builds
			len = strlen(key_num_buffer);
			out_buffer = octo_cmalloc(memory_chunks, len + 1);
			strncpy(out_buffer, key_num_buffer, len + 1);
			SQL_VALUE_STATEMENT(keyword->v, INTEGER_LITERAL, out_buffer);
			// Insert statement into column keyword list
			dqinit(keyword);
			UNPACK_SQL_STATEMENT(t_keyword, cur_column->keywords, keyword);
			dqappend(t_keyword, keyword);
			// Walk to next key and increment index
			cur_column = cur_column->next;
			i++;
		} while (cur_column != start_column);
		// Get the new key columns
		max_key = get_key_columns(table, key_columns);
		assert(max_key == i - 1);
	} else if (-2 == max_key) {
		return 1; // non-zero return value is an error (i.e causes YYABORT in caller)
	}
	cur_keyword = start_keyword;
	do {
		SqlOptionalKeyword *next_keyword;

		next_keyword = cur_keyword->next;
		switch (cur_keyword->keyword) {
		case OPTIONAL_SOURCE:
			assert(0 == (options & SOURCE));
			options |= SOURCE;
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->source = statement;
			DQDEL_AND_CONTINUE(cur_keyword, start_keyword, next_keyword);
			break;
		case OPTIONAL_DELIM:
			assert(0 == (options & DELIM));
			options |= DELIM;
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->delim = statement;
			DQDEL_AND_CONTINUE(cur_keyword, start_keyword, next_keyword);
			break;
		case OPTIONAL_NULLCHAR:
			assert(0 == (options & NULLCHAR));
			options |= NULLCHAR;
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->nullchar = statement;
			DQDEL_AND_CONTINUE(cur_keyword, start_keyword, next_keyword);
			break;
		case NO_KEYWORD:
			break;
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			assert(FALSE);
			return 1;
			break;
		}
		cur_keyword = next_keyword;
		if (cur_keyword == start_keyword) {
			break;
		}
	} while (TRUE);
	if ((SOURCE | DELIM) == options) {
		return 0;
	}
	if (!(options & SOURCE)) {
		int   total_copied = 0;
		int   buffer_size, buffer2_size;
		char *buffer, *buffer2, *buff_ptr;

		buffer_size = buffer2_size = OCTO_INIT_BUFFER_LEN;
		buffer = (char *)malloc(sizeof(char) * buffer_size);
		buffer2 = (char *)malloc(sizeof(char) * buffer2_size);
		UNPACK_SQL_STATEMENT(value, table->tableName, value);
		buff_ptr = buffer;
		copied = snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "^%s(", value->v.reference);
		EXPAND_BUFFER_IF_NEEDED("^%s(", value->v.reference);
		buff_ptr += copied;
		total_copied += copied;
		assert(buffer_size > total_copied);
		UNUSED(total_copied); // Only used for asserts, so use macro to prevent compiler warnings in RelWithDebInfo builds
		for (i = 0; i <= max_key; i++) {
			generate_key_name(&buffer2, &buffer2_size, i, table, key_columns);
			if (0 != i) {
				copied = snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), ",");
				EXPAND_BUFFER_IF_NEEDED("%s", ",");
				buff_ptr += copied;
				total_copied += copied;
				assert(buffer_size > total_copied);
			}
			copied = snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), "%s", buffer2);
			EXPAND_BUFFER_IF_NEEDED("%s", buffer2);
			buff_ptr += copied;
			total_copied += copied;
			assert(buffer_size > total_copied);
		}
		copied = snprintf(buff_ptr, buffer_size - (buff_ptr - buffer), ")");
		EXPAND_BUFFER_IF_NEEDED("%s", ")");
		buff_ptr += copied;
		total_copied += copied;
		assert(buffer_size > total_copied);
		UNUSED(total_copied); // Only used for asserts, so use macro to prevent compiler warnings in RelWithDebInfo builds
		*buff_ptr++ = '\0';
		len = buff_ptr - buffer;
		out_buffer = octo_cmalloc(memory_chunks, len);
		memcpy(out_buffer, buffer, len);
		OCTO_CMALLOC_STRUCT((keyword), SqlOptionalKeyword);
		(keyword)->keyword = OPTIONAL_SOURCE;
		SQL_VALUE_STATEMENT(keyword->v, STRING_LITERAL, out_buffer);
		dqinit(keyword);
		dqappend(start_keyword, keyword);
		free(buffer);
		free(buffer2);
	}
	if (!(options & DELIM)) {
		assert(2 == sizeof(COLUMN_DELIMITER));	// 2 includes null terminator
		str_len = sizeof(COLUMN_DELIMITER) + 1; // +1 for "is_dollar_char" flag
		out_buffer = octo_cmalloc(memory_chunks, str_len);
		out_buffer[0] = DELIM_IS_LITERAL;
		MEMCPY_LIT(&out_buffer[1], COLUMN_DELIMITER);
		out_buffer[str_len - 1] = '\0';
		OCTO_CMALLOC_STRUCT((keyword), SqlOptionalKeyword);
		(keyword)->keyword = OPTIONAL_DELIM;
		SQL_VALUE_STATEMENT(keyword->v, STRING_LITERAL, out_buffer);
		dqinit(keyword);
		dqappend(start_keyword, keyword);
	}
	cur_keyword = start_keyword;
	do {
		SqlOptionalKeyword *next_keyword;

		next_keyword = cur_keyword->next;
		switch (cur_keyword->keyword) {
		case OPTIONAL_SOURCE:
			if ((NULL != table->source) && (table->source->v.keyword == cur_keyword)) {
				break;
			}
			assert(NULL == table->source);
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->source = statement;
			DQDEL_AND_CONTINUE(cur_keyword, start_keyword, next_keyword);
			break;
		case OPTIONAL_DELIM:
			if ((NULL != table->delim) && (table->delim->v.keyword == cur_keyword)) {
				break;
			}
			assert(NULL == table->delim);
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->delim = statement;
			DQDEL_AND_CONTINUE(cur_keyword, start_keyword, next_keyword);
			break;
		case NO_KEYWORD:
			break;
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			assert(FALSE);
			return 1;
			break;
		}
		cur_keyword = next_keyword;
		if (cur_keyword == start_keyword) {
			break;
		}
	} while (TRUE);
	return 0;
}
