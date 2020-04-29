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

#define SOURCE (1 << 0)
#define DELIM (1 << 4)

/* 0 return value implies table create was successful
 * non-zero return value implies error while creating table
 */
int create_table_defaults(SqlStatement *table_statement, SqlStatement *keywords_statement) {
	SqlTable		*table;
	SqlOptionalKeyword	*keyword, *cur_keyword, *start_keyword, *t_keyword;
	SqlColumn		*key_columns[MAX_KEY_COUNT], *cur_column, *start_column;
	SqlStatement		*statement;
	SqlValue		*value;
	char			buffer[MAX_STR_CONST], buffer2[MAX_STR_CONST], *out_buffer;
	char			*buff_ptr;
	size_t			str_len;
	int			max_key = 0, i, len;
	unsigned int		options = 0;

	assert(NULL != keywords_statement);

	UNPACK_SQL_STATEMENT(start_keyword, keywords_statement, keyword);
	UNPACK_SQL_STATEMENT(table, table_statement, create_table);

	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn*));
	max_key = get_key_columns(table, key_columns);
	/* max_key >= 0 is the number of key columns found
	 * -1 indicates no keys found to make all columns keys
	 * -2 inidicates there was some error in the key columns
	 */
	if (max_key == -1) {
		UNPACK_SQL_STATEMENT(start_column, table->columns, column);
		cur_column = start_column;
		i = 0;
		do {
			// Construct the key num keyword
			SQL_STATEMENT(statement, keyword_STATEMENT);
			MALLOC_STATEMENT(statement, keyword, SqlOptionalKeyword);
			keyword = statement->v.keyword;
			keyword->keyword = OPTIONAL_KEY_NUM;
			// key num value is index of key in table
			snprintf(buffer, sizeof(buffer), "%d", i);
			len = strlen(buffer);
			out_buffer = octo_cmalloc(memory_chunks, len+1);
			strncpy(out_buffer, buffer, len+1);
			SQL_STATEMENT(keyword->v, value_STATEMENT);
			MALLOC_STATEMENT(keyword->v, value, SqlValue);
			keyword->v->v.value->type = NUMERIC_LITERAL;
			keyword->v->v.value->v.string_literal = out_buffer;
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
		switch(cur_keyword->keyword) {
		case OPTIONAL_SOURCE:
			assert(0 == (options & SOURCE));
			options |= SOURCE;
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->source = statement;
			break;
		case OPTIONAL_DELIM:
			assert(0 == (options & DELIM));
			options |= DELIM;
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->delim = statement;
			break;
		case NO_KEYWORD:
			break;
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			return 1;
			break;
		}
		cur_keyword = cur_keyword->next;
	} while (cur_keyword != start_keyword);
	if ((SOURCE | DELIM) == options)
		return 0;
	if (!(options & SOURCE)) {
		UNPACK_SQL_STATEMENT(value, table->tableName, value);
		buff_ptr = buffer;
		buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), "^%s(",
				     value->v.reference);
		for(i = 0; i <= max_key; i++) {
			generate_key_name(buffer2, MAX_STR_CONST, i, table, key_columns);
			if (0 != i)
				buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), ",");
			buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), "%s", buffer2);
		}
		buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), ")");
		*buff_ptr++ = '\0';
		len = buff_ptr - buffer;
		out_buffer = octo_cmalloc(memory_chunks, len);
		memcpy(out_buffer, buffer, len);
		OCTO_CMALLOC_STRUCT((keyword), SqlOptionalKeyword);
		(keyword)->keyword = OPTIONAL_SOURCE;
		SQL_STATEMENT(keyword->v, value_STATEMENT);
		OCTO_CMALLOC_STRUCT(keyword->v->v.value, SqlValue);
		keyword->v->v.value->type = COLUMN_REFERENCE;
		keyword->v->v.value->v.reference = out_buffer;
		dqinit(keyword);
		dqappend(start_keyword, keyword);
	}
	if (!(options & DELIM)) {
		assert(2 == sizeof(COLUMN_DELIMITER));			// 2 includes null terminator
		str_len = sizeof(COLUMN_DELIMITER) + 1;			// +1 for "is_dollar_char" flag
		out_buffer = octo_cmalloc(memory_chunks, str_len);
		out_buffer[0] = DELIM_IS_LITERAL;
		MEMCPY_LIT(&out_buffer[1], COLUMN_DELIMITER);
		out_buffer[str_len-1] = '\0';
		OCTO_CMALLOC_STRUCT((keyword), SqlOptionalKeyword);
		(keyword)->keyword = OPTIONAL_DELIM;
		SQL_STATEMENT(keyword->v, value_STATEMENT);
		OCTO_CMALLOC_STRUCT(keyword->v->v.value, SqlValue);
		keyword->v->v.value->type = STRING_LITERAL;
		keyword->v->v.value->v.reference = out_buffer;
		dqinit(keyword);
		dqappend(start_keyword, keyword);
	}
	cur_keyword = start_keyword;
	do {
		switch(cur_keyword->keyword) {
		case OPTIONAL_SOURCE:
			if ((NULL != table->source) && (table->source->v.keyword == cur_keyword)) {
				break;
			}
			assert(NULL == table->source);
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->source = statement;
			break;
		case OPTIONAL_DELIM:
			if ((NULL != table->delim) && (table->delim->v.keyword == cur_keyword)) {
				break;
			}
			assert(NULL == table->delim);
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->delim = statement;
			break;
		case NO_KEYWORD:
			break;
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			return 1;
			break;
		}
		cur_keyword = cur_keyword->next;
	} while (cur_keyword != start_keyword);
	return 0;
}
