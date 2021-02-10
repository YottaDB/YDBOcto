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

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"
#include "template_strings.h"

#define SOURCE	  (1 << 0)
#define DELIM	  (1 << 1)
#define NULLCHAR  (1 << 2)
#define READONLY  (1 << 3)
#define READWRITE (1 << 4)

/* The size of this must match $ZYSUFFIX exactly, which is 22.
 * This mirrors the design of generate_routine_name function. */
#define OCTO_TABLE_GLOBAL_PREFIX "%ydboctoD"
#define OCTO_TABLE_PREFIX_LEN	 (sizeof(OCTO_TABLE_GLOBAL_PREFIX) - 1)
#define OCTO_HASH_LEN		 (YDB_MAX_IDENT - OCTO_TABLE_PREFIX_LEN)

/* We SNPRINTF and resize buffers multiple times, so this is a convenience
 * macro that contains all the necessary operations */
#define SNPRINTF_TO_BUFFER(ARG, BUFFER, BUFF_PTR, BUFFER_SIZE, TOTAL_COPIED)                       \
	{                                                                                          \
		int copied;                                                                        \
                                                                                                   \
		/* main snprintf */                                                                \
		copied = snprintf(BUFF_PTR, BUFFER_SIZE - (BUFF_PTR - BUFFER), "%s", ARG);         \
                                                                                                   \
		/* resize buffer if needed */                                                      \
		while ((BUFFER_SIZE - (BUFF_PTR - BUFFER)) <= (TOTAL_COPIED + copied)) {           \
			char *tmp;                                                                 \
			int   new_buffer_size;                                                     \
                                                                                                   \
			new_buffer_size = BUFFER_SIZE * 2;                                         \
			tmp = (char *)malloc(sizeof(char) * new_buffer_size);                      \
			memcpy(tmp, BUFFER, TOTAL_COPIED);                                         \
			free(BUFFER);                                                              \
			BUFFER = tmp;                                                              \
			BUFFER_SIZE = new_buffer_size;                                             \
			BUFF_PTR = BUFFER + TOTAL_COPIED;                                          \
			copied = snprintf(BUFF_PTR, BUFFER_SIZE - (BUFF_PTR - BUFFER), "%s", ARG); \
		}                                                                                  \
                                                                                                   \
		/* Advance buffer pointer */                                                       \
		BUFF_PTR += copied;                                                                \
		TOTAL_COPIED += copied;                                                            \
		assert(BUFFER_SIZE > TOTAL_COPIED);                                                \
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
	unsigned int	    options;

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
	options = 0;
	cur_keyword = start_keyword;
	do {
		SqlOptionalKeyword *next_keyword;

		next_keyword = cur_keyword->next;
		switch (cur_keyword->keyword) {
		case OPTIONAL_SOURCE:
			options |= SOURCE;
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->source = statement;
			break;
		case OPTIONAL_DELIM:
			options |= DELIM;
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->delim = statement;
			break;
		case OPTIONAL_NULLCHAR:
			options |= NULLCHAR;
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->nullchar = statement;
			break;
		case OPTIONAL_READONLY:
			options |= READONLY;
			table->readwrite = FALSE;
			break;
		case OPTIONAL_READWRITE:
			options |= READWRITE;
			table->readwrite = TRUE;
			break;
		case NO_KEYWORD:
			break;
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			assert(FALSE);
			return 1;
			break;
		}
		dqdel(cur_keyword);
		if (cur_keyword != next_keyword) {
			if (start_keyword == cur_keyword) {
				start_keyword = next_keyword;
			}
			cur_keyword = next_keyword;
			continue;
		}
		assert(cur_keyword == start_keyword);
		break;
	} while (TRUE);
	if (!(options & SOURCE)) {
		int	   total_copied = 0;
		int	   buffer_size, buffer2_size;
		char *	   buffer, *buffer2, *buff_ptr;
		ydb_uint16 mmr_table_hash;
		char	   table_hash[OCTO_HASH_LEN + 1]; // +1 for null terminator

		buffer_size = buffer2_size = OCTO_INIT_BUFFER_LEN;
		buffer = (char *)malloc(sizeof(char) * buffer_size);
		buffer2 = (char *)malloc(sizeof(char) * buffer2_size);
		UNPACK_SQL_STATEMENT(value, table->tableName, value);
		buff_ptr = buffer;

		// Hashed table name (which matches $ZYSUFFIX)
		ydb_mmrhash_128(value->v.reference, strlen(value->v.reference), 0, &mmr_table_hash);
		ydb_hash_to_string(&mmr_table_hash, table_hash, OCTO_HASH_LEN);
		table_hash[OCTO_HASH_LEN] = '\0';
		assert(strlen(table_hash) == 22);

		/* Add ^OCTO_TABLE_GLOBAL_PREFIX to table name, then add table hash to table
		 * name after prefix and then open paren. */
		SNPRINTF_TO_BUFFER("^", buffer, buff_ptr, buffer_size, total_copied);
		SNPRINTF_TO_BUFFER(OCTO_TABLE_GLOBAL_PREFIX, buffer, buff_ptr, buffer_size, total_copied);
		SNPRINTF_TO_BUFFER(table_hash, buffer, buff_ptr, buffer_size, total_copied);
		SNPRINTF_TO_BUFFER("(", buffer, buff_ptr, buffer_size, total_copied);

		// Append keys (aka subscripts)
		for (i = 0; i <= max_key; i++) {
			generate_key_name(&buffer2, &buffer2_size, i, table, key_columns);
			// Add a comma if not first key
			if (0 != i) {
				SNPRINTF_TO_BUFFER(",", buffer, buff_ptr, buffer_size, total_copied);
			}
			// Add key
			SNPRINTF_TO_BUFFER(buffer2, buffer, buff_ptr, buffer_size, total_copied);
		}
		// Add closing parentheses
		SNPRINTF_TO_BUFFER(")", buffer, buff_ptr, buffer_size, total_copied);

		// Null terminate, and prepare to return
		*buff_ptr++ = '\0';
		len = buff_ptr - buffer;
		out_buffer = octo_cmalloc(memory_chunks, len);
		memcpy(out_buffer, buffer, len);
		OCTO_CMALLOC_STRUCT((keyword), SqlOptionalKeyword);
		(keyword)->keyword = OPTIONAL_SOURCE;
		SQL_VALUE_STATEMENT(keyword->v, STRING_LITERAL, out_buffer);
		dqinit(keyword);
		assert(NULL == table->source);
		SQL_STATEMENT(statement, keyword_STATEMENT);
		statement->v.keyword = keyword;
		table->source = statement;
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
		assert(NULL == table->delim);
		SQL_STATEMENT(statement, keyword_STATEMENT);
		statement->v.keyword = keyword;
		table->delim = statement;
	}
	if (!(options & READONLY) && !(options & READWRITE)) {
		if (config->in_auto_upgrade_binary_table_definition) {
			/* In auto upgrade logic where pre-existing tables are being upgraded. In this case, it is not safe
			 * to infer the READONLY vs READWRITE characteristic of a table from the current octo.conf setting
			 * of "tabletype" since the user of the new Octo build might not have yet known this new keyword
			 * (let alone set this keyword in octo.conf appropriately). Therefore, to be safe, assume it is a
			 * READONLY table.
			 */
			table->readwrite = FALSE;
		} else {
			/* Neither READONLY or READWRITE was specified. Assume default based on octo.conf tabletype setting. */
			table->readwrite = ((TABLETYPE_READWRITE == config->default_tabletype) ? TRUE : FALSE);
		}
	}
	return 0;
}
