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
 * This mirrors the design of generate_routine_name function.
 */
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
	int		    max_key = 0, copied, i, len, piece_number;
	unsigned int	    options;
	boolean_t	    readwrite_disallowed; /* TRUE if READWRITE at table level is disallowed due to incompatible qualifier */

	UNPACK_SQL_STATEMENT(table, table_statement, create_table);

	readwrite_disallowed = FALSE; /* Start with FALSE . Will be set to TRUE later if we find an incompatibility. */
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	/* Do various column-level checks */
	cur_column = start_column;
	piece_number = 0;
	do {
		SqlColumn *	    cur_column2;
		SqlValue *	    columnName1;
		boolean_t	    is_key_column;
		SqlOptionalKeyword *piece_keyword;

		/* Check for duplicate column names. If so issue error. */
		UNPACK_SQL_STATEMENT(columnName1, cur_column->columnName, value);
		for (cur_column2 = cur_column->next; start_column != cur_column2; cur_column2 = cur_column2->next) {
			SqlValue *columnName2;

			UNPACK_SQL_STATEMENT(columnName2, cur_column2->columnName, value);
			if (!strcmp(columnName1->v.string_literal, columnName2->v.string_literal)) {
				ERROR(ERR_DUPLICATE_COLUMN, columnName1->v.string_literal);
				return 1; // non-zero return value is an error (i.e causes YYABORT in caller)
			}
		}
		/* Check if there are any incompatible qualifier specifications */
		UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
		cur_keyword = start_keyword;
		is_key_column = FALSE;
		piece_keyword = NULL;
		do {
			switch (cur_keyword->keyword) {
			case PRIMARY_KEY:
			case OPTIONAL_KEY_NUM:
				/* These column-level keywords are compatible with table-level keyword READONLY or READWRITE */
				is_key_column = TRUE;
				break;
			case NOT_NULL:
			case UNIQUE_CONSTRAINT:
				/* These column-level keywords are compatible with table-level keyword READONLY or READWRITE */
				break;
			case OPTIONAL_EXTRACT:
			case OPTIONAL_SOURCE:
			case OPTIONAL_DELIM:
			case OPTIONAL_START:
			case OPTIONAL_STARTINCLUDE:
			case OPTIONAL_END:
				readwrite_disallowed = TRUE;
				break;
			case OPTIONAL_PIECE:
				piece_keyword = cur_keyword;
				break;
			case NO_KEYWORD:
				break;
			default:
				ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
				assert(FALSE);
				return 1;
				break;
			}
			cur_keyword = cur_keyword->next;
		} while (cur_keyword != start_keyword);
		if (!is_key_column) {
			/* If it is not a key column, treat it as a column with a PIECE number
			 * (explicitly specified or implicitly assumed).
			 */
			SqlValue *lcl_value;

			piece_number++;
			if (NULL != piece_keyword) {
				/* Disallow if PIECE number (explicit or implicit) is not in order */
				UNPACK_SQL_STATEMENT(lcl_value, piece_keyword->v, value);
				assert(INTEGER_LITERAL == lcl_value->type);
				if (!readwrite_disallowed) {
					readwrite_disallowed = (piece_number != atoi(lcl_value->v.string_literal));
				}
			}
		}
		cur_column = cur_column->next;
	} while (cur_column != start_column);
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
			UNUSED(copied); // Only used for asserts, so use macro to prevent compiler warnings in
					// RelWithDebInfo builds
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
	assert(NULL != keywords_statement);
	UNPACK_SQL_STATEMENT(start_keyword, keywords_statement, keyword);
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
	/* Now that all table level keywords have been seen, check for incompatibilities. */
	/* 1) If table-level GLOBAL keyword is NOT specified, compute the keyword value.
	 * 2) If table-level GLOBAL keyword is specified and we don't yet know that READWRITE is disallowed in this table,
	 * check if the GLOBAL keyword value can cause an incompatibility. For this check if the specified GLOBAL keyword is
	 * an M global name following by the primary key column(s) as subscripts in the KEY NUM order. If so it is
	 * compatible with READWRITE. Otherwise it is not. 3) If table-level GLOBAL keyword is specified and we already know
	 * that READWRITE is disallowed in this table due to other incompatibilities, no need to do any checks of the
	 * specified GLOBAL.
	 */
	if (!(options & SOURCE) || !readwrite_disallowed) {
		int	   total_copied;
		int	   buffer_size, buffer2_size;
		char *	   buffer, *buffer2, *buff_ptr;
		ydb_uint16 mmr_table_hash;
		char	   table_hash[OCTO_HASH_LEN + 1]; // +1 for null terminator
		char *	   start, *next;

		buffer_size = buffer2_size = OCTO_INIT_BUFFER_LEN;
		buffer = (char *)malloc(sizeof(char) * buffer_size);
		buffer2 = (char *)malloc(sizeof(char) * buffer2_size);
		buff_ptr = buffer;
		total_copied = 0;
		if (!(options & SOURCE)) {
			UNPACK_SQL_STATEMENT(value, table->tableName, value);

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
		} else {
			/* Copy just the subscripts portion of the M gvn specified in GLOBAL keyword for later "strcasecmp"
			 * check */
			UNPACK_SQL_STATEMENT(keyword, table->source, keyword);
			UNPACK_SQL_STATEMENT(value, keyword->v, value);
			start = value->v.string_literal;
			next = strchr(start, '(');
		}
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
		if (!(options & SOURCE)) {
			out_buffer = octo_cmalloc(memory_chunks, len);
			memcpy(out_buffer, buffer, len);
			OCTO_CMALLOC_STRUCT(keyword, SqlOptionalKeyword);
			keyword->keyword = OPTIONAL_SOURCE;
			SQL_VALUE_STATEMENT(keyword->v, STRING_LITERAL, out_buffer);
			dqinit(keyword);
			assert(NULL == table->source);
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = keyword;
			table->source = statement;
		} else if ((NULL == next) || strcasecmp(buffer, next)) {
			/* GLOBAL keyword value did not specify any subscripts OR has specified subscripts that is not in
			 * a format compatible with READWRITE.
			 * Note: Need to use "starcasecmp" since the user might specify column name in "keys(...)" syntax
			 * in any case but "generate_key_name" would have used upper case and they should be treated as the
			 * same. Since the case matters in the global name, we use "next" (instead of "start") and avoid the
			 * global name in the string compare call.
			 */
			readwrite_disallowed = TRUE;
		}
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
	if ((options & READWRITE) && readwrite_disallowed) {
		/* READWRITE has been explicitly specified but at least one incompatible qualifier was found. Issue error.
		 */
		ERROR(ERR_READWRITE_DISALLOWED, NULL);
		return 1;
	}
	if (!(options & READONLY) && !(options & READWRITE)) {
		if (config->in_auto_upgrade_binary_table_definition) {
			/* In auto upgrade logic where pre-existing tables are being upgraded. In this case, it is not safe
			 * to infer the READONLY vs READWRITE characteristic of a table from the current octo.conf setting
			 * of "tabletype" since the user of the new Octo build might not have yet known this new keyword
			 * (let alone set this keyword in octo.conf appropriately). Therefore, check if READWRITE is
			 * allowable (i.e. no incompatible column level keywords have been specified). If so use that. If
			 * not use READONLY.
			 */
			table->readwrite = !readwrite_disallowed;
		} else {
			/* Neither READONLY or READWRITE was specified.
			 * If incompatible column level keyword has been specified, assume READONLY.
			 * If not, assume value based on octo.conf "tabletype" setting.
			 */
			table->readwrite
			    = (readwrite_disallowed ? FALSE : ((TABLETYPE_READWRITE == config->default_tabletype) ? TRUE : FALSE));
		}
	}
	return 0;
}
