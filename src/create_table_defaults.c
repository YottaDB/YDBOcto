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
#define READONLY  (1 << 2)
#define READWRITE (1 << 3)

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

/* Delete CUR_KEYWORD from the doubly linked list starting at START_KEYWORD.
 * If CUR_KEYWORD happens to be the first keyword in the linked list, then START_KEYWORD and KEYWORDS_STMT->v.keyword
 * are both updated to point to the next element (guaranteed by an assert below) in the linked list.
 */
#define DQDEL(CUR_KEYWORD, START_KEYWORD, KEYWORDS_STMT)                                                   \
	{                                                                                                  \
		if (START_KEYWORD == CUR_KEYWORD) {                                                        \
			/* Keyword to be deleted is first in list. Update start pointer of linked list. */ \
			START_KEYWORD = CUR_KEYWORD->next;                                                 \
			/* Assert that DELIM is not the only keyword in the column keyword list.           \
			 * Even if nothing else is there, we would have a NO_KEYWORD keyword in the list.  \
			 * This way we are guaranteed "START_KEYWORD" will not point to a deleted pointer. \
			 */                                                                                \
			if (START_KEYWORD == CUR_KEYWORD) {                                                \
				/* The only keyword in the linked list is going to be deleted.             \
				 * Reset the start of linked list to NULL.                                 \
				 */                                                                        \
				START_KEYWORD = NULL;                                                      \
			}                                                                                  \
			KEYWORDS_STMT->v.keyword = START_KEYWORD;                                          \
		}                                                                                          \
		dqdel(CUR_KEYWORD); /* Delete keyword */                                                   \
	}

/* 0 return value implies table create was successful
 * non-zero return value implies error while creating table
 */
int create_table_defaults(SqlStatement *table_statement, SqlStatement *keywords_statement) {
	SqlTable *	    table;
	SqlOptionalKeyword *keyword, *cur_keyword, *start_keyword;
	SqlColumn *	    cur_column, *start_column, *first_non_key_column;
	SqlStatement *	    statement;
	SqlValue *	    value;
	char *		    out_buffer;
	size_t		    str_len;
	int		    len, piece_number, num_non_key_columns;
	unsigned int	    options;
	boolean_t	    readwrite_disallowed; /* TRUE if READWRITE at table level is disallowed due to incompatible qualifier */

	UNPACK_SQL_STATEMENT(table, table_statement, create_table);
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	readwrite_disallowed = FALSE; /* Start with FALSE . Will be set to TRUE later if we find an incompatibility. */
	/* Do various column-level checks */
	cur_column = start_column;
	piece_number = 0;
	num_non_key_columns = 0;
	first_non_key_column = NULL;
	do {
		SqlColumn *	    cur_column2;
		SqlValue *	    columnName1;
		boolean_t	    is_key_column;
		SqlOptionalKeyword *piece_keyword, *delim_keyword;

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
		delim_keyword = NULL;
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
			case OPTIONAL_START:
			case OPTIONAL_STARTINCLUDE:
			case OPTIONAL_END:
				readwrite_disallowed = TRUE;
				break;
			case OPTIONAL_DELIM:
				/* If column-level DELIM is specified, we treat it as compatible with READWRITE if the delimiter
				 * is "" and this is the only non-key column in this table. Otherwise, READWRITE is not compatible.
				 */
				delim_keyword = cur_keyword;
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
			 * (explicitly specified or implicitly assumed). The only exception is if DELIM ""
			 * is also specified in the column. If so delete the PIECE keyword as it is ignored.
			 */
			SqlValue *lcl_value;

			if (NULL != delim_keyword) {
				boolean_t delim_is_empty;
				char *	  delim, ch;

				UNPACK_SQL_STATEMENT(lcl_value, delim_keyword->v, value);
				delim = lcl_value->v.reference;
				ch = *delim;
				assert((DELIM_IS_DOLLAR_CHAR == ch) || (DELIM_IS_LITERAL == ch));
				if (DELIM_IS_LITERAL == ch) {
					delim++; /* skip first byte to get actual delimiter */
					ch = *delim;
					delim_is_empty = ('\0' == ch);
				} else {
					delim_is_empty = FALSE;
				}
				if (!readwrite_disallowed) {
					/* A DELIM keyword was specified in the first non-key column. Check if it is "".
					 * If so, it is compatible with READWRITE. Otherwise, it is not compatible.
					 */
					readwrite_disallowed = (!delim_is_empty);
				}
				if (delim_is_empty && (NULL != piece_keyword)) {
					/* Delete the PIECE keyword (anyways going to be ignored) */
					DQDEL(piece_keyword, start_keyword, cur_column->keywords);
					piece_keyword = NULL;
				}
			}
			if (0 == num_non_key_columns) {
				first_non_key_column = cur_column; /* Note down the first non-key column for later checks */
			}
			num_non_key_columns++;
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
	if (1 == num_non_key_columns) {
		/* Only one non-key column in the table. If so, check if column-level PIECE number (if any specified) is 1
		 * and if column-level GLOBAL keyword is not specified.  If so remove any column-level DELIM (if specified)
		 * and instead add a new column-level DELIM "" keyword. This will ensure no $PIECE gets generated in physical
		 * plan to extract this column (small optimization).
		 */
		SqlOptionalKeyword *global_keyword;

		UNPACK_SQL_STATEMENT(start_keyword, first_non_key_column->keywords, keyword);
		global_keyword = get_keyword_from_keywords(start_keyword, OPTIONAL_SOURCE);
		if (NULL == global_keyword) {
			boolean_t	    ok_to_set_empty_delim;
			SqlOptionalKeyword *piece_keyword;

			piece_keyword = get_keyword_from_keywords(start_keyword, OPTIONAL_PIECE);
			if (NULL != piece_keyword) {
				SqlValue *lcl_value;

				/* Disallow if PIECE number (explicit or implicit) is not in order */
				UNPACK_SQL_STATEMENT(lcl_value, piece_keyword->v, value);
				assert(INTEGER_LITERAL == lcl_value->type);
				ok_to_set_empty_delim = (1 == atoi(lcl_value->v.string_literal));
			} else {
				ok_to_set_empty_delim = TRUE;
			}
			if (ok_to_set_empty_delim) {
				SqlOptionalKeyword *delim_keyword;

				delim_keyword = get_keyword_from_keywords(start_keyword, OPTIONAL_DELIM);
				if (NULL != delim_keyword) {
					DQDEL(delim_keyword, start_keyword, first_non_key_column->keywords);
				}
				if (NULL != piece_keyword) {
					DQDEL(piece_keyword, start_keyword, first_non_key_column->keywords);
				}
				/* Add DELIM "" keyword */
				OCTO_CMALLOC_STRUCT(delim_keyword, SqlOptionalKeyword);
				delim_keyword->keyword = OPTIONAL_DELIM;
				str_len = sizeof(EMPTY_DELIMITER) + 1; // + 1 for "is_dollar_char" flag
				assert(2 == str_len);
				out_buffer = octo_cmalloc(memory_chunks, str_len);
				out_buffer[0] = DELIM_IS_LITERAL;
				out_buffer[str_len - 1] = '\0';
				SQL_VALUE_STATEMENT(delim_keyword->v, STRING_LITERAL, out_buffer);
				dqinit(delim_keyword);
				if (NULL != start_keyword) {
					dqappend(start_keyword, delim_keyword);
				} else {
					start_keyword = delim_keyword;
					first_non_key_column->keywords->v.keyword = start_keyword;
				}
			}
		}
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
		int	   i, max_key, total_copied;
		int	   buffer_size, buffer2_size;
		char *	   buffer, *buffer2, *buff_ptr;
		ydb_uint16 mmr_table_hash;
		char	   table_hash[OCTO_HASH_LEN + 1]; // +1 for null terminator
		char *	   start, *next;
		SqlColumn *key_columns[MAX_KEY_COUNT];

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
			next = NULL; /* to avoid false [-Wmaybe-uninitialized] warnings from compiler */
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
		memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn *));
		max_key = get_key_columns(table, key_columns);
		assert(0 <= max_key); /* Prior call to "add_key_num_keyword_if_needed()" should ensure this assert */
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
			 * Note: Need to use "strcasecmp" since the user might specify column name in "keys(...)" syntax
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
		OCTO_CMALLOC_STRUCT(keyword, SqlOptionalKeyword);
		keyword->keyword = OPTIONAL_DELIM;
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
