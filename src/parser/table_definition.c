/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"

#define SOURCE	  (1 << 0)
#define DELIM	  (1 << 1)
#define READONLY  (1 << 2)
#define READWRITE (1 << 3)

/* The size of this must match $ZYSUFFIX exactly, which is 22.
 * This mirrors the design of generate_routine_name function.
 */
#define OCTO_HASH_LEN (YDB_MAX_IDENT - LIT_LEN(TABLE_GLOBAL_NAME_PREFIX))

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

/* Function invoked by the rule named "table_definition" in src/parser.y (parses the CREATE TABLE command).
 * Returns
 *	non-NULL pointer to SqlStatement structure on success
 *	NULL on failure
 */
SqlStatement *table_definition(SqlStatement *tableName, SqlStatement *table_element_list, SqlStatement *table_definition_tail,
			       boolean_t if_not_exists_specified) {
	SqlStatement *	    table_stmt;
	SqlTable *	    table;
	SqlColumn *	    key_columns[MAX_KEY_COUNT];
	int		    max_key;
	int		    column_number;
	SqlOptionalKeyword *cur_keyword, *start_keyword;
	SqlColumn *	    cur_column, *start_column, *first_non_key_column;
	SqlStatement *	    statement;
	SqlValue *	    value;
	char *		    out_buffer;
	size_t		    str_len;
	int		    len, piece_number, num_non_key_columns;
	unsigned int	    options;
	boolean_t	    readwrite_disallowed; /* TRUE if READWRITE at table level is disallowed due to incompatible qualifier */
	tabletype_t	    table_type;
	boolean_t	    hidden_column_added;
	char *		    table_source_gvname; /* Points to a null-terminated string containing the unsubscripted global
						  * name specified in the GLOBAL keyword (if one was specified by the user).
						  * If a subscripted global name is specified in the GLOBAL keyword or no
						  * GLOBAL keyword was specified, this is NULL.
						  */

	SQL_STATEMENT(table_stmt, create_table_STATEMENT);
	MALLOC_STATEMENT(table_stmt, create_table, SqlTable);
	assert((value_STATEMENT == tableName->type) && (COLUMN_REFERENCE == tableName->v.value->type));
	UNPACK_SQL_STATEMENT(table, table_stmt, create_table);
	table->tableName = tableName;
	table->columns = table_element_list;
	table->if_not_exists_specified = if_not_exists_specified;
	/* Determine whether table is READWRITE or READONLY. Use default setting from octo.conf.
	 * Override this later based on whether READWRITE or READONLY keywords have been specified in the CREATE TABLE.
	 */
	table_type = config->default_tabletype;
	/*********************************************************************************************************************
	 * First process table-level keywords and set "options" bitmask variable accordingly.
	 * Also update "table_type" based on whether READONLY or READWRITE keywords were specified.
	 *********************************************************************************************************************
	 */
	options = 0;
	table_source_gvname = NULL;
	assert(NULL != table_definition_tail);
	UNPACK_SQL_STATEMENT(start_keyword, table_definition_tail, keyword);
	cur_keyword = start_keyword;
	do {
		SqlOptionalKeyword *next_keyword;

		next_keyword = cur_keyword->next;
		switch (cur_keyword->keyword) {
		case OPTIONAL_SOURCE: {
			char *start, *next;

			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			start = value->v.string_literal;
			next = strchr(start, '(');
			if (NULL == next) {
				/* User specified an unsubscripted global name in the GLOBAL keyword. Note down the name
				 * but otherwise consider the GLOBAL keyword as not specified by the user. This will help
				 * later logic autogenerate the GLOBAL keyword with the proper subscripts (primary key
				 * columns substituted).
				 */
				table_source_gvname = start;
				options &= ~SOURCE; /* Forget GLOBAL keyword(s) specified prior to this GLOBAL keyword */
			} else {
				options |= SOURCE;
				SQL_STATEMENT(statement, keyword_STATEMENT);
				statement->v.keyword = cur_keyword;
				table->source = statement;
				table_source_gvname = NULL;
			}
			break;
		}
		case OPTIONAL_DELIM:
			options |= DELIM;
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->delim = statement;
			break;
		case OPTIONAL_READONLY:
			options &= ~READWRITE; /* Clear any prior READWRITE keyword specifications in same command */
			options |= READONLY;
			table_type = TABLETYPE_READONLY;
			break;
		case OPTIONAL_READWRITE:
			options &= ~READONLY; /* Clear any prior READONLY keyword specifications in same command */
			options |= READWRITE;
			table_type = TABLETYPE_READWRITE;
			break;
		case NO_KEYWORD:
			break;
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			assert(FALSE);
			return NULL;
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
	/*********************************************************************************************************************
	 * Determine the key columns in the table (i.e. those that have "PRIMARY KEY" or "KEY NUM" keyword specified.
	 * At the end of the "get_key_columns()" call below, "max_key" will contain a value
	 *  0    if 1 key column was found
	 *  1    if 2 key columns were found
	 *  N    if N+1 key columns were found where N is a positive number >= 0
	 * -1    if no key columns were found.
	 * -2    if there was some error during this determination.
	 *********************************************************************************************************************
	 */
	hidden_column_added = FALSE;
	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn *));
	max_key = get_key_columns(table, key_columns);
	switch (max_key) {
	case -1:
		/*************************************************************************************************************
		 * This means no column is specified as a key column in the table (i.e. no "PRIMARY KEY" or "KEY NUM" keyword
		 * in any column). There are 2 cases to consider.
		 *
		 * If the table is READWRITE, we will create a hidden key column. This will let us allow duplicate rows to be
		 * inserted into the table using the hidden key column to serve as a unique subscript in the mapped M global.
		 *
		 * But if the table is READONLY, we have to map to a pre-existing M global and so cannot create an additional
		 * column (would require structural changes to the existing M global to add the hidden key column subscript).
		 * Therefore in that case, we treat all columns in the table as key columns i.e. effectively treating all
		 * columns as one big composite key.
		 ************************************************************************************************************
		 */
		if ((options & SOURCE) && !(options & READWRITE) && !(options & READONLY)) {
			/* Table-level GLOBAL keyword specified but neither READWRITE nor READONLY specified.
			 * The GLOBAL keyword specifies a subscripted global name (this is because if an unsubscripted
			 * global name was specified, "options & SOURCE" would be FALSE and we would not have come down
			 * the "if" condition above). In that case, READWRITE is incompatible so set table_type to be
			 * READONLY (in case it was set to READWRITE from the default octo.conf setting).
			 */
			table_type = TABLETYPE_READONLY;
		}
		if (TABLETYPE_READWRITE == table_type) {
			SqlStatement *colname_stmt, *data_type_stmt, *keycol_stmt, *keyword_stmt;

			SQL_STATEMENT(colname_stmt, value_STATEMENT);
			OCTO_CMALLOC_STRUCT(colname_stmt->v.value, SqlValue);
			colname_stmt->v.value->type = COLUMN_REFERENCE;
			/* Note: sizeof of a string literal will include null terminator so that too gets copied in memcpy below */
			colname_stmt->v.value->v.string_literal = octo_cmalloc(memory_chunks, sizeof(HIDDEN_KEY_COL_NAME));
			memcpy(colname_stmt->v.value->v.string_literal, HIDDEN_KEY_COL_NAME, sizeof(HIDDEN_KEY_COL_NAME));

			SQL_STATEMENT(keyword_stmt, keyword_STATEMENT);
			MALLOC_STATEMENT(keyword_stmt, keyword, SqlOptionalKeyword);
			keyword_stmt->v.keyword->keyword = PRIMARY_KEY;
			dqinit(keyword_stmt->v.keyword);

			data_type_stmt = data_type(INTEGER_TYPE, NULL, NULL);

			SQL_STATEMENT(keycol_stmt, column_STATEMENT);
			MALLOC_STATEMENT(keycol_stmt, column, SqlColumn);
			dqinit(keycol_stmt->v.column);
			keycol_stmt->v.column->columnName = colname_stmt;
			keycol_stmt->v.column->data_type_struct = data_type_stmt->v.data_type_struct;
			keycol_stmt->v.column->keywords = keyword_stmt;
			keycol_stmt->v.column->delim = NULL;
			keycol_stmt->v.column->is_hidden_keycol = TRUE;
			/* Add the new hidden key column to tail of the linked list */
			dqappend(table->columns->v.column, keycol_stmt->v.column);
			/* Make the hidden column the start of the list of columns so it gets assigned "column_number" of 1.
			 * Not necessary but better to have the primary key column at the beginning.
			 */
			table->columns = keycol_stmt;
			/* Get the new key columns */
			max_key = get_key_columns(table, key_columns);
			assert(0 == max_key); /* Assert that there is 1 primary key column now */
			hidden_column_added = TRUE;
		} else {
			char	   key_num_buffer[INT32_TO_STRING_MAX];
			int	   i, len;
			char *	   out_buffer;
			SqlColumn *cur_column, *start_column;

			assert(TABLETYPE_READONLY == table_type);
			UNPACK_SQL_STATEMENT(start_column, table->columns, column);
			cur_column = start_column;
			i = 0;
			do {
				SqlOptionalKeyword *keyword, *t_keyword;
				int		    copied;

				// Construct the key num keyword
				SQL_STATEMENT(statement, keyword_STATEMENT);
				MALLOC_STATEMENT(statement, keyword, SqlOptionalKeyword);
				keyword = statement->v.keyword;
				keyword->keyword = OPTIONAL_KEY_NUM;
				// key num value is index of key in table
				copied = snprintf(key_num_buffer, INT32_TO_STRING_MAX, "%d", i);
				assert(INT32_TO_STRING_MAX > copied);
				UNUSED(copied); /* Needed to avoid DeadStores warning in non-Debug builds */
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
			/* Get the new key columns */
			max_key = get_key_columns(table, key_columns);
			assert((i - 1) == max_key); /* Assert that there are "i" (i.e. all columns) primary key columns */
		}
		break;
	case -2:
		/* There was some error during the key column determination */
		return NULL;
		break;
	default:
		/* Normal case. There was at least one key column defined by the user. */
		assert(0 <= max_key);
		break;
	}
	assert(0 <= max_key);
	/****************************************************************************
	 * Assign column PIECE numbers to non-key columns if not explicitly specified.
	 * Also assign "column_number" to all columns (key or non-key). Later used by hash_canonical_query.
	 * This is safe to do now that all key columns (if any) have been determined at this point.
	 ****************************************************************************
	 */
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	column_number = 0;
	piece_number = 1;
	do {
		boolean_t	    delim_is_empty, remove_piece_keyword;
		SqlOptionalKeyword *keyword, *piece_keyword;

		column_number++;
		cur_column->table = table_stmt;
		keyword = get_keyword(cur_column, OPTIONAL_DELIM);
		delim_is_empty = FALSE;
		if (NULL != keyword) {
			char *delim, ch;

			cur_column->delim = keyword->v;
			/* Check if DELIM is "". If so, ignore any PIECE specifications as we want the entire node. */
			UNPACK_SQL_STATEMENT(value, keyword->v, value);
			delim = value->v.reference;
			ch = *delim;
			assert((DELIM_IS_DOLLAR_CHAR == ch) || (DELIM_IS_LITERAL == ch));
			if (DELIM_IS_LITERAL == ch) {
				delim++; /* skip first byte to get actual delimiter */
				ch = *delim;
				delim_is_empty = ('\0' == ch);
			}
		} else {
			assert(NULL == cur_column->delim);
		}
		/* Assign each column a PIECE number if one was not explicitly specified.
		 * PRIMARY KEY columns (those that have a PRIMARY_KEY or OPTIONAL_KEY_NUM specified) are not
		 * counted towards the default piece #.
		 */
		piece_keyword = get_keyword(cur_column, OPTIONAL_PIECE);
		remove_piece_keyword = FALSE;
		if ((NULL == get_keyword(cur_column, PRIMARY_KEY)) && (NULL == get_keyword(cur_column, OPTIONAL_KEY_NUM))) {
			/* Add PIECE keyword only if DELIM is not "" */
			if (NULL == piece_keyword) {
				if (!delim_is_empty) {
					SqlOptionalKeyword *column_keywords, *new_piece_keyword;

					new_piece_keyword = add_optional_piece_keyword_to_sql_column(piece_number);
					UNPACK_SQL_STATEMENT(column_keywords, cur_column->keywords, keyword);
					dqappend(column_keywords, new_piece_keyword);
				}
				/* PIECE was not explicitly specified for this non-key column so count this column towards
				 * the default piece number that is used for other non-key columns with no PIECE specified.
				 * Note that this is done even in the case DELIM "" is specified for a non-key column.
				 */
				piece_number++;
			} else if (delim_is_empty) {
				/* PIECE numbers are not applicable for non-key columns with DELIM "" so remove it */
				remove_piece_keyword = TRUE;
			}
		} else if (NULL != piece_keyword) {
			/* PIECE numbers (if specified) are not applicable for primary key columns so remove it */
			remove_piece_keyword = TRUE;
		}
		if (remove_piece_keyword) {
			SqlOptionalKeyword *next;

			next = piece_keyword->next; /* Note down next before "dqdel" */
			dqdel(piece_keyword);
			if (piece_keyword == cur_column->keywords->v.keyword) {
				/* We removed the first element in the keyword list. Update column keyword list head pointer */
				cur_column->keywords->v.keyword = next;
			}
		}
		cur_column->column_number = column_number;
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	/**************************************************************************************************************
	 * Now that key columns are identified and column PIECE numbers for non-key columns are assigned, proceed with
	 * rest of the initialization.
	 **************************************************************************************************************
	 */
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
				return NULL;
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
				return NULL;
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
					readwrite_disallowed = (!delim_is_empty || (NULL != first_non_key_column));
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
	/* Now that all table level keywords have been seen, check for incompatibilities. */
	/* 1) If table-level GLOBAL keyword is NOT specified, compute the keyword value.
	 * 2) If table-level GLOBAL keyword is specified and we don't yet know that READWRITE is disallowed in this table,
	 *    check if the GLOBAL keyword value can cause an incompatibility. For this check if the specified GLOBAL keyword is
	 *    an M global name followed by the primary key column(s) as subscripts in the KEY NUM order. If so it is
	 *    compatible with READWRITE. Otherwise it is not.
	 * 3) If table-level GLOBAL keyword is specified and we already know that READWRITE is disallowed in this table due to
	 *    other incompatibilities, no need to do any checks of the specified GLOBAL keyword.
	 */
	if (!(options & SOURCE) || !readwrite_disallowed) {
		int	   i, total_copied;
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
			if (NULL != table_source_gvname) {
				/* User had specified an unsubscripted global name. Use that name. */
				SNPRINTF_TO_BUFFER(table_source_gvname, buffer, buff_ptr, buffer_size, total_copied);
			} else {
				/* User did not specify any global name. Auto-generate one. */
				UNPACK_SQL_STATEMENT(value, table->tableName, value);

				/* Hashed table name (which matches $ZYSUFFIX) */
				ydb_mmrhash_128(value->v.reference, strlen(value->v.reference), 0, &mmr_table_hash);
				ydb_hash_to_string(&mmr_table_hash, table_hash, OCTO_HASH_LEN);
				table_hash[OCTO_HASH_LEN] = '\0';
				assert(strlen(table_hash) == 22);

				/* Add ^TABLE_GLOBAL_NAME_PREFIX to table name, then add table hash to table name after prefix */
				SNPRINTF_TO_BUFFER("^", buffer, buff_ptr, buffer_size, total_copied);
				SNPRINTF_TO_BUFFER(TABLE_GLOBAL_NAME_PREFIX, buffer, buff_ptr, buffer_size, total_copied);
				SNPRINTF_TO_BUFFER(table_hash, buffer, buff_ptr, buffer_size, total_copied);
			}
			next = NULL; /* to avoid false [-Wmaybe-uninitialized] warnings from compiler */
		} else {
			SqlOptionalKeyword *keyword;

			/* Copy just the subscripts portion of the M gvn specified in GLOBAL keyword for later "strcasecmp" check */
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
			SqlOptionalKeyword *keyword;

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
		SqlOptionalKeyword *keyword;

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
		/* READWRITE has been explicitly specified but at least one incompatible qualifier was found. Issue error. */
		ERROR(ERR_READWRITE_DISALLOWED, NULL);
		return NULL;
	}
	if (!(options & READONLY) && !(options & READWRITE)) {
		/* Neither READONLY nor READWRITE was specified in the CREATE TABLE command */
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
			if (hidden_column_added && readwrite_disallowed) {
				/* We added a hidden column at the beginning of this function due to the lack of an explicitly
				 * specified primary key column. This was done since at that time the default table type was
				 * READWRITE. But later we found some incompatability that disallows READWRITE and so we
				 * are about to set the table type to READONLY here. But then we have to undo the addition of
				 * the hidden column and a lot of other things that relied on it (e.g. column numbers, primary
				 * key columns, piece numbers etc.). Basically redo this entire function. It is not straightforward
				 * to do so and is not a use case that is likely encountered in practice so we just issue an
				 * error for now even though the user did not explicitly specify READWRITE. We can later rework
				 * this logic to redo and assume as if READONLY was explicitly specified if the need arises.
				 */
				ERROR(ERR_READWRITE_DISALLOWED, NULL);
				return NULL;
			}
			table->readwrite = (readwrite_disallowed ? FALSE : (TABLETYPE_READWRITE == table_type));
		}
	} else {
		table->readwrite = (TABLETYPE_READWRITE == table_type);
	}
	return table_stmt;
}
