/****************************************************************
 *								*
 * Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	*
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
#include "octo_type_check.h"

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
#define DQDELKEYWORD(CUR_KEYWORD, START_KEYWORD, START_KEYWORD_CHANGED, KEYWORDS_STMT)                     \
	{                                                                                                  \
		if (START_KEYWORD == CUR_KEYWORD) {                                                        \
			START_KEYWORD_CHANGED = TRUE;                                                      \
			/* Keyword to be deleted is first in list. Update start pointer of linked list. */ \
			START_KEYWORD = CUR_KEYWORD->next;                                                 \
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

/* The below macro is similar to the DQDELKEYWORD macro above but does it for columns instead of keywords */
#define DQDELCOLUMN(CUR_COLUMN, START_COLUMN, START_COLUMN_CHANGED, COLUMN_STMT)                          \
	{                                                                                                 \
		if (START_COLUMN == CUR_COLUMN) {                                                         \
			START_COLUMN_CHANGED = TRUE;                                                      \
			/* Column to be deleted is first in list. Update start pointer of linked list. */ \
			START_COLUMN = CUR_COLUMN->next;                                                  \
			if (START_COLUMN == CUR_COLUMN) {                                                 \
				/* The only column in the linked list is going to be deleted.             \
				 * Reset the start of linked list to NULL.                                \
				 */                                                                       \
				START_COLUMN = NULL;                                                      \
			}                                                                                 \
			COLUMN_STMT->v.column = START_COLUMN;                                             \
		}                                                                                         \
		dqdel(CUR_COLUMN); /* Delete column */                                                    \
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
	SqlColumn *	    cur_column, *start_column, *next_column, *first_non_key_column;
	SqlStatement *	    statement;
	SqlValue *	    value;
	char *		    out_buffer;
	size_t		    str_len;
	int		    len, piece_number, num_non_key_columns;
	unsigned int	    options;
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

	/* TODO : YDBOcto#582 : UNIQUE
	 *
	 * If specified as a column-level constraint, there is no way multiple columns can be specified there so
	 * it stays a column-level constraint (no table-level constraint possible).
	 *
	 * For each column-level constraint,
	 * 1) Go through each column and combine all UNIQUE constraints into ONE keyword structure
	 * 2) Maintain only the first named unique structure. If nothing is named, pick the first unnamed structure.
	 *    Discard all remaining UNIQUE keyword structures.
	 *
	 * For each table-level constraint,
	 * 1) Maintain a linked list of UNIQUE keyword structures. Possible to have more than one UNIQUE table-level constraints.
	 *
	 */
	/* TODO : YDBOcto#581 : NOT NULL
	 *
	 * For each column-level constraint,
	 * 1) Go through each column and combine all NOT NULL constraints into ONE keyword structure
	 * 2) Take just the first structure. Discard all the rest. Also discard constraint name for this.
	 *
	 * For each table-level constraint,
	 * 1) Not possible as a table-level constraint.
	 *
	 */
	/* TODO : YDBOcto#770 : PRIMARY KEY
	 * If specified as a column-level constraint, there is no way multiple columns can be specified there so
	 * it stays a column-level constraint (no table-level constraint possible).
	 *
	 * For each column-level constraint,
	 * 1) If more than one specified at a column-level and/or the table-level (across all columns), issue error.
	 *
	 * For each table-level constraint,
	 * 1) If more than one specified at a column-level and/or the table-level (across all columns), issue error.
	 *
	 */

	boolean_t primary_key_constraint_seen;
	primary_key_constraint_seen = FALSE;

	/* Define the local variable name under which we will store constraint names as we process the CREATE TABLE.
	 * This will help us identify duplicate constraint names.
	 */
	ydb_buffer_t ydboctoTblConstraint;
	YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOTBLCONSTRAINT, &ydboctoTblConstraint);

	/* Remove any leftover lvn nodes from prior CREATE TABLE query runs just in case */
	int status;
	status = ydb_delete_s(&ydboctoTblConstraint, 0, NULL, YDB_DEL_TREE);
	assert(YDB_OK == status);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return NULL;
	}

	/* Define 2 subscripts. */
	ydb_buffer_t subs[2];
	char	     columnName[OCTO_MAX_IDENT + 1]; /* + 1 for null terminator */
	boolean_t    start_column_changed, start_keyword_changed;

	/* ==============================================================================================================
	 * Scan column-level keywords for CHECK constraint.
	 * a) If specified as a column-level constraint, check the search condition to see if at least one column in the table
	 *    other than the current column of interest is used. If so, move this to a table-level constraint.
	 * b) If specified as a column-level constraint, check the search condition to see if the only column used
	 *    in the constraint is another valid column. If so, make this a column-level constraint of that column.
	 *    Not of the current column.
	 * c) If specified as a column-level constraint, check the search condition to see if no columns are used
	 *    in the constraint. If so, make this a table-level constraint.
	 * This needs to be done first as the automatic name assignment of a CHECK constraint (that happens in a later do/while
	 * loop) is based on which column (if it is a column-level constraint) or table name (if it is a table-level constraint)
	 * it is a part of.
	 */
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	/* Set up first-level subscript as OCTOLIT_COLUMNS for helping determine which columns are used in the CHECK constraint.
	 * Second-level subscript is the actual column name which will be filled inside the "qualify_check_constraint" call.
	 */
	YDB_LITERAL_TO_BUFFER(OCTOLIT_COLUMNS, &subs[0]);
	subs[1].buf_addr = columnName;
	subs[1].len_alloc = sizeof(columnName) - 1; /* reserve 1 byte for null terminator */
	do {
		start_column_changed = FALSE;
		next_column = cur_column->next; /* need to note this down before any DQDELCOLUMN calls below */
		UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
		cur_keyword = start_keyword;
		do {
			SqlOptionalKeyword *next_keyword;
			SqlColumn *	    noted_column;

			start_keyword_changed = FALSE;
			next_keyword = cur_keyword->next;
			switch (cur_keyword->keyword) {
			case OPTIONAL_CHECK_CONSTRAINT:;
				SqlConstraint *constraint;
				SqlValueType   type;

				noted_column = NULL;
				UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);
				if (qualify_check_constraint(constraint->definition, table, &type)) {
					status = ydb_delete_s(&ydboctoTblConstraint, 1, &subs[0], YDB_DEL_TREE);
					assert(YDB_OK == status);
					YDB_ERROR_CHECK(status);
					return NULL; /* CHECK constraint qualification failed */
				}
				if (!IS_BOOLEAN_TYPE(type)) {
					int result;

					ISSUE_TYPE_COMPATIBILITY_ERROR(type, "boolean operations", &constraint->definition, result);
					UNUSED(result);
					status = ydb_delete_s(&ydboctoTblConstraint, 1, &subs[0], YDB_DEL_TREE);
					assert(YDB_OK == status);
					YDB_ERROR_CHECK(status);
					return NULL;
				}
				/* Process list of column names identified as referenced in this CHECK constraint.
				 * Use that to determine if this constraint has to be moved to another column or a table-level one.
				 */
				subs[1].len_used = 0;
				while (TRUE) {
					status = ydb_subscript_next_s(&ydboctoTblConstraint, 2, &subs[0], &subs[1]);
					if (YDB_ERR_NODEEND == status) {
						break;
					}
					YDB_ERROR_CHECK(status);
					if (YDB_OK != status) {
						assert(FALSE);
						return NULL;
					}
					if (NULL != noted_column) {
						/* A column was already noted. This means the current CHECK constraint
						 * references at least 2 columns. It has to be made a table-level constraint.
						 * No need to scan any more.
						 */
						noted_column = NULL;
						break;
					}
					subs[1].buf_addr[subs[1].len_used] = '\0';
					noted_column = find_column(subs[1].buf_addr, table);
					assert(NULL != noted_column);
				}
				/* Remove lvn nodes (if any) that track list of column names referenced in the current
				 * CHECK constraint so we start the next CHECK constraint with a clean state.
				 */
				status = ydb_delete_s(&ydboctoTblConstraint, 1, &subs[0], YDB_DEL_TREE);
				assert(YDB_OK == status);
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					return NULL;
				}
				if (NULL == noted_column) {
					if (NULL != cur_column->columnName) {
						/* The current column-level CHECK constraint references either 0 columns or more
						 * than 1 column from the table and so has to be moved to a table-level constraint
						 * (i.e. a new column with an empty column name).
						 */
						SqlColumn *new_column;

						OCTO_CMALLOC_STRUCT(new_column, SqlColumn);
						dqinit(new_column);
						new_column->columnName = NULL;
						SQL_STATEMENT(new_column->keywords, keyword_STATEMENT);
						new_column->keywords->v.keyword = cur_keyword;
						/* Delete CHECK constraint keyword from current column's keyword linked list */
						DQDELKEYWORD(cur_keyword, start_keyword, start_keyword_changed,
							     cur_column->keywords);
						/* Move this keyword to a new table-level constraint column */
						dqappend(start_column, new_column);
					} else {
						/* An empty current column name implies the CHECK constraint is already part of a
						 * table-level constraint. No need to do anything more.
						 */
					}
				} else if (noted_column != cur_column) {
					/* The CHECK constraint references only one column but is defined as part of a different
					 * column's CHECK constraint. Move the constraint from the current column to the
					 * referenced column.
					 */
					/* Delete CHECK constraint keyword from current column's keyword linked list */
					DQDELKEYWORD(cur_keyword, start_keyword, start_keyword_changed, cur_column->keywords);
					/* Move this keyword to the noted column */
					if (NULL == noted_column->keywords->v.keyword) {
						noted_column->keywords->v.keyword = cur_keyword;
					} else {
						dqappend(noted_column->keywords->v.keyword, cur_keyword);
					}
					if (NULL == cur_column->columnName) {
						/* An empty current column name implies the CHECK constraint is already part of
						 * a table-level constraint. Now that it has been moved to a column-level
						 * constraint, remove this column as its only purpose was to store the
						 * table-level constraint.
						 */
						DQDELCOLUMN(cur_column, start_column, start_column_changed, table->columns);
					}
				} else {
					/* The CHECK constraint references only one column and is already part of that column
					 * level constraint. No more changes needed.
					 */
				}
				break;
			default:
				break;
			}
			cur_keyword = next_keyword;
			if ((NULL == start_keyword) || (!start_keyword_changed && (cur_keyword == start_keyword))) {
				break;
			}
		} while (TRUE);
		cur_column = next_column;
		if ((NULL == start_column) || (!start_column_changed && (cur_column == start_column))) {
			break;
		}
	} while (TRUE);
	/* ==============================================================================================================
	 * Now that CHECK constraint reordering has happened (if needed), do scan/processing of all column-level keywords.
	 * And auto assign constraint names if not specified by the user.
	 */
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	/* Set up first-level subscript as OCTOLIT_NAME for CHECK constraint auto name generation.
	 * Second-level subscript is actual constraint name.
	 */
	YDB_LITERAL_TO_BUFFER(OCTOLIT_NAME, &subs[0]);
	do {
		UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
		cur_keyword = start_keyword;
		do {
			switch (cur_keyword->keyword) {
			case NOT_NULL:
				/* For NOT NULL, Postgres ignores the constraint name. So Octo will do the same.
				 * Discard the SqlStatement and SqlConstraint structures that were malloced.
				 */
				assert(NULL != cur_keyword->v);
				cur_keyword->v = NULL;
				break;
			case PRIMARY_KEY:
				if (primary_key_constraint_seen) {
					UNPACK_SQL_STATEMENT(value, table->tableName, value);
					ERROR(ERR_TABLE_MULTIPLE_PRIMARY_KEYS, value->v.reference);
					yyerror(NULL, NULL, &cur_keyword->v, NULL, NULL, NULL);
					return NULL;
				}
				primary_key_constraint_seen = TRUE;
				/* TODO : YDBOcto#770 : Auto assign name for PRIMARY KEY constraint.
				 * 1) Take this opportunity to check if any of the constraint names have already been used
				 *    (i.e. if there is a collision between a user-specified constraint name and an auto
				 *    assigned name and if so issue error).
				 * 2) When auto assigning names, check against currently used names for collision and if
				 *    so issue error.
				 * 3) Choose a random 8 byte sub string for auto assigning if the total length of
				 *    the name becomes more than 63.
				 */
				cur_keyword->v = NULL; /* TODO: YDBOcto#770: Temporarily set this to get tests to pass */
				break;
			case UNIQUE_CONSTRAINT:
				/* TODO : YDBOcto#582 : Auto assign name for UNIQUE constraint.
				 * 1) Take this opportunity to check if any of the constraint names have already been used
				 *    (i.e. if there is a collision between a user-specified constraint name and an auto
				 *    assigned name and if so issue error).
				 * 2) When auto assigning names, check against currently used names for collision and if
				 *    so issue error.
				 * 3) Choose a random 8 byte sub string for auto assigning if the total length of
				 *    the name becomes more than 63.
				 */
				cur_keyword->v = NULL; /* TODO: YDBOcto#582: Temporarily set this to get tests to pass */
				break;
			case OPTIONAL_CHECK_CONSTRAINT:;
				SqlConstraint *constraint;
				unsigned int   ret_value;

				UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);
				if (NULL == constraint->name) {
					/* User-specified constraint name is NULL. Auto generate a constraint name */
					char *table_name;
					char *column_name;
					char  constraint_name[OCTO_MAX_IDENT + 1];

					UNPACK_SQL_STATEMENT(value, table->tableName, value);
					table_name = value->v.reference;
					if (NULL == cur_column->columnName) {
						column_name = NULL;
					} else {
						SqlValue *column_name_val;
						UNPACK_SQL_STATEMENT(column_name_val, cur_column->columnName, value);
						column_name = column_name_val->v.string_literal;
					}
					/* Generate constraint name of the form TABLENAME_COLUMNNAME_check.
					 * If such a named constraint already exists, try TABLENAME_COLUMNNAME_check1.
					 * If that exists as well, try TABLENAME_COLUMNNAME_check2. etc.
					 */
					int num = 0;
					do {
						/* TODO : YDBOcto#772. Check return value of snprintf below and handle error
						 * if space is not enough by truncating the column name and/or table name
						 * parts of the generated name so we have space for the numeric part at the end.
						 * Choose a random 8 byte sub string for auto assigning if the total length of
						 * the name becomes more than 63.
						 */
						if (!num) {
							if (NULL == column_name) {
								snprintf(constraint_name, sizeof(constraint_name), "%s_%s",
									 table_name, OCTOLIT_CHECK);
							} else {
								snprintf(constraint_name, sizeof(constraint_name), "%s_%s_%s",
									 table_name, column_name, OCTOLIT_CHECK);
							}
						} else {
							if (NULL == column_name) {
								snprintf(constraint_name, sizeof(constraint_name), "%s_%s%d",
									 table_name, OCTOLIT_CHECK, num);
							} else {
								snprintf(constraint_name, sizeof(constraint_name), "%s_%s_%s%d",
									 table_name, column_name, OCTOLIT_CHECK, num);
							}
						}
						/* Check if generated name exists. If so, try next number suffix. */
						YDB_STRING_TO_BUFFER(constraint_name, &subs[1]);
						status = ydb_data_s(&ydboctoTblConstraint, 2, &subs[0], &ret_value);
						assert(YDB_OK == status);
						YDB_ERROR_CHECK(status);
						if (YDB_OK != status) {
							return NULL;
						}
						if (!ret_value) {
							/* Found a name that does not already exist. We are done. */
							break;
						}
						num++;
					} while (TRUE);

					SqlStatement *name_stmt;
					int	      len;
					char *	      malloc_space;
					len = strlen(constraint_name);
					malloc_space = octo_cmalloc(memory_chunks, len + 1);
					strncpy(malloc_space, constraint_name, len + 1);
					SQL_VALUE_STATEMENT(name_stmt, STRING_LITERAL, malloc_space);
					constraint->name = name_stmt;
					YDB_STRING_TO_BUFFER(malloc_space, &subs[1]); /* for use in "ydb_set_s()" call below */
				} else {
					char *	  constraint_name;
					SqlValue *value;

					/* Now that we have a constraint name (either user-specified or auto generated), check if
					 * there are duplicates. If so, issue error.
					 */
					UNPACK_SQL_STATEMENT(value, constraint->name, value);
					constraint_name = value->v.string_literal;
					YDB_STRING_TO_BUFFER(constraint_name, &subs[1]);
					status = ydb_data_s(&ydboctoTblConstraint, 2, &subs[0], &ret_value);
					assert(YDB_OK == status);
					YDB_ERROR_CHECK(status);
					if (YDB_OK != status) {
						return NULL;
					}
					if (ret_value) {
						/* A constraint with the name already exists. Issue duplicate name error. */
						ERROR(ERR_DUPLICATE_CONSTRAINT, constraint_name);
						return NULL;
					}
				}
				/* Now that we know this is not a duplicate, add it to list of known constraint names */
				status = ydb_set_s(&ydboctoTblConstraint, 2, &subs[0], NULL);
				assert(YDB_OK == status);
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					return NULL;
				}
				break;
			default:
				break;
			}
			cur_keyword = cur_keyword->next;
		} while (cur_keyword != start_keyword);
		cur_column = cur_column->next;
	} while (cur_column != start_column);

	/* Remove lvn nodes, if any, from tracking constraint names */
	status = ydb_delete_s(&ydboctoTblConstraint, 0, NULL, YDB_DEL_TREE);
	assert(YDB_OK == status);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return NULL;
	}

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
				/* User-specified an unsubscripted global name in the GLOBAL keyword. Note down the name
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
		case OPTIONAL_AIM_TYPE:
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->aim_type = statement;
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
		boolean_t	    delim_is_empty, remove_piece_keyword, is_extract;
		SqlOptionalKeyword *keyword, *piece_keyword;

		column_number++;
		cur_column->table = table_stmt;

		// Handle DELIM
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

		// Handle EXTRACT
		keyword = get_keyword(cur_column, OPTIONAL_EXTRACT);
		if (NULL != keyword) {
			is_extract = TRUE;
		} else {
			is_extract = FALSE;
		}

		/* Assign each column a PIECE number if one was not explicitly specified.
		 * PRIMARY KEY columns (those that have a PRIMARY_KEY or OPTIONAL_KEY_NUM specified) are not
		 * counted towards the default piece #.
		 */
		piece_keyword = get_keyword(cur_column, OPTIONAL_PIECE);
		remove_piece_keyword = FALSE;
		if (NULL != cur_column->columnName) {
			if (!IS_KEY_COLUMN(cur_column)) {
				/* Add PIECE keyword only if DELIM is not "" and column isn't an EXTRACT field */
				if (NULL == piece_keyword) {
					if (!delim_is_empty && !is_extract) {
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
				} else if (delim_is_empty || is_extract) {
					/* PIECE numbers are not applicable for non-key columns with DELIM "" or EXTRACT so remove
					 * it
					 */
					remove_piece_keyword = TRUE;
				}
			} else if (NULL != piece_keyword) {
				/* PIECE numbers (if specified) are not applicable for primary key columns so remove it */
				remove_piece_keyword = TRUE;
			}
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
	boolean_t readonly_disallowed;	/* TRUE if READONLY at table-level is disallowed due to incompatible option */
	boolean_t readwrite_disallowed; /* TRUE if READWRITE at table-level is disallowed due to incompatible option */

	readwrite_disallowed = FALSE; /* Start with FALSE . Will be set to TRUE later if we find an incompatibility. */
	readonly_disallowed = FALSE;  /* Start with FALSE . Will be set to TRUE later if we find an incompatibility. */
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

		/* Check for duplicate column names. If so issue error.
		 * Skip table-level constraints (which are columns with a NULL columnName).
		 * Note: Cannot do this earlier than when a hidden key column could possibly be added
		 * as otherwise we would skip checking for column name collisions between a user-specified
		 * column name and the auto generated hidden key column name.
		 */
		if (NULL != cur_column->columnName) {
			UNPACK_SQL_STATEMENT(columnName1, cur_column->columnName, value);
			for (cur_column2 = cur_column->next; start_column != cur_column2; cur_column2 = cur_column2->next) {
				if (NULL != cur_column2->columnName) {
					SqlValue *columnName2;

					UNPACK_SQL_STATEMENT(columnName2, cur_column2->columnName, value);
					if (!strcmp(columnName1->v.string_literal, columnName2->v.string_literal)) {
						ERROR(ERR_DUPLICATE_COLUMN, columnName1->v.string_literal);
						return NULL;
					}
				}
			}
		}
		/* Check if there are any incompatible keyword specifications */
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
				/* PRIMARY KEY and KEY NUM are allowed for both READONLY and READWRITE type of tables
				 * so should not set "readonly_disallowed" to TRUE here.
				 */
				break;
			case NOT_NULL:
				/* NOT NULL is allowed for both READONLY and READWRITE type of tables
				 * so should not set "readonly_disallowed" to TRUE here.
				 */
				break;
			case UNIQUE_CONSTRAINT:
				/* Disallow UNIQUE constraint on a READONLY table as it is not possible to enforce this. */
				readonly_disallowed = TRUE; /* UNIQUE is only allowed for READWRITE table. Not READONLY. */
				break;
			case OPTIONAL_EXTRACT:
			case OPTIONAL_SOURCE:
			case OPTIONAL_START:
			case OPTIONAL_STARTINCLUDE:
			case OPTIONAL_END:
			case OPTIONAL_ENDPOINT:
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
			case OPTIONAL_CHECK_CONSTRAINT:
				readonly_disallowed = TRUE; /* CHECK is only allowed for READWRITE table. Not READONLY. */
				break;
			default:
				ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
				assert(FALSE);
				return NULL;
				break;
			}
			cur_keyword = cur_keyword->next;
		} while (cur_keyword != start_keyword);
		if ((NULL != cur_column->columnName) && (!is_key_column)) {
			/* If it is a column with a name (i.e. not a table level constraint) and is not a key column,
			 * treat it as a column with a PIECE number (explicitly specified or implicitly assumed).
			 * The only exception is if DELIM "" is also specified in the column.
			 * If so delete the PIECE keyword as it is ignored.
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
					DQDELKEYWORD(piece_keyword, start_keyword, start_keyword_changed, cur_column->keywords);
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
					DQDELKEYWORD(delim_keyword, start_keyword, start_keyword_changed,
						     first_non_key_column->keywords);
				}
				if (NULL != piece_keyword) {
					DQDELKEYWORD(piece_keyword, start_keyword, start_keyword_changed,
						     first_non_key_column->keywords);
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
	/* Now that all table-level keywords have been seen, check for incompatibilities. */
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
		/* READWRITE has been explicitly specified but at least one incompatible keyword was found. Issue error. */
		ERROR(ERR_READWRITE_DISALLOWED, NULL);
		return NULL;
	}
	if (readonly_disallowed) {
		if (options & READONLY) {
			/* READONLY has been explicitly specified but at least one incompatible keyword was found. Issue error. */
			ERROR(ERR_READONLY_DISALLOWED, NULL);
			return NULL;
		}
		if (readwrite_disallowed) {
			/* READONLY and READWRITE are both disallowed due to incompatible keywords.
			 * Cannot proceed since a table has to be one of those 2 types.
			 */
			ERROR(ERR_READONLY_AND_READWRITE_DISALLOWED, NULL);
			return NULL;
		}
	}
	if (!(options & READONLY) && !(options & READWRITE)) {
		/* Neither READONLY nor READWRITE was specified in the CREATE TABLE command */
		if (config->in_auto_upgrade_binary_table_definition) {
			/* In auto upgrade logic where pre-existing tables are being upgraded. In this case, it is not safe
			 * to infer the READONLY vs READWRITE characteristic of a table from the current octo.conf setting
			 * of "tabletype" since the user of the new Octo build might not have yet known this new keyword
			 * (let alone set this keyword in octo.conf appropriately). Therefore do some additional checks.
			 */
			if (readonly_disallowed) {
				/* READONLY is disallowed. Have to use READWRITE. */
				assert(!readwrite_disallowed); /* or else we would have issued an error above */
				table->readwrite = TRUE;
			} else if (readwrite_disallowed) {
				/* READWRITE is disallowed. Have to use READONLY. */
				assert(!readonly_disallowed); /* or else we would have issued an error above */
				table->readwrite = FALSE;
			} else {
				/* Assume READONLY by default */
				table->readwrite = FALSE;
			}
		} else {
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
			/* If READWRITE incompatible column-level keyword has been specified, assume READONLY.
			 * If READONLY incompatible column-level keyword has been specified, assume READWRITE.
			 * If not, assume value based on octo.conf "tabletype" setting.
			 */
			table->readwrite
			    = (readwrite_disallowed ? FALSE : readonly_disallowed ? TRUE : (TABLETYPE_READWRITE == table_type));
		}
	} else {
		table->readwrite = (TABLETYPE_READWRITE == table_type);
	}
	return table_stmt;
}
