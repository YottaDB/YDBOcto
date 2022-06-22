/****************************************************************
 *								*
 * Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	*
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
			assert(START_COLUMN != CUR_COLUMN);                                               \
			COLUMN_STMT->v.column = START_COLUMN;                                             \
		}                                                                                         \
		dqdel(CUR_COLUMN); /* Delete column */                                                    \
	}

#define ADD_KEY_NUM_KEYWORD_TO_COLUMN(COLUMN, NUM)                                                  \
	{                                                                                           \
		assert(NULL != COLUMN->columnName); /* should be ensured by caller */               \
		/* Construct the key num keyword */                                                 \
		SqlStatement *stmt;                                                                 \
		/* key num value is index of key in table */                                        \
		MALLOC_KEYWORD_STMT(stmt, OPTIONAL_KEY_NUM);                                        \
                                                                                                    \
		char key_num_buffer[INT32_TO_STRING_MAX];                                           \
		int  copied;                                                                        \
		assert(sizeof(NUM) == sizeof(int));                                                 \
		copied = snprintf(key_num_buffer, INT32_TO_STRING_MAX, "%d", NUM);                  \
		assert(INT32_TO_STRING_MAX > copied);                                               \
		UNUSED(copied); /* Needed to avoid DeadStores warning in non-Debug builds */        \
                                                                                                    \
		int len;                                                                            \
		len = strlen(key_num_buffer);                                                       \
                                                                                                    \
		char *outBuff;                                                                      \
		outBuff = octo_cmalloc(memory_chunks, len + 1);                                     \
                                                                                                    \
		strncpy(outBuff, key_num_buffer, len + 1);                                          \
                                                                                                    \
		SqlOptionalKeyword *keyword;                                                        \
		keyword = stmt->v.keyword;                                                          \
		SQL_VALUE_STATEMENT(keyword->v, INTEGER_LITERAL, outBuff);                          \
                                                                                                    \
		/* Insert "stmt" into column keyword list */                                        \
		SqlOptionalKeyword *t_keyword;                                                      \
		if (NULL != COLUMN->keywords) {                                                     \
			/* List of keywords already exists. Append new keyword to existing list. */ \
			UNPACK_SQL_STATEMENT(t_keyword, COLUMN->keywords, keyword);                 \
			dqappend(t_keyword, keyword);                                               \
		} else {                                                                            \
			/* No keywords already exist. Make this the only keyword in the list. */    \
			COLUMN->keywords = stmt;                                                    \
		}                                                                                   \
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
	SqlColumnList *	    dependencies;
	SqlStatement *	    statement;
	SqlValue *	    value;
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
	// Reset the qualify_extract_function() cycle
	qualify_extract_function_cycle = 0;

	/* Define the local variable name under which we will
	 * a) Store constraint names as we process the CREATE TABLE. This will help us identify duplicate constraint names.
	 * b) Store function names/hashes as we process the CREATE TABLE. This will help us ensure a DROP FUNCTION issues an
	 *    error if a table constraint relies on that function definition.
	 */
	ydb_buffer_t ydboctoTblConstraint;
	YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOTBLCONSTRAINT, &ydboctoTblConstraint);
	ydb_buffer_t ydboctoTblExtract;
	YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOTBLEXTRACT, &ydboctoTblExtract);

	/* Remove any leftover lvn nodes from prior CREATE TABLE query runs just in case */
	int status;
	status = ydb_delete_s(&ydboctoTblConstraint, 0, NULL, YDB_DEL_TREE);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		assert(FALSE);
		return NULL;
	}
	status = ydb_delete_s(&ydboctoTblExtract, 0, NULL, YDB_DEL_TREE);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		assert(FALSE);
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
	 *
	 * Scan column-level keywords for UNIQUE or PRIMARY KEY constraint.
	 * a) If specified as a column-level constraint, there is no way multiple columns can be specified there (a syntax
	 *    error would have been issued in that case) so it stays a column-level constraint (i.e. no need to worry about
	 *    moving a column-level constraint to a table-level constraint).
	 * b) For each table-level constraint, check if it specifies only ONE column. If so, move this to be a column-level
	 *    UNIQUE or PRIMARY KEY constraint for the specified column name.
	 *
	 * Scan column-level keywords for PRIMARY KEY constraint.
	 * If any duplicate keywords are found (within a column or across columns), issue error.
	 */
	boolean_t key_num_keyword_seen, table_level_primary_key_constraint_seen;
	key_num_keyword_seen = FALSE;
	table_level_primary_key_constraint_seen = FALSE;

	SqlColumn *primary_key_constraint_col;
	primary_key_constraint_col = NULL;

	/* Note: One might think "primary_key_constraint_col" is enough to ensure only one PRIMARY KEY constraint is specified.
	 * But due to potential moves of table level constraint keywords to column level constraint keywords, we also need
	 * "primary_key_constraint_keyword". There are a few queries in ERR_TABLE_MULTIPLE_PRIMARY_KEYS which will fail to issue
	 * an error without this additional variable in the code.
	 */
	SqlOptionalKeyword *primary_key_constraint_keyword;
	primary_key_constraint_keyword = NULL;

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
			SqlConstraint *	    constraint;

			start_keyword_changed = FALSE;
			next_keyword = cur_keyword->next;
			switch (cur_keyword->keyword) {
			case OPTIONAL_CHECK_CONSTRAINT:;
				SqlValueType type;
				int	     num_cols;

				noted_column = NULL;
				UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);
				assert(OPTIONAL_CHECK_CONSTRAINT == constraint->type);
				/* Note down which constraint we are currently processing. Needed inside "qualify_check_constraint"
				 * to note down function names/hashes used in this constraint. Note that we still have not
				 * assigned a unique name to this constraint (that happens in the later for loop) so we cannot
				 * use the constraint name. Therefore, we use the "constraint" pointer as the subscript/index.
				 */
				ydb_buffer_t func_subs[2];
				YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS, &func_subs[0]);
				func_subs[1].buf_addr = (char *)&constraint->definition;
				func_subs[1].len_used = func_subs[1].len_alloc = sizeof(void *);
				status = ydb_set_s(&ydboctoTblConstraint, 1, &func_subs[0], &func_subs[1]);
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					assert(FALSE);
					return NULL;
				}

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
				num_cols = 0;
				SqlColumnList *start_col;
				/* It is possible that "constraint->v.check_columns" is non-NULL at this point if this was
				 * previously a column level constraint that got moved to a table level constraint (a later column
				 * in the list of columns encountered while processing the table). In that case, the below
				 * processing would end up adding the same set of columns again into "constraint->v.check_columns".
				 * Avoid that by resetting "constraint->check_columns" to NULL.
				 */
				constraint->v.check_columns = NULL;
				start_col = NULL; /* to avoid false [-Wmaybe-uninitialized] warnings from compiler */
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
					num_cols++;
					subs[1].buf_addr[subs[1].len_used] = '\0';
					noted_column = find_column(subs[1].buf_addr, table);
					assert(NULL != noted_column);
					assert(!noted_column->is_hidden_keycol);

					SqlStatement *colname_stmt;
					SQL_STATEMENT(colname_stmt, value_STATEMENT);
					OCTO_CMALLOC_STRUCT(colname_stmt->v.value, SqlValue);
					colname_stmt->v.value->type = COLUMN_REFERENCE;
					/* Note: sizeof of a string literal will include null terminator so that too gets copied in
					 * memcpy below */
					colname_stmt->v.value->v.string_literal = octo_cmalloc(memory_chunks, subs[1].len_used + 1);
					memcpy(colname_stmt->v.value->v.string_literal, subs[1].buf_addr, subs[1].len_used + 1);

					SqlStatement *col_list_stmt;
					col_list_stmt = create_sql_column_list(colname_stmt, NULL, NULL);

					SqlColumnList *cur_col;
					UNPACK_SQL_STATEMENT(cur_col, col_list_stmt, column_list);

					if (NULL == constraint->v.check_columns) {
						constraint->v.check_columns = col_list_stmt;
						start_col = cur_col;
					} else {
						dqappend(cur_col, start_col);
					}
				}
				if (1 < num_cols) {
					/* The current CHECK constraint references at least 2 columns. It has to be made a
					 * table-level constraint. Setting "noted_column" to NULL lets this constraint be
					 * treated the same way as a constraint that referenced 0 columns.
					 */
					noted_column = NULL;
				}
				/* Remove lvn nodes (if any) that track list of column names referenced in the current
				 * CHECK constraint so we start the next CHECK constraint with a clean state.
				 */
				status = ydb_delete_s(&ydboctoTblConstraint, 1, &subs[0], YDB_DEL_TREE);
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					assert(FALSE);
					return NULL;
				}
				if (NULL == noted_column) {
					assert(1 != num_cols);
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
			case PRIMARY_KEY:
				if ((NULL != primary_key_constraint_keyword) && (cur_keyword != primary_key_constraint_keyword)) {
					UNPACK_SQL_STATEMENT(value, table->tableName, value);
					ERROR(ERR_TABLE_MULTIPLE_PRIMARY_KEYS, value->v.reference);
					yyerror(NULL, NULL, &cur_keyword->v, NULL, NULL, NULL);
					return NULL;
				}
				primary_key_constraint_col = cur_column;
				primary_key_constraint_keyword = cur_keyword;
				/* Note: Below comment is needed to avoid gcc [-Wimplicit-fallthrough=] warning */
				/* fall through */
			case UNIQUE_CONSTRAINT:;
				UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);

				SqlStatement *column_name_list;
				column_name_list = constraint->definition;
				if (NULL != column_name_list) {
					if (PRIMARY_KEY == cur_keyword->keyword) {
						table_level_primary_key_constraint_seen = TRUE;
					}
					/* Note: Since only table level UNIQUE or PRIMARY KEY constraint can have a list of columns,
					 * one might be tempted to assert the following.
					 *    assert(NULL == cur_column->columnName);
					 * But this can fail in case this column level UNIQUE or PRIMARY KEY constraint was moved
					 * from a table level UNIQUE or PRIMARY KEY constraint in a previous iteration of the
					 * containing "do/while" loop ("cur_column" is loop variable). Hence the above assert is
					 * commented out.
					 */
					SqlColumnList *start_cl, *cur_cl;
					UNPACK_SQL_STATEMENT(start_cl, column_name_list, column_list);
					cur_cl = start_cl;

					SqlColumn *noted_column;
					noted_column = NULL;
					do {
						SqlValue *col_name;
						UNPACK_SQL_STATEMENT(col_name, cur_cl->value, value);

						noted_column = find_column(col_name->v.string_literal, table);
						assert((NULL == noted_column) || !noted_column->is_hidden_keycol);
						if (NULL == noted_column) {
							/* User specified an unknown column name in the UNIQUE or PRIMARY KEY
							 * constraint list. Issue error.
							 */
							SqlValue *tbl_name;

							UNPACK_SQL_STATEMENT(tbl_name, tableName, value);
							ERROR(ERR_TABLE_UNKNOWN_COLUMN_NAME, col_name->v.string_literal,
							      tbl_name->v.string_literal);
							yyerror(NULL, NULL, &cur_cl->value, NULL, NULL, NULL);
							return NULL;
						}
						cur_cl = cur_cl->next;
						/* Check if duplicate column names are specified. If so, issue error. */
						SqlColumnList *cur_cl2;
						for (cur_cl2 = cur_cl; start_cl != cur_cl2; cur_cl2 = cur_cl2->next) {
							SqlValue *col_name2;

							UNPACK_SQL_STATEMENT(col_name2, cur_cl2->value, value);
							if (!strcmp(col_name->v.string_literal, col_name2->v.string_literal)) {
								ERROR(ERR_DUPLICATE_COLUMN, col_name->v.string_literal);
								yyerror(NULL, NULL, &cur_cl2->value, NULL, NULL, NULL);
								return NULL;
							}
						}
					} while (cur_cl != start_cl);
					assert(NULL != noted_column);
					if ((start_cl->next == start_cl) && (cur_column != noted_column)) {
						/* Table level UNIQUE or PRIMARY KEY constraint only specifies one column. But
						 * that column does not correspond to the current column. Therefore move the
						 * constraint from the current column to the referenced/noted column.
						 */
						/* Delete UNIQUE or PRIMARY KEY constraint keyword from current column's keyword
						 * linked list.
						 */
						DQDELKEYWORD(cur_keyword, start_keyword, start_keyword_changed,
							     cur_column->keywords);
						/* Move this keyword to the referenced column */
						if (NULL == noted_column->keywords->v.keyword) {
							noted_column->keywords->v.keyword = cur_keyword;
						} else {
							dqappend(noted_column->keywords->v.keyword, cur_keyword);
						}
						if (PRIMARY_KEY == cur_keyword->keyword) {
							primary_key_constraint_col = noted_column;
						}
						/* This UNIQUE or PRIMARY KEY constraint is part of a table-level constraint.
						 * Now that it has been moved to a column-level UNIQUE or PRIMARY KEY constraint,
						 * remove this column as its only purpose was to store the table-level constraint.
						 */
						DQDELCOLUMN(cur_column, start_column, start_column_changed, table->columns);
					}
				} else {
					/* This is a column-level UNIQUE or PRIMARY KEY constraint.
					 * Add current column to column list.
					 */
					assert(NULL != cur_column->columnName);
					constraint->definition = create_sql_column_list(cur_column->columnName, NULL, NULL);
				}
				break;
			case OPTIONAL_KEY_NUM:
				key_num_keyword_seen = TRUE;
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
	if (key_num_keyword_seen) {
		if (table_level_primary_key_constraint_seen) {
			/* Issue error since a table-level PRIMARY KEY constraint usage (new syntax) is not compatible
			 * with the KEY NUM syntax (old syntax).
			 */
			UNPACK_SQL_STATEMENT(value, table->tableName, value);
			ERROR(ERR_TABLE_KEY_NUM, value->v.reference);
			return NULL;
		} else {
			/* KEY NUM keyword seen and either a column-level PRIMARY KEY constraint or NO PRIMARY KEY constraint
			 * was seen. Either ways, we need to create a table-level PRIMARY KEY constraint with all columns
			 * containing the KEY NUM Keywords.
			 */
			if (NULL != primary_key_constraint_col) {
				/* Column level PRIMARY KEY constraint seen */
				if (!IS_KEY_COLUMN(primary_key_constraint_col)) {
					/* KEY NUM keyword is not present in this column whereas PRIMARY KEY keyword is.
					 * Add KEY NUM 0 keyword to this column so a later call to "get_key_column()" will
					 * find this as a key column.
					 */
					ADD_KEY_NUM_KEYWORD_TO_COLUMN(primary_key_constraint_col, 0);
				}
			}
			/* Determine all columns with KEY NUM keyword and come up with a column-level or table-level
			 * PRIMARY KEY constraint that takes into account all PRIMARY KEY and KEY NUM columns.
			 */
			memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn *));
			max_key = get_key_columns(table, key_columns);
			if (-2 == max_key) {
				/* There was some error during the key column determination */
				return NULL;
			}
			if (0 < max_key) {
				/* There is more than one key column in this table. Need to create a table level PRIMARY KEY
				 * constraint (for the multiple columns holding the PRIMARY KEY and KEY NUM keywords in that order)
				 * if one does not already exist.
				 */
				cur_column = key_columns[0];
				assert(NULL != cur_column);
				SqlOptionalKeyword *primary_keyword;
				primary_keyword = get_keyword(cur_column, PRIMARY_KEY);
				assert((NULL == primary_keyword) || (cur_column == primary_key_constraint_col));
				assert((NULL != primary_keyword) || (NULL == primary_key_constraint_col));

				if (NULL != primary_keyword) {
					assert(NULL != cur_column->columnName);
					/* The first key column has a PRIMARY KEY keyword and that column is not a table-level
					 * constraint (guaranteed by the above assert). Therefore, the PRIMARY KEY keyword is
					 * a column-level constraint and needs to be made a table-level constraint.
					 */
					/* Remove PRIMARY KEY keyword from current column's keyword linked list */
					UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
					DQDELKEYWORD(primary_keyword, start_keyword, start_keyword_changed, cur_column->keywords);
					assert(NULL != start_keyword); /* There is at least one keyword still left in the column */
					/* Assert that this column is still a key column because KEY NUM keyword is present */
					assert(IS_KEY_COLUMN(cur_column));
				}
				/* Add a new column with the PRIMARY KEY keyword and the column list */
				SqlColumn *new_column;
				OCTO_CMALLOC_STRUCT(new_column, SqlColumn);
				dqinit(new_column);
				new_column->columnName = NULL;
				MALLOC_KEYWORD_STMT(new_column->keywords, PRIMARY_KEY);
				cur_keyword = new_column->keywords->v.keyword;
				SQL_STATEMENT(cur_keyword->v, constraint_STATEMENT);

				SqlConstraint *constraint;
				OCTO_CMALLOC_STRUCT(constraint, SqlConstraint);
				cur_keyword->v->v.constraint = constraint;
				constraint->type = PRIMARY_KEY;

				/* Initialize constraint->name */
				if (NULL != primary_keyword) {
					/* Keep user specified PRIMARY KEY constraint name in new table level constraint */
					SqlConstraint *primary_constraint;
					UNPACK_SQL_STATEMENT(primary_constraint, primary_keyword->v, constraint);
					constraint->name = primary_constraint->name;
				} else {
					constraint->name = NULL; /* A name will be auto generated a little later */
				}

				/* Initialize constraint->definition */
				SqlColumnList *start_cl;
				start_cl = NULL;
				/* Add all key columns to the table-level PRIMARY KEY constraint. */
				int key_num;
				for (key_num = 0; key_num <= max_key; key_num++) {
					SqlColumnList *cur_cl;
					OCTO_CMALLOC_STRUCT(cur_cl, SqlColumnList);
					cur_cl->value = copy_sql_statement(key_columns[key_num]->columnName);
					dqinit(cur_cl);
					if (NULL == start_cl) {
						start_cl = cur_cl;
					} else {
						dqappend(start_cl, cur_cl);
					}
				}

				SqlStatement *column_name_list;
				SQL_STATEMENT(column_name_list, column_list_STATEMENT);
				column_name_list->v.column_list = start_cl;
				constraint->definition = column_name_list;
				/* Add new table constraint as a column in the table */
				UNPACK_SQL_STATEMENT(start_column, table->columns, column);
				dqappend(start_column, new_column);
				/* Note this new column as the only PRIMARY KEY constraint column */
				primary_key_constraint_col = new_column;
			}
		}
	}
	/* ==============================================================================================================
	 * Scan column-level keywords for UNIQUE constraint.
	 * For each column-level constraint,
	 *   1) Go through each column and combine multiple UNIQUE constraint keywords into just ONE keyword.
	 *   2) Maintain only the first named UNIQUE keyword structure. If nothing is named, pick the first
	 *      unnamed structure. Discard all remaining UNIQUE keyword structures.
	 * Note that the UNIQUE keyword trimming treats a PRIMARY KEY specification as a UNIQUE keyword too and
	 * gives the latter the precedence.
	 *
	 * Note: Note that this could possibly be done in the previous do/while loop thereby avoiding an extra do/while loop.
	 * But that code is expected to be harder to read/maintain hence chose the current (separate do/while loop) approach.
	 *
	 * Scan column-level keywords for PRIMARY KEY constraint.
	 * Add KEY NUM keyword to all columns involved in the column or table level PRIMARY KEY constraint.
	 */
	assert(table->columns->v.column == start_column);

	/* While checking for duplicate UNIQUE keywords, also consider a PRIMARY KEY constraint as a UNIQUE specification.
	 * For example, a "CREATE TABLE tmp (id INTEGER PRIMARY KEY UNIQUE);" should remove the "UNIQUE" keyword
	 * as it is a duplicate of the "PRIMARY KEY" keyword. Hence the "if" check below to set "unique_keyword"
	 * starting value to any PRIMARY KEY specification. This helps with column-level UNIQUE keyword trimming below.
	 */
	if (NULL != primary_key_constraint_col) {
		/* To help with trimming between a table-level PRIMARY KEY and a table-level UNIQUE constraint, we need to move
		 * the PRIMARY KEY table-level constraint ahead of all table-level UNIQUE constraints. This is needed so the
		 * column deletion of duplicate table-level constraints (that happens below) will always delete only the UNIQUE
		 * constraint and not the PRIMARY KEY constraint.
		 */
		if (NULL == primary_key_constraint_col->columnName) {
			/* The PRIMARY KEY constraint is a table-level constraint. Move it ahead of all other table-level
			 * constraints (particularly the table-level UNIQUE constraints).
			 */
			SqlColumn *start_tbl_constraint, *start_col_constraint, **start_ptr;
			start_tbl_constraint = NULL; /* Linked list of table-constraint columns */
			start_col_constraint = NULL; /* Linked list of non-table-constraint columns */
			cur_column = start_column;
			do {
				next_column = cur_column->next;
				dqdel(cur_column);
				if (cur_column != primary_key_constraint_col) {
					start_ptr
					    = (NULL == cur_column->columnName) ? &start_tbl_constraint : &start_col_constraint;
					if (NULL == *start_ptr) {
						*start_ptr = cur_column;
					} else {
						dqappend(*start_ptr, cur_column);
					}
				}
				if (next_column == cur_column) {
					break;
				}
				cur_column = next_column;
			} while (TRUE);
			/* The below "if" check is needed to avoid a false clang-tidy warning about "start_col_constraint"
			 * potentially being NULL.
			 */
			if (NULL == start_col_constraint) {
				assert(FALSE);
				FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
				return NULL;
			}
			/* First insert table-level PRIMARY KEY constraint after all non-table-level constraints */
			dqappend(start_col_constraint, primary_key_constraint_col);
			/* Then insert all other table-level constraints (e.g. UNIQUE), if any */
			if (NULL != start_tbl_constraint) {
				dqappend(start_col_constraint, start_tbl_constraint);
			}
			start_column = start_col_constraint;
			table->columns->v.column = start_column;
		}
	}
	cur_column = start_column;
	do {
		SqlOptionalKeyword *unique_keyword;
		if (cur_column == primary_key_constraint_col) {
			unique_keyword = get_keyword(primary_key_constraint_col, PRIMARY_KEY);
			assert(NULL != unique_keyword);
		} else {
			unique_keyword = NULL;
		}

		UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
		cur_keyword = start_keyword;
		do {
			SqlOptionalKeyword *next_keyword;

			next_keyword = cur_keyword->next;
			switch (cur_keyword->keyword) {
			case PRIMARY_KEY:;
				SqlConstraint *constraint;
				UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);
				assert(NULL != constraint->definition);
				assert(constraint->type == cur_keyword->keyword);

				SqlColumnList *start_cl, *cur_cl;
				UNPACK_SQL_STATEMENT(start_cl, constraint->definition, column_list);
				cur_cl = start_cl;

				int key_col_num;
				key_col_num = 0;
				do {
					SqlValue *col_name;
					UNPACK_SQL_STATEMENT(col_name, cur_cl->value, value);

					SqlColumn *cur_column;
					cur_column = find_column(col_name->v.string_literal, table);
					assert((NULL != cur_column) && !cur_column->is_hidden_keycol);

					/* Check if KEY NUM keyword is present in this column. If so, check if the number
					 * matches that derived from the PRIMARY KEY constraint column specification order.
					 * If not, issue an error. If not present, add KEY NUM keyword to this column with
					 * appropriate number.
					 */
					SqlOptionalKeyword *keyword;
					keyword = get_keyword(cur_column, OPTIONAL_KEY_NUM);
					if (NULL == keyword) {
						/* Add KEY NUM keyword */
						ADD_KEY_NUM_KEYWORD_TO_COLUMN(cur_column, key_col_num);
					}
#ifndef NDEBUG
					if (NULL != keyword) {
						SqlValue *value;
						UNPACK_SQL_STATEMENT(value, keyword->v, value);

						int key_num;
						key_num = atoi(value->v.string_literal);
						assert(MAX_KEY_COUNT > key_num);
						assert(key_num == key_col_num);
					}
#endif
					key_col_num++;
					cur_cl = cur_cl->next;
				} while (cur_cl != start_cl);
				if (NULL != cur_column->columnName) {
					/* PRIMARY KEY is part of a column level constraint. No more processing needed. */
					break;
				}
				/* PRIMARY KEY is part of a table level constraint. Need to fall through to the below logic
				 * to see if PRIMARY KEY vs UNIQUE keyword trimming needs to happen across table-level constraints.
				 */
				/* Note: Below comment is needed to avoid gcc [-Wimplicit-fallthrough=] warning */
				/* fall through */
			case UNIQUE_CONSTRAINT:
				if (NULL != cur_column->columnName) {
					assert(UNIQUE_CONSTRAINT == cur_keyword->keyword);
					/* It is a column-level UNIQUE constraint */
					if (NULL == unique_keyword) {
						unique_keyword = cur_keyword;
					} else {
						/* We found 2 UNIQUE/PRIMARY KEY keywords. Check if the first one is named.
						 * If so, we can ignore the second one. If not, check if the second one is
						 * named and the first one is not named. If so, copy the name from the
						 * second one to the first one and ignore the second one. If a PRIMARY KEY
						 * keyword is one of the 2 found keywords, it is always treated as the
						 * 1st keyword so the UNIQUE gets discarded (if any).
						 */
						SqlConstraint *constraint1, *constraint2;
						UNPACK_SQL_STATEMENT(constraint1, unique_keyword->v, constraint);
						UNPACK_SQL_STATEMENT(constraint2, cur_keyword->v, constraint);
						assert(NULL != constraint1->definition);
						assert(NULL != constraint2->definition);
						if ((NULL == constraint1->name) && (NULL != constraint2->name)) {
							/* Copy over the name from the second to the first constraint.
							 * And then discard the second constraint.
							 */
							constraint1->name = constraint2->name;
						}
						assert(start_keyword != cur_keyword);
						assert(next_keyword != cur_keyword);
						dqdel(cur_keyword);
					}
				} else {
					/* It is a table-level UNIQUE or PRIMARY KEY constraint.
					 * Check if it is duplicated across multiple table-level UNIQUE constraints.
					 * If so, trim that down too.
					 */
					SqlConstraint *constraint1;
					UNPACK_SQL_STATEMENT(constraint1, cur_keyword->v, constraint);
					assert(NULL != constraint1->definition);

					SqlColumn *cur_col2, *next_col2;
					for (cur_col2 = cur_column->next; start_column != cur_col2; cur_col2 = next_col2) {
						next_col2 = cur_col2->next;
						if (NULL != cur_col2->columnName) {
							/* It is a column-level constraint. No need to check for duplication
							 * between a table-level constraint and a column-level constraint.
							 */
							continue;
						}

						boolean_t del_col2;
						del_col2 = FALSE;

						/* Check if two table-level UNIQUE constraints are identical by
						 * comparing the column list in the two constraint definitions.
						 */
						SqlOptionalKeyword *start_keyword2, *cur_keyword2;
						UNPACK_SQL_STATEMENT(start_keyword2, cur_col2->keywords, keyword);
						cur_keyword2 = start_keyword2;
						do {
							switch (cur_keyword2->keyword) {
							case UNIQUE_CONSTRAINT:;
								SqlConstraint *constraint2;
								UNPACK_SQL_STATEMENT(constraint2, cur_keyword2->v, constraint);
								assert(NULL != constraint2->definition);
								if (match_sql_statement(constraint1->definition,
											constraint2->definition)) {
									if ((NULL == constraint1->name)
									    && (NULL != constraint2->name)) {
										/* Copy over the name from the second to the first
										 * constraint. And then discard the second
										 * constraint.
										 */
										constraint1->name = constraint2->name;
									}
									del_col2 = TRUE;
								}
								break;
							default:
								break;
							}
							if (del_col2) {
								break;
							}
							cur_keyword2 = cur_keyword2->next;
						} while (cur_keyword2 != start_keyword2);
						if (del_col2) {
							/* This duplicate UNIQUE constraint can be removed. */
							assert(start_column != cur_col2);
							assert(cur_column != cur_col2);
							dqdel(cur_col2);
						}
					}
				}
				break;
			default:
				break;
			}
			cur_keyword = next_keyword;
		} while (cur_keyword != start_keyword);
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	/* ==============================================================================================================
	 * Scan column-level keywords for NOT NULL constraint.
	 * For each column-level constraint,
	 *   1) Keep the first NOT NULL column-level constraint keyword and discard all remaining NOT NULL constraint keywords
	 *      in the same column.
	 *
	 * Scan column-level keywords for CHECK constraint.
	 * Now that CHECK constraint reordering has happened (if needed), do scan/processing of all column-level keywords.
	 *   And auto assign constraint names if not specified by the user.
	 *
	 * Scan column-level keywords for UNIQUE constraint.
	 * Now that UNIQUE constraint trimming down of multiple keywords as well as reordering has happened,
	 *   check if the UNIQUE constraint has a name. If not, auto generate a name for that constraint.
	 * Use this opportunity to also fill in "constraint->v.uniq_gblname" for the UNIQUE constraint now that
	 *   "constraint->definition" is stable.
	 *
	 * Scan column-level keywords for PRIMARY KEY constraint.
	 * Check if the PRIMARY KEY constraint has a name. If not, auto generate a name for that constraint.
	 *
	 * Scan column-level keywords for IDENTITY constraint.
	 * Issue error if multiple IDENTITY keywords are specified for a column.
	 */
	int num_user_visible_columns;

	assert(table->columns->v.column == start_column);
	cur_column = start_column;
	/* Set up first-level subscript as OCTOLIT_NAME for CHECK/UNIQUE/PRIMARY KEY constraint auto name generation.
	 * Second-level subscript is actual constraint name.
	 */
	YDB_LITERAL_TO_BUFFER(OCTOLIT_NAME, &subs[0]);
	num_user_visible_columns = 0;

	do {
		boolean_t not_null_constraint_seen_for_this_column;
		not_null_constraint_seen_for_this_column = FALSE;

		boolean_t identity_constraint_seen_for_this_column;
		identity_constraint_seen_for_this_column = FALSE;

		if (NULL != cur_column->columnName) {
			num_user_visible_columns++;
		}
		UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
		cur_keyword = start_keyword;
		do {
			SqlOptionalKeyword *next_keyword;
			next_keyword = cur_keyword->next;

			SqlConstraint *constraint;
			switch (cur_keyword->keyword) {
			case OPTIONAL_GENERATED_ALWAYS_IDENTITY:
			case OPTIONAL_GENERATED_BY_DEFAULT_IDENTITY:
				assert(NULL != cur_column->columnName);
				if (NULL != cur_column->columnName) { // Avoids [clang-analyzer-core.NullDereference]
					if (identity_constraint_seen_for_this_column) {
						SqlValue *column_name;
						SqlValue *table_name;
						UNPACK_SQL_STATEMENT(column_name, cur_column->columnName, value);
						UNPACK_SQL_STATEMENT(table_name, table->tableName, value);
						/* More than one identity constraint, this is invalid */
						ERROR(ERR_TABLE_MULTIPLE_IDENTITY, column_name->v.string_literal,
						      table_name->v.string_literal);
						yyerror(NULL, NULL, &cur_keyword->v, NULL, NULL, NULL);
						return NULL;
					} else {
						identity_constraint_seen_for_this_column = TRUE;
						/* For IDENTITY, Postgres ignores the constraint name. So Octo will do the same.
						 * Discard the SqlStatement and SqlConstraint structures that were malloced.
						 */
						assert(NULL != cur_keyword->v);
						cur_keyword->v = NULL;
					}
					// Also check if the type of the column. Identity is only allowed on integer based columns.
					if (INTEGER_TYPE != cur_column->data_type_struct.data_type) {
						ERROR(ERR_NON_INTEGER_IDENTITY, "");
						yyerror(NULL, NULL, &cur_column->columnName, NULL, NULL, NULL);
						return NULL;
					}
				}
				break;
			case NOT_NULL:
				if (not_null_constraint_seen_for_this_column) {
					/* More than one NOT NULL constraints exist for this column.
					 * Ignore all but the first one in the keyword linked list for this column.
					 */
					assert(start_keyword != cur_keyword);
					assert(next_keyword != cur_keyword);
					dqdel(cur_keyword);
				} else {
					not_null_constraint_seen_for_this_column = TRUE;
					/* For NOT NULL, Postgres ignores the constraint name. So Octo will do the same.
					 * Discard the SqlStatement and SqlConstraint structures that were malloced.
					 */
					assert(NULL != cur_keyword->v);
					cur_keyword->v = NULL;
				}
				break;
			case PRIMARY_KEY:
			case UNIQUE_CONSTRAINT:
			case OPTIONAL_CHECK_CONSTRAINT:;
				unsigned int ret_value;

				UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);
				assert(constraint->type == cur_keyword->keyword);
				if (NULL == constraint->name) {
					/* User-specified constraint name is NULL. Auto generate a constraint name */
					char *table_name;
					char *column_name;
					char  constraint_name[OCTO_MAX_IDENT + 1]; /* +1 for null terminator */

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
						/* The below call will fill "constraint_name" with an auto generated name */
						constraint_name_auto_generate(cur_keyword, table_name, column_name, num,
									      constraint_name, sizeof(constraint_name));
						/* Check if generated name already exists in this table.
						 * If so, try next number suffix.
						 */
						YDB_STRING_TO_BUFFER(constraint_name, &subs[1]);
						status = ydb_data_s(&ydboctoTblConstraint, 2, &subs[0], &ret_value);
						YDB_ERROR_CHECK(status);
						if (YDB_OK != status) {
							assert(FALSE);
							return NULL;
						}
						if (!ret_value) {
							/* Found a name that does not already exist. */
							/* Check if this is a PRIMARY KEY constraint. If so, the name should
							 * not just be unique within this table, but also across PRIMARY KEY
							 * constraint names of other tables. If not, try next number suffix.
							 */
							if (PRIMARY_KEY == constraint->type) {
								ydb_buffer_t pkey_subs[3];
								YDB_STRING_TO_BUFFER(config->global_names.octo, &pkey_subs[0]);
								YDB_STRING_TO_BUFFER(OCTOLIT_PRIMARY_KEY_NAME, &pkey_subs[1]);
								YDB_STRING_TO_BUFFER(constraint_name, &pkey_subs[2]);
								status = ydb_data_s(&pkey_subs[0], 2, &pkey_subs[1], &ret_value);
								YDB_ERROR_CHECK(status);
								if (YDB_OK != status) {
									assert(FALSE);
									return NULL;
								}
								if (!ret_value) {
									/* Found an auto assigned name for the PRIMARY KEY
									 * constraint that does not already exist in other
									 * tables too. We are done.
									 */
									break;
								}
								/* else: we need to try next number suffix. */
							} else {
								break;
							}
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
					/* Set the constraint name to always be double-quoted when handling by other modules,
					 * e.g. `emit_column_specification.c` and `describe_tablename.c`.
					 */
					name_stmt->v.value->is_double_quoted = TRUE;
					constraint->name = name_stmt;
					YDB_STRING_TO_BUFFER(malloc_space, &subs[1]); /* for use in "ydb_set_s()" call below */
				} else {
					char *	  constraint_name;
					SqlValue *value;

					/* Now that we have a constraint name (either user-specified or auto generated), check if
					 * there are duplicates. If so, issue error.
					 */
					UNPACK_SQL_STATEMENT(value, constraint->name, value);
					/* Set the constraint name to always be double-quoted when handling by other modules,
					 * e.g. `emit_column_specification.c` and `describe_tablename.c`.
					 */
					value->is_double_quoted = TRUE;
					constraint_name = value->v.string_literal;
					YDB_STRING_TO_BUFFER(constraint_name, &subs[1]);
					status = ydb_data_s(&ydboctoTblConstraint, 2, &subs[0], &ret_value);
					YDB_ERROR_CHECK(status);
					if (YDB_OK != status) {
						assert(FALSE);
						return NULL;
					}
					if (ret_value) {
						/* A constraint with the name already exists. Issue duplicate name error. */
						ERROR(ERR_DUPLICATE_CONSTRAINT, constraint_name);
						return NULL;
					} else if ((PRIMARY_KEY == constraint->type)
						   && !config->in_auto_upgrade_binary_table_definition) {
						/* Check if the explicitly specified name for the PRIMARY KEY constraint
						 * conflicts with any PRIMARY KEY constraint name of other existing tables.
						 * If so, treat this also as a duplicate constraint issue.
						 * Note that we do not want to do this check in case of an auto upgrade logic
						 * where pre-existing tables are being upgraded. In that case, the gvn node
						 * would already exist indicating a conflict but it is a conflict with the same
						 * table as we are auto-upgrading right now and so we skip this check in that case.
						 */
						ydb_buffer_t pkey_subs[4];
						YDB_STRING_TO_BUFFER(config->global_names.octo, &pkey_subs[0]);
						YDB_STRING_TO_BUFFER(OCTOLIT_PRIMARY_KEY_NAME, &pkey_subs[1]);
						YDB_STRING_TO_BUFFER(constraint_name, &pkey_subs[2]);

						char table_name[OCTO_MAX_IDENT + 1];
						pkey_subs[3].buf_addr = table_name;
						pkey_subs[3].len_used
						    = 0; /* needed to avoid false [clang-analyzer-core.uninitialized.ArraySubscript]
							    warning when we use pkey_subs[3].len_used later below */
						pkey_subs[3].len_alloc = sizeof(table_name);
						status = ydb_get_s(&pkey_subs[0], 2, &pkey_subs[1], &pkey_subs[3]);
						if (YDB_ERR_GVUNDEF != status) {
							/* User specified PRIMARY KEY constraint name conflicts with the
							 * PRIMARY KEY constraint name of another existing table. Issue error.
							 */
							YDB_ERROR_CHECK(status);
							if (YDB_OK != status) {
								assert(FALSE);
								return NULL;
							}
							assert(pkey_subs[3].len_used < pkey_subs[3].len_alloc);
							table_name[pkey_subs[3].len_used] = '\0';
							ERROR(ERR_DUPLICATE_PRIMARY_KEY_CONSTRAINT, constraint_name, table_name);
							return NULL;
						}
						/* else: We know the user specified PRIMARY KEY constraint name does not
						 * conflict with the PRIMARY KEY constraint names of any other existing table.
						 * So move on to the next step.
						 */
					}
				}
				/* Now that we know this is not a duplicate, add it to list of known constraint names */
				status = ydb_set_s(&ydboctoTblConstraint, 2, &subs[0], NULL);
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					assert(FALSE);
					return NULL;
				}
				if (OPTIONAL_CHECK_CONSTRAINT == constraint->type) {
					/* Note down the mapping between a constraint definition (the 8-byte pointer) and its name.
					 * This is needed later in "store_table_in_pg_class.c" to know which constraint uses which
					 * function names/hashes. Note that functions are only possible in CHECK constraints. Not
					 * UNIQUE constraints.
					 */
					ydb_buffer_t map_subs[2];
					YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS_MAP, &map_subs[0]);
					map_subs[1].buf_addr = (char *)&constraint->definition;
					map_subs[1].len_used = map_subs[1].len_alloc = sizeof(void *);
					/* Note: subs[1] contains the constraint name */
					status = ydb_set_s(&ydboctoTblConstraint, 2, &map_subs[0], &subs[1]);
					YDB_ERROR_CHECK(status);
					if (YDB_OK != status) {
						assert(FALSE);
						return NULL;
					}
				} else if (UNIQUE_CONSTRAINT == constraint->type) {
					/* Initialize "constraint->v.uniq_gblname" for the UNIQUE constraint (based on the
					 * table name and list of column names that form the UNIQUE constraint) now that
					 * "constraint->definition" is stable.
					 */
					UNPACK_SQL_STATEMENT(value, table->tableName, value);

					hash128_state_t state;
					HASH128_STATE_INIT(state, 0);
					ydb_mmrhash_128_ingest(&state, (void *)value->v.reference, strlen(value->v.reference));

					SqlColumnList *start_cl, *cur_cl;
					UNPACK_SQL_STATEMENT(start_cl, constraint->definition, column_list);
					cur_cl = start_cl;
					do {
						SqlValue *col_name;
						UNPACK_SQL_STATEMENT(col_name, cur_cl->value, value);
						ydb_mmrhash_128_ingest(&state, (void *)col_name->v.string_literal,
								       strlen(col_name->v.string_literal));
						cur_cl = cur_cl->next;
					} while (cur_cl != start_cl);

					char *buf;
					buf = octo_cmalloc(memory_chunks, MAX_ROUTINE_LEN + 2);
					/* + 2 below is because : 1 byte for '^' global prefix, 1 byte for null terminator */
					generate_name_type(UniqueGlobal, &state, 0, buf, MAX_ROUTINE_LEN + 2);

					SqlStatement *uniq_gblname;
					SQL_STATEMENT(uniq_gblname, value_STATEMENT);
					MALLOC_STATEMENT(uniq_gblname, value, SqlValue);
					uniq_gblname->v.value->type = STRING_LITERAL;
					uniq_gblname->v.value->v.string_literal = buf;
					constraint->v.uniq_gblname = uniq_gblname;

					/* TODO: Currently we only use the table name and column names in the UNIQUE constraint
					 *   to generate this hash. But we need to use the schema name (YDBOcto#99) and database
					 *   name (YDBOcto#417) when support for those are added.
					 *
					 * TODO: Additionally we might need to migrate data from the old global names that used to
					 *   previously maintain the UNIQUE constraint to the new global names when YDBOcto#99 and
					 *   YDBOcto#417 are each implemented. Maybe as part of the auto-upgrade logic or so.
					 *   An alternative to avoid data migration is to skip including the database and/or
					 *   schema name in the hash when the defaults are in use. See below comment for details.
					 *	https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/417#note_1072666858
					 */
				}
				break;
			default:
				break;
			}
			cur_keyword = next_keyword;
		} while (cur_keyword != start_keyword);
		cur_column = cur_column->next;
	} while (cur_column != start_column);

	/* Note that we no longer need lvn nodes that help track constraint names (subs[0] = OCTOLIT_COLUMNS)
	 * but we still need lvn nodes that help track function names/hashes (subs[0] = OCTOLIT_FUNCTIONS) until
	 * "store_table_in_pg_class()" time so we will not remove these nodes until then.
	 */

	if (0 == num_user_visible_columns) {
		UNPACK_SQL_STATEMENT(value, table->tableName, value);
		ERROR(ERR_TABLE_MUST_HAVE_A_VISIBLE_COLUMN, value->v.reference);
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
		assert(NULL == primary_key_constraint_col);
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
			SqlStatement *colname_stmt, *data_type_stmt, *keycol_stmt;

			SQL_STATEMENT(colname_stmt, value_STATEMENT);
			OCTO_CMALLOC_STRUCT(colname_stmt->v.value, SqlValue);
			colname_stmt->v.value->type = COLUMN_REFERENCE;
			/* Note: sizeof of a string literal will include null terminator so that too gets copied in memcpy below */
			colname_stmt->v.value->v.string_literal = octo_cmalloc(memory_chunks, sizeof(HIDDEN_KEY_COL_NAME));
			memcpy(colname_stmt->v.value->v.string_literal, HIDDEN_KEY_COL_NAME, sizeof(HIDDEN_KEY_COL_NAME));

			data_type_stmt = data_type(INTEGER_TYPE, NULL, NULL);

			SQL_STATEMENT(keycol_stmt, column_STATEMENT);
			MALLOC_STATEMENT(keycol_stmt, column, SqlColumn);
			dqinit(keycol_stmt->v.column);
			keycol_stmt->v.column->columnName = colname_stmt;
			keycol_stmt->v.column->data_type_struct = data_type_stmt->v.data_type_struct;
			keycol_stmt->v.column->delim = NULL;
			keycol_stmt->v.column->is_hidden_keycol = TRUE;

			/* Note this column as a PRIMARY KEY (i.e. KEY NUM 0) */
			ADD_KEY_NUM_KEYWORD_TO_COLUMN(keycol_stmt->v.column, 0);

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
			int	   i;
			SqlColumn *cur_column, *start_column;

			assert(TABLETYPE_READONLY == table_type);
			UNPACK_SQL_STATEMENT(start_column, table->columns, column);
			cur_column = start_column;
			i = 0;
			do {
				/* If the current column exist only to capture a table-level constraint, do not include
				 * it in the list of key columns for this table.
				 */
				if (NULL != cur_column->columnName) {
					ADD_KEY_NUM_KEYWORD_TO_COLUMN(cur_column, i);
					i++;
				}
				cur_column = cur_column->next;
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
	dependencies = NULL;
	do {
		boolean_t	    delim_is_empty, remove_piece_keyword, is_extract;
		SqlOptionalKeyword *keyword, *piece_keyword;

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
			SqlValueType type;

			is_extract = TRUE;
			if ((value_STATEMENT == keyword->v->type) && (CALCULATED_VALUE == keyword->v->v.value->type)) {
				SqlValueType col_type;

				/* Check function call return type against column type and
				 * issue error if mismatch.
				 */
				UNPACK_SQL_STATEMENT(value, keyword->v, value);
				status = function_call_data_type_check(value->v.calculated, &type, NULL, table);
				if (status) {
					// Error issued by function_call_data_type_check
					return NULL;
				}
				col_type = get_sqlvaluetype_from_sqldatatype(cur_column->data_type_struct.data_type, TRUE);
				if (col_type != type) {
					ERROR(ERR_EXTRACT_TYPE_MISMATCH, get_user_visible_type_string(col_type),
					      get_user_visible_type_string(type));
					yyerror(&keyword->v->loc, NULL, NULL, NULL, NULL, NULL);
					return NULL;
				}

				// Store pointer to SqlStatement containing EXTRACT specification
				ydb_buffer_t func_subs[2];
				YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS, &func_subs[0]);
				func_subs[1].buf_addr = (char *)&keyword->v;
				func_subs[1].len_used = func_subs[1].len_alloc = sizeof(void *);
				status = ydb_set_s(&ydboctoTblExtract, 1, &func_subs[0], &func_subs[1]);
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					assert(FALSE);
					return NULL;
				}
				/* Note down the mapping between an EXTRACT function call (the 8-byte pointer) and its name.
				 * This is needed later in "store_table_in_pg_class.c" to know which EXTRACT function uses which
				 * function names/hashes.
				 */
				ydb_buffer_t map_subs[2];
				YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS_MAP, &map_subs[0]);
				YDB_STRING_TO_BUFFER(cur_column->columnName->v.value->v.string_literal, &subs[1]);
				map_subs[1].buf_addr = (char *)&keyword->v;
				map_subs[1].len_used = map_subs[1].len_alloc = sizeof(void *);
				/* Note: subs[1] contains the column name */
				status = ydb_set_s(&ydboctoTblExtract, 2, &map_subs[0], &subs[1]);
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					assert(FALSE);
					return NULL;
				}
				qualify_extract_function_cycle++;
				if (qualify_extract_function(keyword->v, table, &type, TRUE, NULL, cur_column->columnName,
							     &dependencies)) {
					ydb_buffer_t ydboctoTblExtract;
					YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOTBLEXTRACT, &ydboctoTblExtract);

					// Cleanup any nodes created by `qualify_extract_function()`
					status = ydb_delete_s(&ydboctoTblExtract, 1, &subs[0], YDB_DEL_TREE);
					assert(YDB_OK == status);
					YDB_ERROR_CHECK(status);
					return NULL; /* EXTRACT function qualification failed */
				}
			}
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
			if (!cur_column->is_hidden_keycol) {
				column_number++;
			}
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
			cur_column->column_number = column_number;
		} else {
			cur_column->column_number = 0;
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
				 * so should not set "readonly_disallowed" to TRUE here. In the case of READONLY, we don't
				 * enforce this constraint since updates to the global happen outside the scope of Octo.
				 */
				break;
			case NOT_NULL:
				/* NOT NULL is allowed for both READONLY and READWRITE type of tables
				 * so should not set "readonly_disallowed" to TRUE here. In the case of READONLY, we don't
				 * enforce this constraint since updates to the global happen outside the scope of Octo.
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
			case OPTIONAL_GENERATED_ALWAYS_IDENTITY:
			case OPTIONAL_GENERATED_BY_DEFAULT_IDENTITY:
				readonly_disallowed = TRUE; /* IDENTITY is only allowed for READWRITE table. Not READONLY. */
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

				char *out_buffer;
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
		int   i, total_copied;
		int   buffer_size, buffer2_size;
		char *buffer, *buffer2, *buff_ptr;
		char *start, *next;

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
				/* TODO: Currently we only use the table name to generate this hash. But we need to use the
				 *   schema name (YDBOcto#99) and database name (YDBOcto#417) when support for those are added.
				 *   See below comments for details.
				 *	https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/99#note_1072582652
				 *	https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/417#note_1072589179
				 *
				 * TODO: Additionally we might need to migrate data from the old global names that used to
				 *   previously maintain the user data to the new global names when YDBOcto#99 and
				 *   YDBOcto#417 are each implemented. Maybe as part of the auto-upgrade logic or so.
				 *   An alternative to avoid data migration is to skip including the database and/or
				 *   schema name in the hash when the defaults are in use. See below comment for details.
				 *	https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/417#note_1072666858
				 */
				hash128_state_t state;
				HASH128_STATE_INIT(state, 0);

				int len;
				len = strlen(value->v.reference);
				ydb_mmrhash_128_ingest(&state, (void *)value->v.reference, len);

				char table_global[MAX_ROUTINE_LEN + 2];
				generate_name_type(TableGlobal, &state, len, table_global, sizeof(table_global));
				SNPRINTF_TO_BUFFER(table_global, buffer, buff_ptr, buffer_size, total_copied);
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
			char *out_buffer;
			out_buffer = octo_cmalloc(memory_chunks, len);
			memcpy(out_buffer, buffer, len);
			MALLOC_KEYWORD_STMT(statement, OPTIONAL_SOURCE);
			SQL_VALUE_STATEMENT(statement->v.keyword->v, STRING_LITERAL, out_buffer);
			assert(NULL == table->source);
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

		char *out_buffer;
		out_buffer = octo_cmalloc(memory_chunks, str_len);
		out_buffer[0] = DELIM_IS_LITERAL;
		MEMCPY_LIT(&out_buffer[1], COLUMN_DELIMITER);
		out_buffer[str_len - 1] = '\0';
		MALLOC_KEYWORD_STMT(statement, OPTIONAL_DELIM);
		SQL_VALUE_STATEMENT(statement->v.keyword->v, STRING_LITERAL, out_buffer);
		assert(NULL == table->delim);
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
			} else if (readonly_disallowed && (TABLETYPE_READONLY == table_type)) {
				/* If a READONLY incompatible column-level keyword has been specified and
				 * if octo.conf setting for tabletype is set to READONLY then this is a clear
				 * indication of incompatibility.
				 */
				ERROR(ERR_READONLY_DISALLOWED, NULL);
				return NULL;
			}
			/* If READWRITE incompatible column-level keyword has been specified, assume READONLY.
			 * If READONLY incompatible column-level keyword has been specified, assume READWRITE.
			 * If not, assume value based on octo.conf "tabletype" setting.
			 */
			table->readwrite = (readwrite_disallowed  ? FALSE
					    : readonly_disallowed ? TRUE
								  : (TABLETYPE_READWRITE == table_type));
		}
	} else {
		table->readwrite = (TABLETYPE_READWRITE == table_type);
	}
	return table_stmt;
}
