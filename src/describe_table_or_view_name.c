/****************************************************************
 *								*
 * Copyright (c) 2022-2025 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <assert.h>

#include "octo.h"

/* This function implements the "\d tablename" and "\d viewname" command at the OCTO> prompt.
 *
 * Input
 * -----
 * "stmt" holds the table/view name.
 *
 * Output
 * ------
 * Displays to "stdout" the table/view definition details (column names, types etc.)
 *
 * Return
 * ------
 *  0 : Success
 * -1 : Error encountered (e.g. unknown table etc.)
 *
 */
int describe_table_or_view_name(SqlStatement *table_name) {
	SqlValue     *value;
	char	     *tablename;
	SqlTable     *table;
	SqlStatement *table_or_view_stmt;
	SqlColumn    *start_column, *cur_column;
	FILE	     *memstream;
	char	     *outbuf;
	size_t	      outsize;

	memstream = open_memstream(&outbuf, &outsize);
	if (NULL == memstream) {
		ERROR(ERR_SYSCALL_WITH_ARG, "open_memstream()", errno, strerror(errno), "memstream");
		return -1;
	}

	UNPACK_SQL_STATEMENT(value, table_name, value);
	tablename = value->v.reference;
	table_or_view_stmt = find_view_or_table(tablename);
	if (NULL == table_or_view_stmt) {
		ERROR(ERR_UNKNOWN_TABLE_OR_VIEW, tablename);
		fclose(memstream);
		free(outbuf);
		return -1;
	}
	if (create_view_STATEMENT == table_or_view_stmt->type) {
		SqlView *view;
		UNPACK_SQL_STATEMENT(view, table_or_view_stmt, create_view);
		UNPACK_SQL_STATEMENT(value, view->viewName, value);
		/* Note: The below output is more or less the same as what \d viewname outputs at the psql prompt */

		/* First output view name */
		fprintf(memstream, "View \"%s\"\n", value->v.reference);

		/* Next output the table columns */
		fprintf(memstream, "Column|Type|Collation|Nullable|Default\n");

		SqlTableAlias *table_alias;
		boolean_t      is_set_oper = FALSE;
		if (set_operation_STATEMENT == view->src_table_alias_stmt->type) {
			SqlStatement *stmt = drill_to_table_alias(view->src_table_alias_stmt);
			UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
			/* Used to avoid precise column specification as in cases where the precision and scale vary between
			 * columns of different branches of the set_operation_STATEMENT, we will have to do more processing
			 * to identify that type is same or different. To avoid this computation and still output a reasonable type
			 * information we display only the type without any precision or scale.
			 */
			is_set_oper = TRUE;
		} else {
			UNPACK_SQL_STATEMENT(table_alias, view->src_table_alias_stmt, table_alias);
		}

		SqlColumnListAlias *start_cla, *cur_cla;
		UNPACK_SQL_STATEMENT(start_cla, table_alias->column_list, column_list_alias);
		cur_cla = start_cla;
		do {
			UNPACK_SQL_STATEMENT(value, cur_cla->alias, value);
			fprintf(memstream, "%s|", value->v.reference); /* "Column" column */

			SqlColumn *col;
			col = get_column_under_column_list_alias(cur_cla);
			if ((NULL == col) || (is_set_oper)) {
				// Get just the column type
				fprintf(memstream, "%s|", get_user_visible_type_string(cur_cla->type)); /* "Type" column */
			} else {
				// Get more precise column type
				int  ret;
				char data_type_string[MAX_USER_VISIBLE_TYPE_STRING_LEN];
				ret = get_user_visible_data_type_string(&col->data_type_struct, data_type_string,
									sizeof(data_type_string));
				if (0 > ret) {
					assert(FALSE);
					fclose(memstream);
					free(outbuf);
					return -1;
				}
				fprintf(memstream, "%s|", data_type_string); /* "Type" column */
			}
			fprintf(memstream,
				"|"); /* "Collation" column (currently empty as we don't yet support the COLLATE keyword) */
			fprintf(memstream, "|"); /* "Nullable" column is just kept for similarity with table display */
			fprintf(memstream, "|"); /* "Default" column is just kept for similarity with table display */
			fprintf(memstream, "\n");
			cur_cla = cur_cla->next;
		} while (cur_cla != start_cla);

		/* Next output the view definition */
		// fetch the text definition
		ydb_buffer_t view_name_buffer;
		UNPACK_SQL_STATEMENT(value, view->viewName, value);
		YDB_STRING_TO_BUFFER(value->v.reference, &view_name_buffer);

		char *text_definition = NULL;
		int   status = get_table_or_view_text_definition(&view_name_buffer, &text_definition);
		if (0 > status) {
			assert(FALSE);
			if (NULL != text_definition) {
				free(text_definition);
			}
			fclose(memstream);
			free(outbuf);
			return -1;
		}
		fprintf(memstream, "View definition:\n%s\n", text_definition);
		free(text_definition);
		fclose(memstream);
		SAFE_PRINTF(fprintf, stdout, FALSE, FALSE, "%s", outbuf);
		free(outbuf);
		return 0;
	} else {
		assert(create_table_STATEMENT == table_or_view_stmt->type);
		UNPACK_SQL_STATEMENT(table, table_or_view_stmt, create_table);
		UNPACK_SQL_STATEMENT(value, table->tableName, value);

		/* Note: The below output is more or less the same as what \d tablename outputs at the psql prompt */

		/* First output Column names, types etc. */
		fprintf(memstream, "Table \"%s\" stored in ", value->v.reference);

		/* Next output GLOBAL (could be subscripted) that holds the table records */
		describe_tablename_global(memstream, table);
		fprintf(memstream, " : Type = %s\n", (table->readwrite ? "READWRITE" : "READONLY"));
	}

	/* Next output the table columns */
	fprintf(memstream, "Column|Type|Collation|Nullable|Default\n");
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		/* Skip processing hidden columns AND columns that correspond to table constraints */
		if (!cur_column->is_hidden_keycol && (NULL != cur_column->columnName)) {
			UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
			fprintf(memstream, "%s|", value->v.reference); /* "Column" column */

			int  ret;
			char data_type_string[MAX_USER_VISIBLE_TYPE_STRING_LEN];
			ret = get_user_visible_data_type_string(&cur_column->data_type_struct, data_type_string,
								sizeof(data_type_string));
			if (0 > ret) {
				assert(FALSE);
				fclose(memstream);
				free(outbuf);
				outbuf = NULL;
				return -1;
			}

			fprintf(memstream, "%s|", data_type_string); /* "Type" column */
			fprintf(memstream,
				"|"); /* "Collation" column (currently empty as we don't yet support the COLLATE keyword) */

			char *nullable;
			nullable = (IS_COLUMN_NOT_NULL(cur_column) ? "NOT NULL" : "");
			fprintf(memstream, "%s|", nullable); /* "Nullable" column */

			char *default_str;
			if (IS_COLUMN_ALWAYS_IDENTITY(cur_column)) {
				default_str = "generated always as identity";
			} else if (IS_COLUMN_BY_DEFAULT_IDENTITY(cur_column)) {
				default_str = "generated by default as identity";
			} else {
				/* fprintf(stdout, ""); "Default" column is empty till YDBOcto#555 is implemented hence commented */
				default_str = "";
			}
			fprintf(memstream, "%s\n", default_str);
		}
		cur_column = cur_column->next;
	} while (cur_column != start_column);

	/* Next output UNIQUE constraints (if any) */
	boolean_t first_unique_constraint;
	char	 *buffer, *buffer_orig, **buff_ptr;
	int	  buffer_size, status;

	buffer_orig = NULL;
	first_unique_constraint = TRUE;
	cur_column = start_column;
	do {
		SqlOptionalKeyword *cur_keyword, *start_keyword;

		UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
		cur_keyword = start_keyword;
		do {
			switch (cur_keyword->keyword) {
			case PRIMARY_KEY:
				/* Note: A READONLY table uses the PRIMARY KEY constraint for SELECT queries to know the key
				 * columns but it does not actively maintain any indexes. But since it helps to know the
				 * primary key constraint name for READONLY tables too (for example, such a constraint name
				 * could prevent the same PRIMARY KEY constraint name from being specified for a different
				 * READWRITE type table and result in a ERR_DUPLICATE_PRIMARY_KEY_CONSTRAINT error) we display
				 * the PRIMARY KEY in the "Indexes" list below even though they are not actively maintained.
				 */
				/* Note: Below comment is needed to avoid gcc [-Wimplicit-fallthrough=] warning */
				/* fall through */
			case UNIQUE_CONSTRAINT:;
				if (first_unique_constraint) {
					fprintf(memstream, "Indexes:\n");
					buffer_size = OCTO_INIT_BUFFER_LEN;
					buffer = (char *)malloc(sizeof(char) * buffer_size);
					buffer_orig = buffer;
					first_unique_constraint = FALSE;
				}

				SqlConstraint *constraint;
				UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);
				assert(cur_keyword->keyword == constraint->type);
				fprintf(memstream, "    ");
				assert(NULL != constraint->name);

				UNPACK_SQL_STATEMENT(value, constraint->name, value);
				assert(value->is_double_quoted);
				fprintf(memstream, "\"%s\" ", value->v.string_literal);

				fprintf(memstream, "%s CONSTRAINT, Column(s) ",
					(UNIQUE_CONSTRAINT == constraint->type) ? "UNIQUE" : "PRIMARY KEY");
				buffer = buffer_orig;
				buff_ptr = &buffer;
				/* Although we are emitting a UNIQUE or PRIMARY KEY constraint, all we need to emit at this
				 * point is a list of column names and we have a "column_list_STATEMENT" type (asserted below).
				 * That can be emitted by "emit_check_constraint()" so we use that function even though
				 * it is a CHECK constraint specific function.
				 */
				assert(column_list_STATEMENT == constraint->definition->type);
				status = emit_check_constraint(&buffer_orig, &buffer_size, buff_ptr, constraint->definition);
				if (0 > status) {
					assert(FALSE);
					free(buffer);
					fclose(memstream);
					free(outbuf);
					outbuf = NULL;
					return -1;
				}
				fprintf(memstream, "%s", buffer_orig);
				fprintf(memstream, ", ");
				if (UNIQUE_CONSTRAINT == constraint->type) {
					fprintf(memstream, "Global ");
					UNPACK_SQL_STATEMENT(value, constraint->v.uniq_gblname, value);
					fprintf(memstream, "%s\n", value->v.string_literal);
				} else {
					assert(PRIMARY_KEY == constraint->type);
					describe_tablename_global(memstream, table);
					fprintf(memstream, "\n");
				}
				break;
			default:
				break;
			}
			cur_keyword = cur_keyword->next;
		} while (cur_keyword != start_keyword);
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	if (NULL != buffer_orig) {
		free(buffer_orig);
		buffer_orig = NULL;
	}

	/* Next output CHECK constraints (if any) */
	boolean_t first_check_constraint;

	assert(NULL == buffer_orig);
	first_check_constraint = TRUE;
	cur_column = start_column;
	do {
		SqlOptionalKeyword *cur_keyword, *start_keyword;

		UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
		cur_keyword = start_keyword;
		do {
			switch (cur_keyword->keyword) {
			case OPTIONAL_CHECK_CONSTRAINT:;
				if (first_check_constraint) {
					fprintf(memstream, "Check constraints:\n");
					buffer_size = OCTO_INIT_BUFFER_LEN;
					buffer = (char *)malloc(sizeof(char) * buffer_size);
					buffer_orig = buffer;
					first_check_constraint = FALSE;
				}

				SqlConstraint *constraint;
				UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);
				assert(OPTIONAL_CHECK_CONSTRAINT == constraint->type);
				UNPACK_SQL_STATEMENT(value, constraint->name, value);
				assert(value->is_double_quoted);
				fprintf(memstream, "    \"%s\" CHECK (", value->v.string_literal);
				buffer = buffer_orig;
				buff_ptr = &buffer;
				status = emit_check_constraint(&buffer_orig, &buffer_size, buff_ptr, constraint->definition);
				if (0 > status) {
					assert(FALSE);
					free(buffer);
					fclose(memstream);
					free(outbuf);
					return -1;
				}
				fprintf(memstream, "%s)\n", buffer_orig);
				/* Note that "constraint->v.check_columns" is information derived from "constraint->definition"
				 * and is not relevant to the user so is not displayed here. Hence no processing for that done here.
				 */
				break;
			default:
				break;
			}
			cur_keyword = cur_keyword->next;
		} while (cur_keyword != start_keyword);
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	if (NULL != buffer_orig) {
		free(buffer_orig);
	}

	fclose(memstream);
	SAFE_PRINTF(fprintf, stdout, FALSE, FALSE, "%s", outbuf);
	free(outbuf);
	outbuf = NULL;

	return 0;
}
