/****************************************************************
 *								*
 * Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	*
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

/* This function implements the "\d tablename" command at the OCTO> prompt.
 *
 * Input
 * -----
 * "stmt" holds the table name.
 *
 * Output
 * ------
 * Displays to "stdout" the table definition details (column names, types etc.)
 *
 * Return
 * ------
 *  0 : Success
 * -1 : Error encountered (e.g. unknown table etc.)
 *
 */
int describe_tablename(SqlStatement *table_name) {
	SqlValue * value;
	char *	   tablename;
	SqlTable * table;
	SqlColumn *start_column, *cur_column;

	UNPACK_SQL_STATEMENT(value, table_name, value);
	tablename = value->v.reference;
	table = find_table(tablename);
	if (NULL == table) {
		ERROR(ERR_UNKNOWN_TABLE, tablename);
		return -1;
	}
	UNPACK_SQL_STATEMENT(value, table->tableName, value);

	/* Note: The below output is more or less the same as what \d tablename outputs at the psql prompt */

	/* First output Column names, types etc. */
	fprintf(stdout, "Table \"%s\"\n", value->v.reference);
	fprintf(stdout, "Column|Type|Collation|Nullable|Default\n");
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		/* Skip processing hidden columns AND columns that correspond to table constraints */
		if (!cur_column->is_hidden_keycol && (NULL != cur_column->columnName)) {
			UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
			fprintf(stdout, "%s|", value->v.reference); /* "Column" column */

			int  ret;
			char data_type_string[MAX_USER_VISIBLE_TYPE_STRING_LEN];
			ret = get_user_visible_data_type_string(&cur_column->data_type_struct, data_type_string,
								sizeof(data_type_string));
			if (0 > ret) {
				assert(FALSE);
				return -1;
			}

			fprintf(stdout, "%s|", data_type_string); /* "Type" column */
			fprintf(stdout, "|"); /* "Collation" column (currently empty as we don't yet support the COLLATE keyword) */

			char *nullable;
			nullable = (IS_COLUMN_NOT_NULL(cur_column) ? "NOT NULL" : "");
			fprintf(stdout, "%s|", nullable); /* "Nullable" column */

			/* fprintf(stdout, ""); "Default" column is empty till YDBOcto#555 is implemented hence commented */
			fprintf(stdout, "\n");
		}
		cur_column = cur_column->next;
	} while (cur_column != start_column);

	/* Next output constraints (if any) */
	boolean_t first_check_constraint;
	char *	  buffer, *buffer_orig, **buff_ptr;
	int	  buffer_size, status;

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
					fprintf(stdout, "Check constraints:\n");
					buffer_size = OCTO_INIT_BUFFER_LEN;
					buffer = (char *)malloc(sizeof(char) * buffer_size);
					buffer_orig = buffer;
					first_check_constraint = FALSE;
				}

				SqlConstraint *constraint;
				SqlValue *     value;
				UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);
				UNPACK_SQL_STATEMENT(value, constraint->name, value);
				fprintf(stdout, "    \"%s\" CHECK (", value->v.string_literal);
				buffer = buffer_orig;
				buff_ptr = &buffer;
				status = emit_check_constraint(&buffer, &buffer_size, buff_ptr, constraint->definition);
				if (0 > status) {
					assert(FALSE);
					free(buffer);
					return -1;
				}
				fprintf(stdout, "%s)\n", buffer_orig);
				break;
			default:
				break;
			}
			cur_keyword = cur_keyword->next;
		} while (cur_keyword != start_keyword);
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	return 0;
}
