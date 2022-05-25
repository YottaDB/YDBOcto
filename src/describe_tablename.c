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
	/* The below output is more or less the same as what \d tablename outputs at the psql prompt */
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
	return 0;
}
