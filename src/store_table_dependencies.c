/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#include "octo.h"
#include "octo_types.h"
#include "helpers.h"

int store_table_dependencies(SqlTable *table, char *table_name, ydb_buffer_t *table_name_buffer) {
	int status = store_function_dependencies(table_name, DDL_CheckConstraint);
	if (status) {
		return 1;
	}
	status = store_function_dependencies(table_name, DDL_ExtractFunction);
	if (status) {
		return 1;
	}
	/* Store the PRIMARY KEY constraint name for this table in a global so we can ensure unique PRIMARY KEY
	 * constraint names across all tables in Octo. Note that this will change once schema support is added.
	 * See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/770#note_1095422448 for more details.
	 * Below is an example gvn node (where "NAMES" is the table name)
	 *	^%ydboctoocto("primary_key_name","NAMES_ID_PKEY")="NAMES"
	 */
	ydb_buffer_t pkey_subs[4];
	YDB_STRING_TO_BUFFER(config->global_names.octo, &pkey_subs[0]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PRIMARY_KEY_NAME, &pkey_subs[1]);

	/* Find the PRIMARY KEY constraint in this table */
	char	  *primary_key_constraint_name;
	SqlColumn *start_column, *cur_column;
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	primary_key_constraint_name = NULL;
	do {
		SqlOptionalKeyword *cur_keyword, *start_keyword;
		UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
		cur_keyword = start_keyword;
		do {
			if (PRIMARY_KEY == cur_keyword->keyword) {
				SqlConstraint *constraint;
				UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);

				SqlValue *value;
				UNPACK_SQL_STATEMENT(value, constraint->name, value);
				primary_key_constraint_name = value->v.string_literal;
				break;
			}
			cur_keyword = cur_keyword->next;
		} while (cur_keyword != start_keyword);
		if (NULL != primary_key_constraint_name) {
			break;
		}
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	if (NULL != primary_key_constraint_name) {
		/* A PRIMARY KEY constraint keyword exists in the table (must be the only one keyword) */
		YDB_STRING_TO_BUFFER(primary_key_constraint_name, &pkey_subs[2]);
		YDB_STRING_TO_BUFFER(table_name, &pkey_subs[3]);
		status = ydb_set_s(&pkey_subs[0], 2, &pkey_subs[1], &pkey_subs[3]);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		/* Store cross reference to the above gvn node so it is easy for DROP TABLE to know which node to remove.
		 * Below is an example gvn node (where "NAMES" is the table name)
		 *	^%ydboctoschema("NAMES","primary_key_name")="NAMES_ID_PKEY"
		 */
		YDB_STRING_TO_BUFFER(config->global_names.schema, &pkey_subs[0]);
		YDB_STRING_TO_BUFFER(table_name, &pkey_subs[1]);
		YDB_STRING_TO_BUFFER(OCTOLIT_PRIMARY_KEY_NAME, &pkey_subs[2]);
		YDB_STRING_TO_BUFFER(primary_key_constraint_name, &pkey_subs[3]);
		status = ydb_set_s(&pkey_subs[0], 2, &pkey_subs[1], &pkey_subs[3]);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
	}
	/* else: It is possible no PRIMARY KEY constraint keyword exists in case the user did not specify one.
	 * In that case, we don't need to store anything about PRIMARY KEY names for this table in gvn nodes.
	 */

	/* Initialize auto-incrementing column by setting up a gvn which tracks the auto-increment value for columns
	 * with IDENTITY keyword usage.
	 */
	SqlValue *value;
	cur_column = start_column;
	do {
		if (IS_COLUMN_IDENTITY(cur_column)) {
			/* Add IDENTITY value `0` to the following gvn. This will be the initial value of this auto-incrementing
			 * column. Everytime a row is added, $INCREMENT of the following gvn is stored as the column value
			 * 	^%ydboctoschema("NAMES", "identity",col_name)=0
			 */
			ydb_buffer_t schema, subs[4];
			YDB_STRING_TO_BUFFER(config->global_names.schema, &schema);
			subs[0] = *table_name_buffer;
			YDB_LITERAL_TO_BUFFER(OCTOLIT_IDENTITY, &subs[1]);

			UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
			YDB_STRING_TO_BUFFER(value->v.string_literal, &subs[2]);
			// Value to store
			YDB_LITERAL_TO_BUFFER(OCTOLIT_0, &subs[3]);
			status = ydb_set_s(&schema, 3, subs, &subs[3]);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				assert(FALSE);
				return 1;
			}
		}
		cur_column = cur_column->next;
	} while (cur_column != start_column);

	return 0;
}
