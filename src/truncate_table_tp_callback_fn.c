/****************************************************************
 *								*
 * Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdlib.h>
#include <assert.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"

int truncate_table_tp_callback_fn(SqlStatement *truncate_stmt) {
	SqlTruncateTableStatement *truncate_table;
	SqlColumnList *		   column_list, *cur_table;

	// Get the list of tables to truncate
	UNPACK_SQL_STATEMENT(truncate_table, truncate_stmt, truncate_table);
	UNPACK_SQL_STATEMENT(column_list, truncate_table->tables, column_list);
	cur_table = column_list;
	do {
		char *	      tablename;
		SqlTable *    table;
		SqlStatement *table_stmt;

		// Truncate each table in the list, if it exists. If not, let the user know that it doesn't exist.
		tablename = cur_table->value->v.value->v.reference;
		table_stmt = find_view_or_table(tablename);
		if (NULL == table_stmt) {
			ERROR(ERR_UNKNOWN_TABLE, tablename);
			return YDB_TP_ROLLBACK;
		} else {
			if (create_table_STATEMENT != table_stmt->type) {
				ERROR(ERR_WRONG_TYPE, tablename, OCTOLIT_TABLE);
				return YDB_TP_ROLLBACK;
			} else {
				assert(NULL != table_stmt->v.create_table);
				table = table_stmt->v.create_table;
			}
			if (table->readwrite) {
				char tableGVNAME[YDB_MAX_IDENT + 2]; // + 2: One for ^, one for null byte. YDB_MAX_IDENT
								     // does not include ^.
				ydb_buffer_t gvname_buff;

				// Kill the GVN that holds the row data for the given table
				POPULATE_GVN_BUFFER_FROM_TABLE(gvname_buff, table, tableGVNAME);
				ydb_delete_s(&gvname_buff, 0, NULL, YDB_DEL_TREE);
			} else {
				ERROR(ERR_TABLE_READONLY, "TRUNCATE", tablename);
				return YDB_TP_ROLLBACK;
			}
		}
		cur_table = cur_table->next;
	} while (cur_table != column_list);
	return YDB_OK;
}
