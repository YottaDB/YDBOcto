/****************************************************************
 *								*
 * Copyright (c) 2023-2026 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"

/* Executes a TRUNCATE TABLE statement: KILLs the row-data global of every READWRITE table named in
 * TRUNCATE_STMT. All the data globals are deleted in a single "_ydboctoKillGvnTree" M call-in that
 * KILLs them (KILL @gvnlist) inside a TSTART/TCOMMIT transaction, so the TRUNCATE is atomic. The delete
 * is done in M, like DROP TABLE, so an extended-reference GLOBAL is resolved by M name indirection.
 *
 * Returns YDB_OK on success. On failure returns the KILL call-in's YottaDB status, or 1 for an
 * octo-level validation error (unknown / wrong-type / read-only table); the error is also reported
 * via ERROR() / YDB_ERROR_CHECK() before returning.
 */
int truncate_tables(SqlStatement *truncate_stmt) {
	SqlTruncateTableStatement *truncate_table;
	SqlColumnList		  *column_list, *cur_table;
	ydb_buffer_t		   gvname_buff;
	ydb_string_t		   ci_gvnlist;
	char			  *gvnlist, *gvnptr;
	unsigned int		   gvnlist_len;
	int			   status;

	UNPACK_SQL_STATEMENT(truncate_table, truncate_stmt, truncate_table);
	UNPACK_SQL_STATEMENT(column_list, truncate_table->tables, column_list);

	/* First pass: validate every table named in the list (it exists, is a table, and is READWRITE)
	 * BEFORE any data is deleted. A validation failure therefore leaves every table's data intact,
	 * i.e. TRUNCATE of a list of tables is atomic with respect to validation errors (see TT02).
	 * Also accumulate the byte count needed for the comma-separated list of data globals built below.
	 */
	gvnlist_len = 0;
	cur_table = column_list;
	do {
		char	     *tablename;
		SqlStatement *table_stmt;
		SqlTable     *table;

		tablename = cur_table->value->v.value->v.reference;
		table_stmt = find_view_or_table(tablename);
		if (NULL == table_stmt) {
			ERROR(ERR_UNKNOWN_TABLE, tablename);
			return 1;
		}
		if (create_table_STATEMENT != table_stmt->type) {
			ERROR(ERR_WRONG_TYPE, tablename, OCTOLIT_TABLE);
			return 1;
		}
		assert(NULL != table_stmt->v.create_table);
		table = table_stmt->v.create_table;
		if (!table->readwrite) {
			ERROR(ERR_TABLE_READONLY, "TRUNCATE", tablename);
			return 1;
		}
		POPULATE_GVN_BUFFER_FROM_TABLE(gvname_buff, table);
		gvnlist_len += gvname_buff.len_used + 1; /* + 1 for the ',' separator between globals */
		cur_table = cur_table->next;
	} while (cur_table != column_list);

	/* Second pass: build a comma-separated list of the validated tables' data globals, then KILL
	 * them all in a single "_ydboctoKillGvnTree" M call-in. The delete is done in M (KILL @gvnlist), as
	 * DROP TABLE does via "discardTable^%ydboctoDiscard", so M name indirection resolves an
	 * extended-reference GLOBAL (^|"gld"|name).
	 *
	 * "run_query()" holds the DDL lock across this call, so no concurrent CREATE/DROP TABLE can run
	 * between the two passes: each table's GLOBAL -- hence the length summed above and the bytes
	 * copied below -- is identical in both passes.
	 */
	gvnlist = (char *)malloc(gvnlist_len);
	gvnptr = gvnlist;
	cur_table = column_list;
	do {
		SqlStatement *table_stmt;
		SqlTable     *table;

		table_stmt = find_view_or_table(cur_table->value->v.value->v.reference);
		assert(NULL != table_stmt); /* validated to exist (and be a READWRITE table) in the first pass */
		table = table_stmt->v.create_table;
		POPULATE_GVN_BUFFER_FROM_TABLE(gvname_buff, table);
		if (gvnptr != gvnlist) {
			*gvnptr++ = ','; /* separate this global from the previous one */
		}
		memcpy(gvnptr, gvname_buff.buf_addr, gvname_buff.len_used);
		gvnptr += gvname_buff.len_used;
		cur_table = cur_table->next;
	} while (cur_table != column_list);

	ci_gvnlist.address = gvnlist;
	ci_gvnlist.length = gvnptr - gvnlist;
	assert((gvnptr - gvnlist) == (gvnlist_len - 1));
	status = ydb_ci("_ydboctoKillGvnTree", &ci_gvnlist);
	free(gvnlist);
	if (YDB_OK != status) {
		YDB_ERROR_CHECK(status);
		return status; /* propagate the call-in's YottaDB status (cf. drop_schema_from_local_cache) */
	}
	return YDB_OK;
}
