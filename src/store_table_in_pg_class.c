/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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

/**
 * Attempts to store a row in pg_catalog.pg_class for this table
 */
int store_table_in_pg_class(SqlTable *table) {
	int		status;
	SqlValue	*value;
	SqlColumn	*start_column;
	SqlColumn	*cur_column;
	ydb_buffer_t	*oid_buffer;
	ydb_buffer_t	*pg_class;
	ydb_buffer_t	*pg_attribute;
	ydb_buffer_t	buffer_b;
	char		*table_name;
	char		buffer[MAX_STR_CONST];

	// Prepare buffers
	pg_class = make_buffers(config->global_names.octo, 4, "tables", "pg_catalog", "pg_class", "");
	oid_buffer = make_buffers(config->global_names.octo, 1, "oid");
	YDB_MALLOC_BUFFER(&pg_class[4], MAX_STR_CONST);
	status = ydb_incr_s(&oid_buffer[0], 1, &oid_buffer[1], NULL, &pg_class[4]);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		YDB_FREE_BUFFER(&pg_class[4]);
		free(pg_class);
		free(oid_buffer);
		return 1;
	}
	pg_class[4].buf_addr[pg_class[4].len_used] = '\0';

	// Extract the table name
	UNPACK_SQL_STATEMENT(value, table->tableName, value);
	table_name = value->v.string_literal;
	// Convert table name fo uppercase
	while(*table_name != '\0') {
		*table_name = toupper(*table_name);
		table_name++;
	}
	table_name = value->v.string_literal;
	// These are hard-coded magic values related to the Postgres catalog
	snprintf(buffer, sizeof(buffer),
		"%s|2200|16388|0|16385|0|16386|0|0|0|0|16389|t|0|p|r|3|0|0|0|0|0|0|0|t|d|0|0|571|1||||%s",
		table_name, pg_class[4].buf_addr);
	buffer_b.len_alloc = buffer_b.len_used = strlen(buffer);
	buffer_b.buf_addr = buffer;
	status = ydb_set_s(&pg_class[0], 4, &pg_class[1], &buffer_b);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		YDB_FREE_BUFFER(&pg_class[4]);
		free(pg_class);
		free(oid_buffer);
		return 1;
	}

	// We should also store the column definitions in the pg_attribute table
	pg_attribute = make_buffers(config->global_names.octo, 4, "tables", "pg_catalog", "pg_attribute", "");
	YDB_MALLOC_BUFFER(&pg_attribute[4], MAX_STR_CONST);
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		int atttypid;
		switch(cur_column->type) {
			case NUMERIC_TYPE:
				atttypid = 1700;
				break;
			case INTEGER_TYPE:
				atttypid = 23;
				break;
			case CHARACTER_STRING_TYPE:
				atttypid = 1043;
				break;
			case DATE_TIME_TYPE:
			case INTERVAL_TYPE:
			case UNKNOWN_SqlDataType:
				status = 1;
				ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
				break;
		}
		if (YDB_OK != status)
			break;
		UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
		char *column_name = value->v.string_literal;
		// Convert name to upper case
		while(*column_name != '\0') {
			*column_name = toupper(*column_name);
			column_name++;
		}
		column_name = value->v.string_literal;
		// Store table oid, column name, type,
		snprintf(buffer, sizeof(buffer), "%s|%s|%d|-1|-1|2|0|-1|-1|0|x|i|0|0|0|\"\"|0|t|0|100||||",
				pg_class[4].buf_addr, column_name, atttypid);
		status = ydb_incr_s(&oid_buffer[0], 1, &oid_buffer[1], NULL, &pg_attribute[4]);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		status = ydb_set_s(&pg_attribute[0], 4, &pg_attribute[1], &buffer_b);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		cur_column = cur_column->next;
	} while(cur_column != start_column);
	YDB_FREE_BUFFER(&pg_class[4]);
	YDB_FREE_BUFFER(&pg_attribute[4]);
	free(oid_buffer);
	free(pg_class);
	free(pg_attribute);
	if (YDB_OK != status)
		return 1;
	return 0;
}
