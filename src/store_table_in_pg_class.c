/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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

#define CLEANUP_AND_RETURN(PG_CLASS, OID_BUFFER) \
	{                                        \
		YDB_FREE_BUFFER(&PG_CLASS[4]);   \
		free(PG_CLASS);                  \
		free(OID_BUFFER);                \
		return 1;                        \
	}

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, PG_CLASS, OID_BUFFER) \
	{                                                              \
		YDB_ERROR_CHECK(STATUS);                               \
		if (YDB_OK != STATUS) {                                \
			CLEANUP_AND_RETURN(PG_CLASS, OID_BUFFER);      \
		}                                                      \
	}

#define BUFFER_SIZE 1024

/* Attempts to store a row in pg_catalog.pg_class for this table.
 * Note that this function is similar to store_function_in_pg_proc.
 */
int store_table_in_pg_class(SqlTable *table, ydb_buffer_t *table_name_buffer) {
	int		  status;
	SqlValue *	  value;
	SqlColumn *	  start_column;
	SqlColumn *	  cur_column;
	ydb_buffer_t *	  oid_buffer;
	ydb_buffer_t *	  pg_class;
	ydb_buffer_t *	  pg_attribute;
	ydb_buffer_t	  buffer_b;
	ydb_buffer_t	  schema_global;
	ydb_buffer_t	  pg_class_schema[2], pg_attribute_schema[3];
	char *		  table_name;
	char		  buffer[BUFFER_SIZE];
	long long	  class_oid;
	long unsigned int copied;

	// Prepare buffers
	pg_class = make_buffers(config->global_names.octo, 4, OCTOLIT_TABLES, OCTOLIT_PG_CATALOG, OCTOLIT_PG_CLASS, "");
	oid_buffer = make_buffers(config->global_names.octo, 1, OCTOLIT_OID);
	YDB_MALLOC_BUFFER(&pg_class[4], INT64_TO_STRING_MAX);
	/* Get a unique oid TABLEOID for the passed in table.
	 * 	i.e. $INCREMENT(^%ydboctoocto(OCTOLIT_OID))
	 */
	status = ydb_incr_s(&oid_buffer[0], 1, &oid_buffer[1], NULL, &pg_class[4]);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, oid_buffer);
	pg_class[4].buf_addr[pg_class[4].len_used] = '\0';

	// Extract the table name
	UNPACK_SQL_STATEMENT(value, table->tableName, value);
	table_name = value->v.string_literal;
	// Convert table name to uppercase
	TOUPPER_STR(table_name);
	table_name = value->v.string_literal;
	/* These are hard-coded magic values related to the Postgres catalog.
	 * Columns of `pg_catalog.pg_class` table in `tests/fixtures/postgres.sql`.
	 * Any changes to that table definition will require changes here too.
	 */
	copied = snprintf(buffer, sizeof(buffer),
			  "%s|2200|16388|0|16385|0|16386|0|0|0|0|16389|1|0|p|r|3|0|0|1|0|0|0|0|0|1|d|0|571|1||||%s", table_name,
			  pg_class[4].buf_addr);
	assert(sizeof(buffer) > copied);
	UNUSED(copied);
	buffer_b.len_alloc = buffer_b.len_used = copied;
	buffer_b.buf_addr = buffer;
	/* Set the table name passed in as having an oid of TABLEOID in the pg_catalog.
	 * 	i.e. SET ^%ydboctoocto(OCTOLIT_TABLES,OCTOLIT_PG_CATALOG,OCTOLIT_PG_CLASS,TABLEOID)=...
	 */
	status = ydb_set_s(&pg_class[0], 4, &pg_class[1], &buffer_b);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, oid_buffer);
	/* Store a cross reference of the TABLEOID in ^%ydboctoschema.
	 *	i.e. SET ^%ydboctoschema("NAMES",OCTOLIT_PG_CLASS)=TABLEOID
	 * That way a later DROP TABLE or CREATE TABLE NAMES can clean all ^%ydboctoocto and ^%ydboctoschema
	 * nodes created during the previous CREATE TABLE NAMES.
	 */
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	pg_class_schema[0] = *table_name_buffer;
	pg_class_schema[1] = pg_class[3];
	status = ydb_set_s(&schema_global, 2, pg_class_schema, &pg_class[4]);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, oid_buffer);

	class_oid = strtoll(pg_class[4].buf_addr, NULL, 10); /* copy over class OID before we start changing it for column OID */
	if ((LLONG_MIN == class_oid) || (LLONG_MAX == class_oid)) {
		ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), pg_class[4].buf_addr);
		CLEANUP_AND_RETURN(pg_class, oid_buffer);
	}
	table->oid = class_oid; /* Initialize oid in SqlTable. Caller later invokes "compress_statement()" that stores this as
				 * part of the binary table definition in the database.
				 */
	// We should also store the column definitions in the pg_attribute table
	pg_attribute = make_buffers(config->global_names.octo, 4, OCTOLIT_TABLES, OCTOLIT_PG_CATALOG, OCTOLIT_PG_ATTRIBUTE, "");
	pg_attribute[4] = pg_class[4]; /* Inherit ydb_buffer used for OID */
	pg_attribute_schema[0] = *table_name_buffer;
	pg_attribute_schema[1] = pg_attribute[3];
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		int   atttypid;
		char *column_name;

		switch (cur_column->data_type_struct.data_type) {
		/* Below atttypid values were obtained from Postgres using the below query.
		 *	`select typname,oid from pg_type where typname in ('numeric','int4','varchar','bool');`
		 */
		case BOOLEAN_TYPE:
			atttypid = 16;
			break;
		case INTEGER_TYPE:
			atttypid = 23;
			break;
		case STRING_TYPE:
			atttypid = 1043;
			break;
		case NUMERIC_TYPE:
			atttypid = 1700;
			break;
		default:
			assert(FALSE);
			status = 1;
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			break;
		}
		if (YDB_OK != status) {
			break;
		}
		UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
		column_name = value->v.string_literal;
		// Convert name to upper case
		TOUPPER_STR(column_name);
		column_name = value->v.string_literal;
		/* Store table oid, column name, type,
		 * These are hard-coded magic values related to the Postgres catalog
		 * Columns of `pg_catalog.pg_attribute` table in `tests/fixtures/postgres.sql`.
		 * Any changes to that table definition will require changes here too.
		 */
		copied = snprintf(buffer, sizeof(buffer), "%lld|%s|%d|-1|-1|2|0|-1|-1|0|x|i|0|0|0|\"\"|0|1|0|100||||", class_oid,
				  column_name, atttypid);
		assert(sizeof(buffer) > copied);
		UNUSED(copied);
		/* Get a unique oid COLUMNOID for each column in the table.
		 * 	i.e. $INCREMENT(^%ydboctoocto(OCTOLIT_OID))
		 */
		status = ydb_incr_s(&oid_buffer[0], 1, &oid_buffer[1], NULL, &pg_attribute[4]);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			break;
		}
		/* Set the column name as having an oid of COLUMNOID in the pg_catalog.
		 * 	i.e. SET ^%ydboctoocto(OCTOLIT_TABLES,OCTOLIT_PG_CATALOG,OCTOLIT_PG_ATTRIBUTE,COLUMNOID)=...
		 */
		status = ydb_set_s(&pg_attribute[0], 4, &pg_attribute[1], &buffer_b);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			break;
		}
		/* Store a cross reference of the COLUMNOID in ^%ydboctoschema.
		 *	i.e. SET^ %ydboctoschema(TABLENAME,OCTOLIT_PG_ATTRIBUTE,COLUMNNAME)=COLUMNOID
		 */
		column_name = value->v.string_literal;
		// Convert name to upper case
		TOUPPER_STR(column_name);
		column_name = value->v.string_literal;
		YDB_STRING_TO_BUFFER(column_name, &pg_attribute_schema[2]);
		status = ydb_set_s(&schema_global, 3, &pg_attribute_schema[0], &pg_attribute[4]);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			break;
		}
		cur_column = cur_column->next;
	} while (cur_column != start_column);
	YDB_FREE_BUFFER(&pg_class[4]);
	free(oid_buffer);
	free(pg_class);
	free(pg_attribute);
	if (YDB_OK != status) {
		return 1;
	}
	return 0;
}
