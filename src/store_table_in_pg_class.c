/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

/* The length of a column of the given type, in bytes.
 * A value of -1 signifies a variable length attribute.
 *
 * Documented in the pg_type PostgreSQL table:
 *   select oid, typname, typlen from pg_type;
 */
typedef enum PG_AttributeLength {
	bool_AttributeLength = 1,
	int4_AttributeLength = 4,
	varchar_AttributeLength = -1,
	numeric_AttributeLength = -1,
} PG_AttributeLength;

/* The type id of each supported PostgreSQL type, as documented in the
 * pg_type PostgreSQL table:
 *   select oid, typname, typlen from pg_type;
 */
typedef enum PG_AttributeTypeId {
	bool_AttributeTypeId = 16,
	int4_AttributeTypeId = 23,
	varchar_AttributeTypeId = 1043,
	numeric_AttributeTypeId = 1700,
} PG_AttributeTypeId;

typedef enum PG_AttributeByVal {
	bool_AttributeByVal = TRUE,
	int4_AttributeByVal = TRUE,
	varchar_AttributeByVal = FALSE,
	numeric_AttributeByVal = FALSE,
} PG_AttributeByVal;

typedef enum PG_AttributeStorage {
	bool_AttributeStorage = 'p',
	int4_AttributeStorage = 'p',
	varchar_AttributeStorage = 'x',
	numeric_AttributeStorage = 'm',
} PG_AttributeStorage;

typedef enum PG_AttributeAlign {
	bool_AttributeAlign = 'c',
	int4_AttributeAlign = 'i',
	varchar_AttributeAlign = 'i',
	numeric_AttributeAlign = 'i',
} PG_AttributeAlign;

/* Attempts to store a row in pg_catalog.pg_class for this table.
 * Note that this function is similar to store_function_in_pg_proc.
 */
int store_table_in_pg_class(SqlTable *table, ydb_buffer_t *table_name_buffer) {
	int		  status, attnum;
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
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&pg_class[4], INT64_TO_STRING_MAX);
	/* Get a unique oid TABLEOID for the passed in table.
	 * 	i.e. $INCREMENT(^%ydboctoocto(OCTOLIT_OID))
	 */
	status = ydb_incr_s(&oid_buffer[0], 1, &oid_buffer[1], NULL, &pg_class[4]);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, oid_buffer);
	pg_class[4].buf_addr[pg_class[4].len_used] = '\0';

	// Extract the table name
	UNPACK_SQL_STATEMENT(value, table->tableName, value);
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
	/* The column number of the given column, indexed from 1.
	 * Implemented per the following description of `attnum`:
	 *	"The number of the column. Ordinary columns are numbered from 1 up."
	 *
	 * This description was quoted from:
	 *	https://www.postgresql.org/docs/9.6/catalog-pg-attribute.html
	 */
	attnum = 1;
	do {
		/* `atttypmod` is for storing type-specific information about the given column, per the PostgreSQL docs:
		 *
		 * "atttypmod records type-specific data supplied at table creation time
		 * (for example, the maximum length of a varchar column). It is passed to
		 * type-specific input functions and length coercion functions. The value
		 * will generally be -1 for types that do not need atttypmod."
		 *
		 * Source: https://www.postgresql.org/docs/9.6/catalog-pg-attribute.html
		 */
		int   attnotnull;
		char  attalign;
		char  attstorage;
		int   attbyval;
		int   atttypmod;
		int   atttypid;
		int   attlen;
		char *column_name;

		attnotnull = IS_COLUMN_NOT_NULL(cur_column);
		atttypmod = -1; // The default value for the atttypmod column
		if (NULL != cur_column->columnName) {
			switch (cur_column->data_type_struct.data_type) {
			/* Below atttypid values were obtained from Postgres using the below query.
			 *	`select typname,oid from pg_type where typname in ('numeric','int4','varchar','bool');`
			 */
			case BOOLEAN_TYPE:
				atttypid = bool_AttributeTypeId;
				attlen = bool_AttributeLength;
				attbyval = bool_AttributeByVal;
				attstorage = bool_AttributeStorage;
				attalign = bool_AttributeAlign;
				break;
			case INTEGER_TYPE:
				atttypid = int4_AttributeTypeId;
				attlen = int4_AttributeLength;
				attbyval = int4_AttributeByVal;
				attstorage = int4_AttributeStorage;
				attalign = int4_AttributeAlign;
				break;
			case STRING_TYPE:
				atttypid = varchar_AttributeTypeId;
				attlen = varchar_AttributeLength;
				if (SIZE_OR_PRECISION_UNSPECIFIED != cur_column->data_type_struct.size_or_precision) {
					/* The + 4 below is done to match Postgres output (I guess it needs this space
					 * to store a 4-byte integer internally).
					 */
					atttypmod = cur_column->data_type_struct.size_or_precision + 4;
				}
				attbyval = varchar_AttributeByVal;
				attstorage = varchar_AttributeStorage;
				attalign = varchar_AttributeAlign;
				break;
			case NUMERIC_TYPE:
				atttypid = numeric_AttributeTypeId;
				attlen = numeric_AttributeLength;
				if (SIZE_OR_PRECISION_UNSPECIFIED != cur_column->data_type_struct.size_or_precision) {
					if (SCALE_UNSPECIFIED == cur_column->data_type_struct.scale) {
						atttypmod = 0;
					} else {
						atttypmod = cur_column->data_type_struct.scale;
					}
					/* The + 4 below is done to match Postgres output (I guess it needs this space
					 * to store a 4-byte integer internally).
					 */
					atttypmod += 4;
					/* The << 16 below is also done to match Postgres output */
					atttypmod += (cur_column->data_type_struct.size_or_precision << 16);
				}
				attbyval = numeric_AttributeByVal;
				attstorage = numeric_AttributeStorage;
				attalign = numeric_AttributeAlign;
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
			/* Store table attribute information in `pg_catalog.pg_attribute`.
			 *
			 * Columns of `pg_catalog.pg_attribute` table in `tests/fixtures/postgres.sql`.
			 * Any changes to that table definition will require changes here too.
			 *
			 * Most columns contain values that are not respected or enforced by Octo,
			 * but are nonetheless requested by some SQL clients. So, we here try to
			 * give the best/most coherent value for these columns given Octo's incomplete
			 * PostgreSQL feature set.
			 *
			 * Note also that the atttypid, atttypmod, attbyval, and attstorage columns ought to contain values
			 * reflected in `pg_type`, though Octo doesn't use any of these values itself. Similarly,
			 * `attcollation` and `attrelid` (`column_name` below) relate to the `pg_collation` and `pg_class`
			 * tables, respectively.
			 *
			 * For the most part, we expect these intra-catalog relations to not affect clients, but if a client
			 * attempts to utilize these relations in queries, e.g. through JOIN conditions, it could cause issues
			 * if these various related tables are out of sync. That said, there are no known cases of such issues
			 * as of this writing.
			 */
			copied = snprintf(buffer, sizeof(buffer), "%lld|%s|%d|-1|%d|%d|0|-1|%d|%d|%c|%c|%d|0|0||1|0|0|0|||||",
					  class_oid, column_name, atttypid, attlen, attnum, atttypmod, attbyval, attstorage,
					  attalign, attnotnull);
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
			YDB_STRING_TO_BUFFER(column_name, &pg_attribute_schema[2]);
			status = ydb_set_s(&schema_global, 3, &pg_attribute_schema[0], &pg_attribute[4]);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				break;
			}
		}
		cur_column = cur_column->next;
		attnum++;
	} while (cur_column != start_column);
	YDB_FREE_BUFFER(&pg_class[4]);
	free(oid_buffer);
	free(pg_class);
	free(pg_attribute);
	if (YDB_OK != status) {
		return 1;
	}
	status = store_function_dependencies(table_name, DDL_CheckConstraint);
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
	char *primary_key_constraint_name;
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
