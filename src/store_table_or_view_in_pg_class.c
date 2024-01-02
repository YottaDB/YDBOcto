/****************************************************************
 *								*
 * Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	*
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

#define SET_PG_ATTRIBUTE_INFO(BUFFER, CLASS_OID, COLUMN_NAME, ATTTYPID, ATTLEN, ATTNUM, ATTTYPMOD, ATTBYVAL, ATTSTORAGE, ATTALIGN, \
			      ATTNOTNULL, OID_BUFFER, PG_ATTRIBUTE, STATUS, BUFFER_B, PG_ATTRIBUTE_SCHEMA, COPIED)                 \
	{                                                                                                                          \
		COPIED = snprintf(BUFFER, sizeof(BUFFER), "%lld|%s|%d|-1|%d|%d|0|-1|%d|%d|%c|%c|%d|0|0||1|0|0|0|||||", CLASS_OID,  \
				  COLUMN_NAME, ATTTYPID, ATTLEN, ATTNUM, ATTTYPMOD, ATTBYVAL, ATTSTORAGE, ATTALIGN, ATTNOTNULL);   \
		assert(sizeof(BUFFER) > COPIED);                                                                                   \
		UNUSED(COPIED);                                                                                                    \
		/* Get a unique oid COLUMNOID for each column in the view. */                                                      \
		/* 	i.e. $INCREMENT(^%ydboctoocto(OCTOLIT_OID)) */                                                                 \
		STATUS = ydb_incr_s(&OID_BUFFER[0], 1, &OID_BUFFER[1], NULL, &PG_ATTRIBUTE[4]);                                    \
		YDB_ERROR_CHECK(STATUS);                                                                                           \
		if (YDB_OK != STATUS) {                                                                                            \
			break;                                                                                                     \
		}                                                                                                                  \
		/* Set the column name as having an oid of COLUMNOID in the pg_catalog.*/                                          \
		/* 	i.e. SET ^%ydboctoocto(OCTOLIT_TABLES,OCTOLIT_PG_CATALOG,OCTOLIT_PG_ATTRIBUTE,COLUMNOID)=...*/                 \
		STATUS = ydb_set_s(&PG_ATTRIBUTE[0], 4, &PG_ATTRIBUTE[1], &BUFFER_B);                                              \
		YDB_ERROR_CHECK(STATUS);                                                                                           \
		if (YDB_OK != STATUS) {                                                                                            \
			break;                                                                                                     \
		}                                                                                                                  \
		/* Store a cross reference of the COLUMNOID in ^%ydboctoschema.*/                                                  \
		/*	i.e. SET^ %ydboctoschema(TABLENAME,OCTOLIT_PG_ATTRIBUTE,COLUMNNAME)=COLUMNOID*/                                 \
		YDB_STRING_TO_BUFFER(COLUMN_NAME, &PG_ATTRIBUTE_SCHEMA[2]);                                                        \
		STATUS = ydb_set_s(&schema_global, 3, &PG_ATTRIBUTE_SCHEMA[0], &PG_ATTRIBUTE[4]);                                  \
		YDB_ERROR_CHECK(STATUS);                                                                                           \
		if (YDB_OK != STATUS) {                                                                                            \
			break;                                                                                                     \
		}                                                                                                                  \
	}

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
	date_AttributeLength = 4,
	time_AttributeLength = 8,
	timetz_AttributeLength = 12,
	timestamp_AttributeLength = 8,
	timestamptz_AttributeLength = 8,
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
	date_AttributeTypeId = 1082,
	time_AttributeTypeId = 1083,
	timetz_AttributeTypeId = 1266,
	timestamp_AttributeTypeId = 1114,
	timestamptz_AttributeTypeId = 1184,
} PG_AttributeTypeId;

typedef enum PG_AttributeByVal {
	bool_AttributeByVal = TRUE,
	int4_AttributeByVal = TRUE,
	varchar_AttributeByVal = FALSE,
	numeric_AttributeByVal = FALSE,
	date_AttributeByVal = TRUE,
	time_AttributeByVal = TRUE,
	timetz_AttributeByVal = FALSE,
	timestamp_AttributeByVal = TRUE,
	timestamptz_AttributeByVal = TRUE,
} PG_AttributeByVal;

typedef enum PG_AttributeStorage {
	bool_AttributeStorage = 'p',
	int4_AttributeStorage = 'p',
	varchar_AttributeStorage = 'x',
	numeric_AttributeStorage = 'm',
	date_AttributeStorage = 'p',
	time_AttributeStorage = 'p',
	timetz_AttributeStorage = 'p',
	timestamp_AttributeStorage = 'p',
	timestamptz_AttributeStorage = 'p',
} PG_AttributeStorage;

typedef enum PG_AttributeAlign {
	bool_AttributeAlign = 'c',
	int4_AttributeAlign = 'i',
	varchar_AttributeAlign = 'i',
	numeric_AttributeAlign = 'i',
	date_AttributeAlign = 'i',
	time_AttributeAlign = 'd',
	timetz_AttributeAlign = 'd',
	timestamp_AttributeAlign = 'd',
	timestamptz_AttributeAlign = 'd',
} PG_AttributeAlign;

/* Attempts to store a row in pg_catalog.pg_class for this table.
 * Note that this function is similar to store_function_in_pg_proc.
 */
int store_table_or_view_in_pg_class(SqlStatement *table_or_view_stmt, ydb_buffer_t *name_buffer) {
	boolean_t is_view = FALSE;
	SqlView	 *view;
	SqlTable *table;
	if (create_view_STATEMENT == table_or_view_stmt->type) {
		is_view = TRUE;
		UNPACK_SQL_STATEMENT(view, table_or_view_stmt, create_view);
		table = NULL;
	} else {
		UNPACK_SQL_STATEMENT(table, table_or_view_stmt, create_table);
		view = NULL;
	}
	int	     attnum;
	SqlColumn   *start_column;
	SqlColumn   *cur_column;
	ydb_buffer_t buffer_b;

	// Prepare buffers
	ydb_buffer_t *pg_class;
	ydb_buffer_t *oid_buffer;
	pg_class = make_buffers(config->global_names.octo, 4, OCTOLIT_TABLES, OCTOLIT_PG_CATALOG, OCTOLIT_PG_CLASS, "");
	oid_buffer = make_buffers(config->global_names.octo, 1, OCTOLIT_OID);
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&pg_class[4], INT64_TO_STRING_MAX);
	/* Get a unique oid TABLEOID/VIEWOID for the passed in table or view.
	 * 	i.e. $INCREMENT(^%ydboctoocto(OCTOLIT_OID))
	 */
	int status = ydb_incr_s(&oid_buffer[0], 1, &oid_buffer[1], NULL, &pg_class[4]);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, oid_buffer);
	pg_class[4].buf_addr[pg_class[4].len_used] = '\0';

	// Extract the table/view name
	SqlValue *value;
	if (is_view) {
		UNPACK_SQL_STATEMENT(value, view->viewName, value);
	} else {
		UNPACK_SQL_STATEMENT(value, table->tableName, value);
	}

	char *name;
	name = value->v.string_literal;
	/* These are hard-coded magic values related to the Postgres catalog.
	 * Columns of `pg_catalog.pg_class` table in `tests/fixtures/postgres.sql`.
	 * Any changes to that table definition will require changes here too.
	 * `relkind` value for view is 'v' and 'r' for a table.
	 */
	long unsigned int copied;
	char		  buffer[BUFFER_SIZE];
	char		  relkind_char;
	relkind_char = (is_view ? 'v' : 'r');
	copied = snprintf(buffer, sizeof(buffer),
			  "%s|2200|16388|0|16385|0|16386|0|0|0|0|16389|1|0|p|%c|3|0|0|1|0|0|0|0|0|1|d|0|571|1||||%s", name,
			  relkind_char, pg_class[4].buf_addr);
	assert(sizeof(buffer) > copied);
	UNUSED(copied);
	buffer_b.len_alloc = buffer_b.len_used = copied;
	buffer_b.buf_addr = buffer;
	/* Set the table/view name passed in as having an oid of TABLEOID/VIEWOID in the pg_catalog.
	 * 	i.e. SET ^%ydboctoocto(OCTOLIT_TABLES,OCTOLIT_PG_CATALOG,OCTOLIT_PG_CLASS,OID)=...
	 */
	status = ydb_set_s(&pg_class[0], 4, &pg_class[1], &buffer_b);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, oid_buffer);
	/* Store a cross reference of the TABLEOID/VIEWOID in ^%ydboctoschema.
	 *	i.e. SET ^%ydboctoschema("NAMES",OCTOLIT_PG_CLASS)=TABLEOID/VIEWOID
	 * That way a later DROP TABLE/VIEW or CREATE TABLE/VIEW can clean all ^%ydboctoocto and ^%ydboctoschema
	 * nodes created during the previous CREATE TABLE/VIEW.
	 */
	ydb_buffer_t schema_global;
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	ydb_buffer_t pg_class_schema[2];
	pg_class_schema[0] = *name_buffer;
	pg_class_schema[1] = pg_class[3];
	status = ydb_set_s(&schema_global, 2, pg_class_schema, &pg_class[4]);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, oid_buffer);

	long long class_oid;
	class_oid = strtoll(pg_class[4].buf_addr, NULL, 10); /* copy over class OID before we start changing it for column OID */
	if ((LLONG_MIN == class_oid) || (LLONG_MAX == class_oid)) {
		ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), pg_class[4].buf_addr);
		CLEANUP_AND_RETURN(pg_class, oid_buffer);
	}
	if (!is_view) {
		table->oid = class_oid; /* Initialize oid in SqlTable. Caller later invokes "compress_statement()" that stores this
					 * as part of the binary table definition in the database.
					 */
	}
	/* Assign ^%ydboctoschema(table_name/view_name)="table"/"view".
	 * This is useful to determine if a relation is a view or a table.
	 * Used by run_query() (to issue ERR_WRONG_TYPE) and find_view_or_table() (to determine memory chunk allocation).
	 */
	ydb_buffer_t lit_value;
	if (is_view) {
		YDB_LITERAL_TO_BUFFER(OCTOLIT_VIEW, &lit_value);
	} else {
		YDB_LITERAL_TO_BUFFER(OCTOLIT_TABLE, &lit_value);
	}
	status = ydb_set_s(&schema_global, 1, pg_class_schema, &lit_value);
	CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, oid_buffer);

	// We should also store the column definitions in the pg_attribute table
	ydb_buffer_t *pg_attribute;
	pg_attribute = make_buffers(config->global_names.octo, 4, OCTOLIT_TABLES, OCTOLIT_PG_CATALOG, OCTOLIT_PG_ATTRIBUTE, "");
	pg_attribute[4] = pg_class[4]; /* Inherit ydb_buffer used for OID */

	ydb_buffer_t pg_attribute_schema[3];
	pg_attribute_schema[0] = *name_buffer;
	pg_attribute_schema[1] = pg_attribute[3];
	if (!is_view) {
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
				case DATE_TYPE:
					atttypid = date_AttributeTypeId;
					attlen = date_AttributeLength;
					attbyval = date_AttributeByVal;
					attstorage = date_AttributeStorage;
					attalign = date_AttributeAlign;
					break;
				case TIME_TYPE:
					atttypid = time_AttributeTypeId;
					attlen = time_AttributeLength;
					attbyval = time_AttributeByVal;
					attstorage = time_AttributeStorage;
					attalign = time_AttributeAlign;
					break;
				case TIME_WITH_TIME_ZONE_TYPE:
					atttypid = timetz_AttributeTypeId;
					attlen = timetz_AttributeLength;
					attbyval = timetz_AttributeByVal;
					attstorage = timetz_AttributeStorage;
					attalign = timetz_AttributeAlign;
					break;
				case TIMESTAMP_TYPE:
					atttypid = timestamp_AttributeTypeId;
					attlen = timestamp_AttributeLength;
					attbyval = timestamp_AttributeByVal;
					attstorage = timestamp_AttributeStorage;
					attalign = timestamp_AttributeAlign;
					break;
				case TIMESTAMP_WITH_TIME_ZONE_TYPE:
					atttypid = timestamptz_AttributeTypeId;
					attlen = timestamptz_AttributeLength;
					attbyval = timestamptz_AttributeByVal;
					attstorage = timestamptz_AttributeStorage;
					attalign = timestamptz_AttributeAlign;
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
				 * attempts to utilize these relations in queries, e.g. through JOIN conditions, it could cause
				 * issues if these various related tables are out of sync. That said, there are no known cases of
				 * such issues as of this writing.
				 */
				column_name = value->v.string_literal;
				SET_PG_ATTRIBUTE_INFO(buffer, class_oid, column_name, atttypid, attlen, attnum, atttypmod, attbyval,
						      attstorage, attalign, attnotnull, oid_buffer, pg_attribute, status, buffer_b,
						      pg_attribute_schema, copied);
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
	} else {
		// View
		// We need to traverse the column_list of the underlying view definition (which is a table_alias_STATEMENT)
		SqlTableAlias *table_alias;
		if (table_alias_STATEMENT == view->src_table_alias_stmt->type) {
			UNPACK_SQL_STATEMENT(table_alias, view->src_table_alias_stmt, table_alias);
		} else {
			assert(set_operation_STATEMENT == view->src_table_alias_stmt->type);
			SqlStatement *stmt = drill_to_table_alias(view->src_table_alias_stmt);
			UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		}

		SqlColumnListAlias *start_cla, *cur_cla;
		UNPACK_SQL_STATEMENT(start_cla, table_alias->column_list, column_list_alias);
		cur_cla = start_cla;
		/* The column number of the given column, indexed from 1.
		 * Implemented per the following description of `attnum`:
		 *	"The number of the column. Ordinary columns are numbered from 1 up."
		 *
		 * This description was quoted from:
		 *	https://www.postgresql.org/docs/9.6/catalog-pg-attribute.html
		 */
		int attnum = 1;
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
			int  attnotnull;
			char attalign;
			char attstorage;
			int  attbyval;
			int  atttypmod;
			int  atttypid;
			int  attlen;

			// attnotnull = IS_COLUMN_NOT_NULL(cur_column);
			attnotnull = 0; // False in case of a view's column
			atttypmod = -1; // The default value for the atttypmod column
			switch (cur_cla->type) {
			/* Below atttypid values were obtained from Postgres using the below query.
			 *	`select typname,oid from pg_type where typname in ('numeric','int4','varchar','bool');`
			 */
			case BOOLEAN_VALUE:
				atttypid = bool_AttributeTypeId;
				attlen = bool_AttributeLength;
				attbyval = bool_AttributeByVal;
				attstorage = bool_AttributeStorage;
				attalign = bool_AttributeAlign;
				// atttypid = 16;
				break;
			case INTEGER_LITERAL:
				// atttypid = 23;
				atttypid = int4_AttributeTypeId;
				attlen = int4_AttributeLength;
				attbyval = int4_AttributeByVal;
				attstorage = int4_AttributeStorage;
				attalign = int4_AttributeAlign;
				break;
			case BOOLEAN_OR_STRING_LITERAL:
			/* It is possible for BOOLEAN_OR_STRING_LITERAL to be unresolved to a specific type at this point.
			 * Example queries which might lead us to this case are
			 * 	create view view_name as `select 't';
			 *	create view view_name as select true,'t' union select 'f','t';
			 * In the second query the second column in both parts of the set operation will be of
			 * `BOOLEAN_OR_STRING_LITERAL` type. This will be resolved in hash_canonical_query() to STRING_LITERAL.
			 * So consider this to be of STRING_TYPE here.
			 */
			case STRING_LITERAL:
				atttypid = varchar_AttributeTypeId;
				attlen = varchar_AttributeLength;
				// if (SIZE_OR_PRECISION_UNSPECIFIED != cur_column->data_type_struct.size_or_precision) {
				/* The + 4 below is done to match Postgres output (I guess it needs this space
				 * to store a 4-byte integer internally).
				 */
				//	atttypmod = cur_column->data_type_struct.size_or_precision + 4;
				//}
				attbyval = varchar_AttributeByVal;
				attstorage = varchar_AttributeStorage;
				attalign = varchar_AttributeAlign;
				// atttypid = 1043;
				break;
			case NUMERIC_LITERAL:
				atttypid = numeric_AttributeTypeId;
				attlen = numeric_AttributeLength;
				// if (SIZE_OR_PRECISION_UNSPECIFIED != cur_column->data_type_struct.size_or_precision) {
				//	if (SCALE_UNSPECIFIED == cur_column->data_type_struct.scale) {
				//		atttypmod = 0;
				//	} else {
				//		atttypmod = cur_column->data_type_struct.scale;
				//	}
				/* The + 4 below is done to match Postgres output (I guess it needs this space
				 * to store a 4-byte integer internally).
				 */
				//	atttypmod += 4;
				/* The << 16 below is also done to match Postgres output */
				//	atttypmod += (cur_column->data_type_struct.size_or_precision << 16);
				//}
				attbyval = numeric_AttributeByVal;
				attstorage = numeric_AttributeStorage;
				attalign = numeric_AttributeAlign;
				// atttypid = 1700;
				break;
			case NUL_VALUE:
				/* Queries such as the following can reach this code
				 * 	create view v as SELECT '';
				 * 	create view v as SELECT NULL;
				 * Postgres has `25` as the id value for such columns. Here
				 * `25` corresponds to `text` type in Postgres (`select typname,oid from pg_type where oid=25;`).
				 * Since there is no NUL_VALUE type equivalent in pg_type we set this value to be equivalent to
				 * what is seen in Postgres. This will not effect any processing in Octo but if any client which
				 * makes use of this value has an issue we need to revisit this.
				 */
				atttypid = 25;
				attlen = -1;
				attbyval = FALSE;
				attstorage = 'x';
				attalign = 'i';
				// atttypmod = -1;
				break;
			case DATE_LITERAL:
				atttypid = date_AttributeTypeId;
				attlen = date_AttributeLength;
				attbyval = date_AttributeByVal;
				attstorage = date_AttributeStorage;
				attalign = date_AttributeAlign;
				break;
			case TIME_LITERAL:
				atttypid = time_AttributeTypeId;
				attlen = time_AttributeLength;
				attbyval = time_AttributeByVal;
				attstorage = time_AttributeStorage;
				attalign = time_AttributeAlign;
				break;
			case TIME_WITH_TIME_ZONE_LITERAL:
				atttypid = timetz_AttributeTypeId;
				attlen = timetz_AttributeLength;
				attbyval = timetz_AttributeByVal;
				attstorage = timetz_AttributeStorage;
				attalign = timetz_AttributeAlign;
				break;
			case TIMESTAMP_LITERAL:
				atttypid = timestamp_AttributeTypeId;
				attlen = timestamp_AttributeLength;
				attbyval = timestamp_AttributeByVal;
				attstorage = timestamp_AttributeStorage;
				attalign = timestamp_AttributeAlign;
				break;
			case TIMESTAMP_WITH_TIME_ZONE_LITERAL:
				atttypid = timestamptz_AttributeTypeId;
				attlen = timestamptz_AttributeLength;
				attbyval = timestamptz_AttributeByVal;
				attstorage = timestamptz_AttributeStorage;
				attalign = timestamptz_AttributeAlign;
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
			SqlValue *value;
			UNPACK_SQL_STATEMENT(value, cur_cla->alias, value);

			char *column_name;
			column_name = value->v.string_literal;
			/* Store view oid, column name, type,
			 * These are hard-coded magic values related to the Postgres catalog
			 * Columns of `pg_catalog.pg_attribute` table in `tests/fixtures/postgres.sql`.
			 * Any changes to that table definition will require changes here too.
			 */
			SET_PG_ATTRIBUTE_INFO(buffer, class_oid, column_name, atttypid, attlen, attnum, atttypmod, attbyval,
					      attstorage, attalign, attnotnull, oid_buffer, pg_attribute, status, buffer_b,
					      pg_attribute_schema, copied);
			cur_cla = cur_cla->next;
			attnum++;
		} while (cur_cla != start_cla);

		YDB_FREE_BUFFER(&pg_class[4]);
		free(oid_buffer);
		free(pg_class);
		free(pg_attribute);
		if (YDB_OK != status) {
			return 1;
		}
	}

	if (is_view) {
		store_view_dependencies(name, name_buffer);
	} else {
		store_table_dependencies(table, name, name_buffer);
	}
	return 0;
}
