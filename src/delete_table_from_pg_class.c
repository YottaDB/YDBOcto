/****************************************************************
 *								*
 * Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"
#include "helpers.h"

#define CLEANUP(PG_CLASS, PG_ATTRIBUTE, PG_ATTRIBUTE_SCHEMA, OID_BUFFER) \
	{                                                                \
		assert(NULL != PG_CLASS);                                \
		free(PG_CLASS);                                          \
		assert(NULL != PG_ATTRIBUTE);                            \
		free(PG_ATTRIBUTE);                                      \
		YDB_FREE_BUFFER(&PG_ATTRIBUTE_SCHEMA[2]);                \
		YDB_FREE_BUFFER(&OID_BUFFER);                            \
	}

#define CLEANUP_AND_RETURN(PG_CLASS, PG_ATTRIBUTE, PG_ATTRIBUTE_SCHEMA, OID_BUFFER) \
	{                                                                           \
		CLEANUP(PG_CLASS, PG_ATTRIBUTE, PG_ATTRIBUTE_SCHEMA, OID_BUFFER);   \
		return 1;                                                           \
	}

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, PG_CLASS, PG_ATTRIBUTE, PG_ATTRIBUTE_SCHEMA, OID_BUFFER) \
	{                                                                                                 \
		YDB_ERROR_CHECK(STATUS);                                                                  \
		if (YDB_OK != STATUS) {                                                                   \
			CLEANUP_AND_RETURN(PG_CLASS, PG_ATTRIBUTE, PG_ATTRIBUTE_SCHEMA, OID_BUFFER);      \
		}                                                                                         \
	}

/* Deletes all references to a tablename and its columns from the catalog.
 * Undoes what was done by "store_table_in_pg_class.c".
 * Returns
 *	0 for normal
 *	1 for error
 */
int delete_table_from_pg_class(ydb_buffer_t *table_name_buffer) {
	int	      status;
	ydb_buffer_t *pg_class;
	ydb_buffer_t *pg_attribute;
	ydb_buffer_t  pg_class_schema[2], pg_attribute_schema[3];
	ydb_buffer_t  schema_global;
	ydb_buffer_t  oid_buffer;

	pg_class = make_buffers(config->global_names.octo, 4, OCTOLIT_TABLES, OCTOLIT_PG_CATALOG, OCTOLIT_PG_CLASS, "");
	pg_attribute = make_buffers(config->global_names.octo, 4, OCTOLIT_TABLES, OCTOLIT_PG_CATALOG, OCTOLIT_PG_ATTRIBUTE, "");
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&oid_buffer, INT64_TO_STRING_MAX);
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&pg_attribute_schema[2], OCTO_MAX_IDENT);
	/* Check OID for tablename (usually stored as ^%ydboctoschema(TABLENAME,OCTOLIT_PG_CLASS)=TABLEOID) */
	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	pg_class_schema[0] = *table_name_buffer;
	pg_class_schema[1] = pg_class[3];
	status = ydb_get_s(&schema_global, 2, pg_class_schema, &oid_buffer);
	if (YDB_ERR_GVUNDEF != status) {
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, pg_attribute, pg_attribute_schema, oid_buffer);
		pg_class[4] = oid_buffer;
		/* Delete table OID node : i.e. KILL ^%ydboctoocto(OCTOLIT_TABLES,OCTOLIT_PG_CATALOG,OCTOLIT_PG_CLASS,TABLEOID) */
		status = ydb_delete_s(&pg_class[0], 4, &pg_class[1], YDB_DEL_NODE);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, pg_attribute, pg_attribute_schema, oid_buffer);
	} else {
		/* OID for table does not exist. Move on to next step. */
	}
	/* Check OID for each column in table (usually stored as
	 * ^%ydboctoschema(TABLENAME,OCTOLIT_PG_ATTRIBUTE,COLUMNNAME)=COLUMNOID) */
	pg_attribute_schema[0] = *table_name_buffer;
	pg_attribute_schema[1] = pg_attribute[3];
	assert(0 == pg_attribute_schema[2].len_used); /* should have been set by OCTO_MALLOC_NULL_TERMINATED_BUFFER above */
	do {
		/* Find COLUMNNAME such that ^%ydboctoschema(TABLENAME,OCTOLIT_PG_ATTRIBUTE,COLUMNNAME) exists */
		status = ydb_subscript_next_s(&schema_global, 3, pg_attribute_schema, &pg_attribute_schema[2]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, pg_attribute, pg_attribute_schema, oid_buffer);
		/* Get COLUMNOID corresponding to COLUMNNAME */
		status = ydb_get_s(&schema_global, 3, pg_attribute_schema, &oid_buffer);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, pg_attribute, pg_attribute_schema, oid_buffer);
		/* Delete column OID node : i.e. KILL
		 * ^%ydboctoocto(OCTOLIT_TABLES,OCTOLIT_PG_CATALOG,OCTOLIT_PG_ATTRIBUTE,COLUMNOID) */
		pg_attribute[4] = oid_buffer;
		status = ydb_delete_s(&pg_attribute[0], 4, &pg_attribute[1], YDB_DEL_NODE);
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(status, pg_class, pg_attribute, pg_attribute_schema, oid_buffer);
	} while (TRUE);
	/* Cleanup and return success */
	CLEANUP(pg_class, pg_attribute, pg_attribute_schema, oid_buffer);
	return 0;
}
