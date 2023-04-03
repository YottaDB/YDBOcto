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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "helpers.h"

/* Called by store_table_or_view_in_pg_class() to store view dependency information gathered by parser in globals.
 * The information stored by the parser is in %ydboctoViewDependency lvn.
 */
int store_view_dependencies(char *view_name, ydb_buffer_t *view_name_buffer) {
	// Populate dependency nodes
	ydb_buffer_t ydboctoViewDependency; // lvn buffer
	YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOVIEWDEPENDENCY, &ydboctoViewDependency);
	/* Store list of functions (hash and name) in gvns so a later DROP FUNCTION can issue an error if an existing view relies on
	 * the function about to be dropped.
	 * Lvn - %ydboctoViewDependency("V2","FUNCTION","ydboctoFN0uUSDY6E7G9VcjaOGNP9G")="SAMEVALUE"
	 * Gvn 1 - ^%ydboctoocto("functionviewdependency","SAMEVALUE","ydboctoFN0uUSDY6E7G9VcjaOGNP9G","V2")
	 * Gvn 2 - ^%ydboctoocto("viewdependency","V2","functions","SAMEVALUE","ydboctoFN0uUSDY6E7G9VcjaOGNP9G")
	 */
	// Subscript setup
	ydb_buffer_t subs[3];

	// Subs[0] - V2
	subs[0] = *view_name_buffer;

	// Subs[1] - functions
	YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS, &subs[1]);

	// Subs[2] - ydboctoFN0uUSDY6E7G9VcjaOGNP9G
	char function_hash_buff[MAX_ROUTINE_LEN + 1];
	subs[2].buf_addr = function_hash_buff;
	subs[2].len_alloc = sizeof(function_hash_buff) - 1; /*reserve 1 byte for null terminator */
	subs[2].len_used = 0;

	// result - SAMEVALUE
	ydb_buffer_t function_name;
	char	     function_name_buff[OCTO_MAX_IDENT + 1];
	function_name.buf_addr = function_name_buff;
	function_name.len_alloc = sizeof(function_name_buff) - 1; /* reserve 1 byte for null terminator */
	int status;

	while (TRUE) {
		status = ydb_subscript_next_s(&ydboctoViewDependency, 3, &subs[0], &subs[2]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		// "subs[2] now constaints the function-hash
		status = ydb_get_s(&ydboctoViewDependency, 3, &subs[0], &function_name);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		// function_name now contains the function-name

		/* Store the gvn node now
		 * Gvn 1 - ^%ydboctoocto("functionviewdependency","SAMEVALUE","ydboctoFN0uUSDY6E7G9VcjaOGNP9G","V2")
		 */
		ydb_buffer_t gvn_subs[6];
		YDB_STRING_TO_BUFFER(config->global_names.octo, &gvn_subs[0]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONVIEWDEPENDENCY, &gvn_subs[1]);
		gvn_subs[2] = function_name;
		gvn_subs[3] = subs[2]; // hash
		YDB_STRING_TO_BUFFER(view_name, &gvn_subs[4]);
		status = ydb_set_s(&gvn_subs[0], 4, &gvn_subs[1], NULL);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		/* Store the gvn node now
		 * Gvn 2 - ^%ydboctoocto("viewdependency","V2","functions","SAMEVALUE","ydboctoFN0uUSDY6E7G9VcjaOGNP9G")
		 */
		YDB_LITERAL_TO_BUFFER(OCTOLIT_VIEWDEPENDENCY, &gvn_subs[1]);
		YDB_STRING_TO_BUFFER(view_name, &gvn_subs[2]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS, &gvn_subs[3]);
		gvn_subs[4] = function_name;
		gvn_subs[5] = subs[2]; // hash
		status = ydb_set_s(&gvn_subs[0], 5, &gvn_subs[1], NULL);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
	}
	/* Store list of tables (name) in gvns so a later DROP TABLE can issue an error if an existing view relies on
	 * the table about to be dropped.
	 * Lvn - %viewdependency("V2","tables","NAMES")=""
	 * Gvn 1 - ^%ydboctoocto("tableviewdependency","NAMES","V2")
	 * Gvn 2 - ^%ydboctoocto("viewdependency","V2","tables","names")
	 */
	// ydboctoViewDependency is already setup
	// Subscript setup
	// Subs[0] - V2
	// Subs[1] - tables
	YDB_LITERAL_TO_BUFFER(OCTOLIT_TABLES, &subs[1]);

	// Subs[2] - NAMES
	char table_name_buff[OCTO_MAX_IDENT + 1];
	subs[2].buf_addr = table_name_buff;
	subs[2].len_alloc = sizeof(table_name_buff);
	subs[2].len_used = 0;

	while (TRUE) {
		status = ydb_subscript_next_s(&ydboctoViewDependency, 3, &subs[0], &subs[2]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		// subs[2] now contains the table name

		/* Store the gvn node now
		 * Gvn 1 - ^%ydboctoocto("tableviewdependency","NAMES","V2")
		 */
		ydb_buffer_t gvn_subs[5];
		YDB_STRING_TO_BUFFER(config->global_names.octo, &gvn_subs[0]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_TABLEVIEWDEPENDENCY, &gvn_subs[1]);
		gvn_subs[2] = subs[2]; // table_name
		YDB_STRING_TO_BUFFER(view_name, &gvn_subs[3]);
		status = ydb_set_s(&gvn_subs[0], 3, &gvn_subs[1], NULL);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		/* Store the gvn node now
		 * Gvn 2 - ^%ydboctoocto("viewdependency","V2","tables","names");
		 */
		YDB_LITERAL_TO_BUFFER(OCTOLIT_VIEWDEPENDENCY, &gvn_subs[1]);
		YDB_STRING_TO_BUFFER(view_name, &gvn_subs[2]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_TABLES, &gvn_subs[3]);
		gvn_subs[4] = subs[2]; // table_name
		status = ydb_set_s(&gvn_subs[0], 4, &gvn_subs[1], NULL);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
	}

	/* Store list of views (oid and name) in gvns so a later DROP VIEW can issue an error if an existing view relies on
	 * the view about to be dropped.
	 * Lvn - %viewdependency("V2","views","V1")=""
	 * Gvn 1 - ^%ydboctoocto("viewdependency","V1","fromview","V2")
	 * Gvn 2 - ^%ydboctoocto("viewdependency","V2","views","V1")
	 */
	// ydboctoViewDependency is already setup
	// Subscript setup
	// Subs[0] - V2
	// Subs[1] - views
	YDB_LITERAL_TO_BUFFER(OCTOLIT_VIEWS, &subs[1]);

	// Subs[2] - V1
	char dependent_on_view_name_buff[OCTO_MAX_IDENT + 1];
	subs[2].buf_addr = dependent_on_view_name_buff;
	subs[2].len_alloc = sizeof(dependent_on_view_name_buff);
	subs[2].len_used = 0;

	while (TRUE) {
		status = ydb_subscript_next_s(&ydboctoViewDependency, 3, &subs[0], &subs[2]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		// subs[2] now contains the dependent view name
		/* Store the gvn node now
		 * Gvn 1 - ^%ydboctoocto("viewdependency","V1","fromview","V2")
		 */
		ydb_buffer_t gvn_subs[5];
		YDB_STRING_TO_BUFFER(config->global_names.octo, &gvn_subs[0]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_VIEWDEPENDENCY, &gvn_subs[1]);
		gvn_subs[2] = subs[2]; // dependent_on_view_name
		YDB_LITERAL_TO_BUFFER(OCTOLIT_FROMVIEW, &gvn_subs[3]);
		YDB_STRING_TO_BUFFER(view_name, &gvn_subs[4]);
		status = ydb_set_s(&gvn_subs[0], 4, &gvn_subs[1], NULL);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		/* Store the gvn node now
		 * Gvn 2 - ^%ydboctoocto("viewdependency","V2","views","V1");
		 */
		YDB_LITERAL_TO_BUFFER(OCTOLIT_VIEWDEPENDENCY, &gvn_subs[1]);
		YDB_STRING_TO_BUFFER(view_name, &gvn_subs[2]); // view being created
		YDB_LITERAL_TO_BUFFER(OCTOLIT_VIEWS, &gvn_subs[3]);
		gvn_subs[4] = subs[2]; // dependent_on_view_name
		status = ydb_set_s(&gvn_subs[0], 4, &gvn_subs[1], NULL);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
	}
	// lvn to gvn copied, delete lvn data
	status = ydb_delete_s(&ydboctoViewDependency, 0, NULL, YDB_DEL_TREE);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		assert(FALSE);
		return 1;
	}
	return 0;
}
