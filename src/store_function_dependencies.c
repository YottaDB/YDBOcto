/****************************************************************
 *								*
 * Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	*
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

/* Store list of functions (hash and name) in gvns so a later DROP FUNCTION can issue an error if an existing
 * table constraint or extract function relies on the function that is about to be dropped. We already store this list in an lvn
 * so all we need to do is to move the lvn data into the gvn here.
 *
 * In the case of table constraints, functions are stored under `%ydboctoTblConstraint`. In the case of extract specifications,
 * functions are stored under `%ydboctoTblExtract`.
 *
 * Below is an example layout of the input lvn nodes for a table constraint (where 8-byte-constraint-pointer =
 *"&constraint->definition" and ,"%ydboctoFN0uUSDY6E7G9VcjaOGNP9G" is the function hash and "SAMEVALUE" is the function name)
 *	%ydboctoTblConstraint("functions")=8-byte-constraint-pointer
 *	%ydboctoTblConstraint("functions",8-byte-constraint-pointer,"%ydboctoFN0uUSDY6E7G9VcjaOGNP9G")="SAMEVALUE"
 *	%ydboctoTblConstraint("functions_map",8-byte-constraint-pointer)="NAME1"
 *
 * And below is the desired layout of the output gvn nodes (where "NAMES" is the table name)
 *	^%ydboctoocto("functions","SAMEVALUE","%ydboctoFN0uUSDY6E7G9VcjaOGNP9G","check_constraint","NAMES","NAME1")=""
 *	^%ydboctoocto("tableconstraint","NAMES","NAME1","SAMEVALUE","%ydboctoFN0uUSDY6E7G9VcjaOGNP9G")=""
 */

int store_function_dependencies(char *table_name, DDLDependencyType dtype) {
	ydb_buffer_t ydboctoTblDep;
	if (DDL_CheckConstraint == dtype) {
		YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOTBLCONSTRAINT, &ydboctoTblDep);
	} else {
		assert(DDL_ExtractFunction == dtype);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOTBLEXTRACT, &ydboctoTblDep);
	}

	ydb_buffer_t subs[3];
	char	     function_hash_buff[MAX_ROUTINE_LEN + 1];
	subs[2].buf_addr = function_hash_buff;
	subs[2].len_alloc = sizeof(function_hash_buff) - 1; /* reserve 1 byte for null terminator */

	ydb_buffer_t function_name;
	char	     function_name_buff[OCTO_MAX_IDENT + 1];
	function_name.buf_addr = function_name_buff;
	function_name.len_alloc = sizeof(function_name_buff) - 1; /* reserve 1 byte for null terminator */

	ydb_buffer_t dep_name;
	char	     dep_name_buff[OCTO_MAX_IDENT + 1];
	dep_name.buf_addr = dep_name_buff;
	dep_name.len_alloc = sizeof(dep_name_buff) - 1; /* reserve 1 byte for null terminator */

	int  status;
	char pointer_buff[sizeof(void *)];
	subs[1].buf_addr = pointer_buff;
	subs[1].len_alloc = sizeof(pointer_buff);
	subs[1].len_used = 0;
	while (TRUE) {
		YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS, &subs[0]);
		status = ydb_subscript_next_s(&ydboctoTblDep, 2, &subs[0], &subs[1]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		/* Note: "subs[1]" now contains the 8-byte-constraint-pointer or the 8-byte-extract-pointer */
		subs[2].len_used = 0;
		status = ydb_subscript_next_s(&ydboctoTblDep, 3, &subs[0], &subs[2]);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		/* Note: "subs[2]" now contains the function-hash */
		status = ydb_get_s(&ydboctoTblDep, 3, &subs[0], &function_name);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		/* Note: "function_name" now contains the function-name */
		YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS_MAP, &subs[0]);
		status = ydb_get_s(&ydboctoTblDep, 2, &subs[0], &dep_name);
		if (YDB_ERR_LVUNDEF == status) {
			/* The EXTRACT definition is a string literal, not a function call. So no function call
			 * information was stored in the database. This is an acceptable scenario, so update
			 * status to reflect this.
			 */
			status = YDB_OK;
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		/* Note: "dep_name" now contains the constraint or extract name */
		/* Now that we got all the needed information from the lvn node, store it in the gvns */

		/* Store the gvn node
		 * ^%ydboctoocto("functions","SAMEVALUE","%ydboctoFN0uUSDY6E7G9VcjaOGNP9G","check_constraint","NAMES","NAME1")=""
		 *   OR
		 * ^%ydboctoocto("functions","SAMEVALUE","%ydboctoFN0uUSDY6E7G9VcjaOGNP9G","extractfunction","NAMES","NAME1")=""
		 */
		ydb_buffer_t gvn_subs[7];
		YDB_STRING_TO_BUFFER(config->global_names.octo, &gvn_subs[0]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS, &gvn_subs[1]);
		gvn_subs[2] = function_name;
		gvn_subs[3] = subs[2];
		if (DDL_CheckConstraint == dtype) {
			YDB_LITERAL_TO_BUFFER(OCTOLIT_CHECK_CONSTRAINT, &gvn_subs[4]);
		} else {
			assert(DDL_ExtractFunction == dtype);
			YDB_LITERAL_TO_BUFFER(OCTOLIT_EXTRACTFUNCTION, &gvn_subs[4]);
		}
		YDB_STRING_TO_BUFFER(table_name, &gvn_subs[5]);
		gvn_subs[6] = dep_name;
		status = ydb_set_s(&gvn_subs[0], 6, &gvn_subs[1], NULL);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
		/* Store the gvn node, e.g.
		 * ^%ydboctoocto("tableconstraint","NAMES","NAME1","SAMEVALUE","%ydboctoFN0uUSDY6E7G9VcjaOGNP9G")=""
		 *   OR
		 * ^%ydboctoocto("extractfunction","NAMES","NAME1","SAMEVALUE","%ydboctoFN0uUSDY6E7G9VcjaOGNP9G")=""
		 */
		if (DDL_CheckConstraint == dtype) {
			YDB_LITERAL_TO_BUFFER(OCTOLIT_TABLECONSTRAINT, &gvn_subs[1]);
		} else {
			assert(DDL_ExtractFunction == dtype);
			YDB_LITERAL_TO_BUFFER(OCTOLIT_EXTRACTFUNCTION, &gvn_subs[1]);
		}
		YDB_STRING_TO_BUFFER(table_name, &gvn_subs[2]);
		gvn_subs[3] = dep_name;
		gvn_subs[4] = function_name;
		gvn_subs[5] = subs[2];
		status = ydb_set_s(&gvn_subs[0], 5, &gvn_subs[1], NULL);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE);
			return 1;
		}
	}
	/* Now that we have copied over the lvn nodes tracking function names/hashes in a check constraint or extract functions into
	 * a gvn, delete the lvn data. See comment in "src/parser/table_definition.c" (search for OCTOLIT_FUNCTIONS) for details.
	 */
	status = ydb_delete_s(&ydboctoTblDep, 0, NULL, YDB_DEL_TREE);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		assert(FALSE);
		return 1;
	}
	return 0;
}
