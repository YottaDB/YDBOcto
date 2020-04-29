/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
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

/* Deletes all references to a function name from the catalog.
 * Undoes what was done by "store_function_in_pg_proc.c".
 */
int delete_function_from_pg_proc(ydb_buffer_t *function_name_buffer) {
	int		status;
	ydb_buffer_t	pg_proc[5];
	ydb_buffer_t	octo_functions[4];
	char		oid_str[INT64_TO_STRING_MAX];

	YDB_STRING_TO_BUFFER(config->global_names.octo, &pg_proc[0]);
	YDB_STRING_TO_BUFFER("tables", &pg_proc[1]);
	YDB_STRING_TO_BUFFER("pg_catalog", &pg_proc[2]);
	YDB_STRING_TO_BUFFER("pg_proc", &pg_proc[3]);
	pg_proc[4].buf_addr = oid_str;
	pg_proc[4].len_alloc = sizeof(oid_str);

	// Check OID for FUNCTIONNAME (usually stored as ^%ydboctoocto("functions",FUNCTIONNAME,"oid")=FUNCTIONOID)
	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_functions[0]);
	YDB_STRING_TO_BUFFER("functions", &octo_functions[1]);
	octo_functions[2] = *function_name_buffer;
	YDB_STRING_TO_BUFFER("oid", &octo_functions[3]);
	status = ydb_get_s(&octo_functions[0], 3, &octo_functions[1], &pg_proc[4]);
	if (YDB_ERR_GVUNDEF != status) {
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			return 1;
		}
		// Delete function OID node : i.e. KILL ^%ydboctoocto("tables","pg_catalog","pg_proc",FUNCTIONOID)
		status = ydb_delete_s(&pg_proc[0], 4, &pg_proc[1], YDB_DEL_NODE);
		if (YDB_OK != status) {
			return 1;
		}
	} else {
		// OID for this function doesn't exist. Move on to next step.
	}
	return 0;
}
