/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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
#include <string.h>

#include "octo.h"
#include "octo_types.h"
#include "helpers.h"

void cleanup_tables() {
	int	     status;
	char	     buffer[MAX_STR_CONST];
	char	     table_name[MAX_STR_CONST];
	ydb_buffer_t loaded_schemas_b[4];
	ydb_buffer_t result_b;

	YDB_STRING_TO_BUFFER(config->global_names.loadedschemas, &loaded_schemas_b[0]);
	YDB_STRING_TO_BUFFER("tables", &loaded_schemas_b[1]);
	loaded_schemas_b[2].buf_addr = table_name;
	loaded_schemas_b[2].len_used = 0;
	loaded_schemas_b[2].len_alloc = sizeof(table_name);
	YDB_STRING_TO_BUFFER("chunk", &loaded_schemas_b[3]);

	result_b.buf_addr = buffer;
	result_b.len_alloc = result_b.len_used = sizeof(buffer);
	while (TRUE) {
		status = ydb_subscript_next_s(&loaded_schemas_b[0], 2, &loaded_schemas_b[1], &loaded_schemas_b[2]);
		if (YDB_ERR_NODEEND == status) {
			break;
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		status = ydb_get_s(&loaded_schemas_b[0], 3, &loaded_schemas_b[1], &result_b);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		OCTO_CFREE(*((MemoryChunk **)result_b.buf_addr));
	}
}
