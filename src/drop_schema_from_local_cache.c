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

int drop_schema_from_local_cache(ydb_buffer_t *name_buffer, SqlSchemaType schema_type) {
	ydb_buffer_t schema_local, subs_array[3], ret;
	int	     status;
	MemoryChunk *save_chunk;
	void *	     value;
	char	     retbuff[sizeof(void *)];

	YDB_STRING_TO_BUFFER(config->global_names.loadedschemas, &schema_local);
	/* Free up memory chunk noted down at the end of "src/find_function.c" or "src/find_table.c"
	 * Those would be stored in one of the below 2 node pairs:
	 *	%ydboctoloadedschemas(OCTOLIT_FUNCTIONS,FUNCTIONNAME)
	 *	%ydboctoloadedschemas(OCTOLIT_FUNCTIONS,FUNCTIONNAME,OCTOLIT_CHUNK)
	 * OR
	 *	%ydboctoloadedschemas(TABLENAME)
	 *	%ydboctoloadedschemas(TABLENAME,OCTOLIT_CHUNK)
	 */
	if (TableSchema == schema_type) {
		YDB_STRING_TO_BUFFER(OCTOLIT_TABLES, &subs_array[0]);
	} else if (FunctionSchema == schema_type) {
		YDB_STRING_TO_BUFFER(OCTOLIT_FUNCTIONS, &subs_array[0]);
	} else {
		assert(FALSE);
	}
	subs_array[1] = *name_buffer;
	ret.buf_addr = &retbuff[0];
	ret.len_alloc = sizeof(retbuff);
	status = ydb_get_s(&schema_local, 2, &subs_array[0], &ret);
	/* Note it is possible to get YDB_ERR_LVUNDEF if the table was not loaded into the local cache previously.
	 * In that case, just move on. Hence the special check for YDB_ERR_LVUNDEF below.
	 */
	if (YDB_OK == status) {
		assert(sizeof(void *) == ret.len_used);
		value = *(void **)ret.buf_addr;
		free(value);
	} else if (YDB_ERR_LVUNDEF != status) {
		YDB_ERROR_CHECK(status);
		return status;
	}
	YDB_LITERAL_TO_BUFFER(OCTOLIT_CHUNK, &subs_array[2]);
	status = ydb_get_s(&schema_local, 3, &subs_array[0], &ret);
	if (YDB_OK == status) {
		assert(sizeof(void *) == ret.len_used);
		save_chunk = *(void **)ret.buf_addr;
		free(save_chunk);
	} else if (YDB_ERR_LVUNDEF != status) {
		YDB_ERROR_CHECK(status);
		return status;
	}
	/* Now that memory has been freed, delete those nodes */
	status = ydb_delete_s(&schema_local, 1, &subs_array[0], YDB_DEL_TREE);
	YDB_ERROR_CHECK(status);
	return status;
}
