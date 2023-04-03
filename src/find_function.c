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
#include <string.h>
#include <errno.h>

#include "octo.h"
#include "octo_types.h"
#include "helpers.h"

// Note that this function is very similar to find_view_or_table.c, so changes there may need to be reflected here also.
SqlFunction *find_function(const char *function_name, const char *function_hash) {
	SqlFunction * function;
	SqlStatement *stmt;
	ydb_buffer_t  save_value;
	ydb_buffer_t  value_buffer;
	char	      value_str[MAX_DEFINITION_FRAGMENT_SIZE];
	char *	      buff, *cur_buff;
	int	      status;
	int32_t	      length;
	long	      length_long;
	MemoryChunk * old_chunk;
	ydb_buffer_t  loaded_schemas, octo_global, function_subs[5], ret;
	char	      retbuff[sizeof(void *)];
	char	      oid_buff[INT32_TO_STRING_MAX], len_str[INT32_TO_STRING_MAX];
	boolean_t     drop_cache;
	uint64_t      db_oid;

	TRACE(INFO_FUNCTION_SEARCH, function_name);
	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);

	/* We look to see if the function has already been loaded, and if so we get the pointer to process-local memory
	 * from the YDB local variable, then setup the stmt pointer. Else, we load the binary from the database
	 * and store a pointer to the parsed schema.
	 *
	 * Once a function is loaded from the database, it is stored in process local memory and used from there unless
	 * a later CREATE FUNCTION or DROP FUNCTION statement has concurrently executed. In this case, we load the function
	 * again from the database and release the process local memory that was housing the previous function definition.
	 */
	YDB_STRING_TO_BUFFER(config->global_names.loadedschemas, &loaded_schemas);
	YDB_STRING_TO_BUFFER(OCTOLIT_FUNCTIONS, &function_subs[0]);
	YDB_STRING_TO_BUFFER((char *)function_name, &function_subs[1]);
	YDB_STRING_TO_BUFFER((char *)function_hash, &function_subs[2]);
	ret.buf_addr = &retbuff[0];
	ret.len_alloc = sizeof(retbuff);
	status = ydb_get_s(&loaded_schemas, 3, &function_subs[0], &ret);
	switch (status) {
	case YDB_OK:
		/* We have the function definition already stored in process local memory. Use that as long as the
		 * function definition has not changed in the database.
		 */
		stmt = *((SqlStatement **)ret.buf_addr);
		UNPACK_SQL_STATEMENT(function, stmt, create_function);
		/* Check if function has not changed in the database since we cached it
		 *	^%ydboctoocto(OCTOLIT_FUNCTIONS,FUNCTIONNAME,FUNCTIONHASH)
		 */
		YDB_STRING_TO_BUFFER(OCTOLIT_OID, &function_subs[3]);
		ret.buf_addr = &oid_buff[0];
		ret.len_alloc = sizeof(oid_buff);
		status = ydb_get_s(&octo_global, 4, &function_subs[0], &ret);
		switch (status) {
		case YDB_OK:
			assert(ret.len_alloc > ret.len_used); // ensure space for null terminator
			ret.buf_addr[ret.len_used] = '\0';
			db_oid = (uint64_t)strtoll(ret.buf_addr, NULL, 10);
			if ((LLONG_MIN == (long long)db_oid) || (LLONG_MAX == (long long)db_oid)) {
				ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), ret.buf_addr);
				return NULL;
			}
			drop_cache = (db_oid != function->oid);
			break;
		case YDB_ERR_GVUNDEF:
			// A DROP FUNCTION ran concurrently since we cached it. Reload cache from database.
			drop_cache = TRUE;
			break;
		default:
			YDB_ERROR_CHECK(status);
			return NULL;
			break;
		}
		if (!drop_cache) {
			return function;
		} else {
			/* We do not expect the loaded function cache to be dropped in the normal case.
			 * This includes most of the bats tests in the YDBOcto repo. Therefore, we assert that this
			 * code path is never reached unless an env var is defined that says this is expected.
			 * This way it serves as a good test case of the fact that once a function is loaded in the process
			 * local cache, it is used almost always except for rare cases when we go reload from the database.
			 */
			assert(NULL != getenv("octo_dbg_drop_cache_expected"));
			status = drop_schema_from_local_cache(&function_subs[1], FunctionSchema, &function_subs[2]);
			if (YDB_OK != status) {
				// YDB_ERROR_CHECK would already have been done inside "drop_schema_from_local_cache()"
				return NULL;
			}
		}
		break;
	case YDB_ERR_LVUNDEF:
		// Function definition does not exist locally. Fall through to read it from database.
		break;
	default:
		YDB_ERROR_CHECK(status);
		return NULL;
		break;
	}

	// Get the length in bytes of the binary function definition
	YDB_STRING_TO_BUFFER(OCTOLIT_LENGTH, &function_subs[3]);
	function_subs[4].buf_addr = len_str;
	function_subs[4].len_used = 0; /* needed to avoid false [clang-analyzer-core.uninitialized.ArraySubscript] warning when we
					  use function_subs[4].len_used later below */
	function_subs[4].len_alloc = sizeof(len_str);
	status = ydb_get_s(&octo_global, 4, &function_subs[0], &function_subs[4]);
	if (YDB_ERR_GVUNDEF == status) {
		// Definition for function (previous CREATE FUNCTION statement) doesn't exist
		return NULL;
	}
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return NULL;
	}
	function_subs[4].buf_addr[function_subs[4].len_used] = '\0';
	length_long = strtol(function_subs[4].buf_addr, NULL, 10);
	if ((ERANGE != errno) && (0 <= length_long) && (INT32_MAX >= length_long)) {
		length = (int32_t)length_long;
	} else {
		ERROR(ERR_LIBCALL, "strtol");
		return NULL;
	}
	// Switch subscript from OCTOLIT_LENGTH back to OCTOLIT_BINARY to get the binary function definition
	YDB_STRING_TO_BUFFER(OCTOLIT_BINARY, &function_subs[3]);

	/* Allocate a buffer to hold the full function.
	 * Note that we create a new memory chunk here (different from the memory chunk used to store octo structures
	 * for queries) so that this function structure does not get cleaned when one query finishes and the next query starts.
	 */
	old_chunk = memory_chunks;
	memory_chunks = alloc_chunk(length);
	buff = octo_cmalloc(memory_chunks, length);

	value_buffer.buf_addr = value_str;
	value_buffer.len_alloc = sizeof(value_str);
	function_subs[4].len_used = 0;
	cur_buff = buff;
	while (TRUE) {
		/* See "src/run_query.c" under "case create_function_STATEMENT:" for how these global variable nodes are created */
		status = ydb_subscript_next_s(&octo_global, 5, &function_subs[0], &function_subs[4]);
		if (YDB_ERR_NODEEND == status) {
			status = YDB_OK;
			break;
		}
		assert(length > (cur_buff - buff));
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		status = ydb_get_s(&octo_global, 5, &function_subs[0], &value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		memcpy(cur_buff, value_buffer.buf_addr, value_buffer.len_used);
		cur_buff += value_buffer.len_used;
		assert(MAX_DEFINITION_FRAGMENT_SIZE >= value_buffer.len_used);
		if (length == (cur_buff - buff)) {
			break;
		}
	}
	if (YDB_OK != status) {
		OCTO_CFREE(memory_chunks);
		memory_chunks = old_chunk;
		return NULL;
	}
	assert(length == (cur_buff - buff));

	stmt = (void *)buff;
	decompress_statement((char *)stmt, length);
	if (NULL == stmt) {
		OCTO_CFREE(memory_chunks);
		memory_chunks = old_chunk;
		return NULL;
	}

	// Note the pointer to the loaded parse tree root
	save_value.buf_addr = (char *)&stmt;
	save_value.len_used = save_value.len_alloc = sizeof(void *);
	status = ydb_set_s(&loaded_schemas, 3, &function_subs[0], &save_value);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		OCTO_CFREE(memory_chunks);
		memory_chunks = old_chunk;
		return NULL;
	}
	// Note down the memory chunk so we can free it later
	YDB_STRING_TO_BUFFER(OCTOLIT_CHUNK, &function_subs[2]);
	save_value.buf_addr = (char *)&memory_chunks;
	save_value.len_used = save_value.len_alloc = sizeof(void *);
	status = ydb_set_s(&loaded_schemas, 4, &function_subs[0], &save_value);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		OCTO_CFREE(memory_chunks);
		memory_chunks = old_chunk;
		return NULL;
	}
	UNPACK_SQL_STATEMENT(function, stmt, create_function);

	// Restore memory chunks
	memory_chunks = old_chunk;

	return function;
}
