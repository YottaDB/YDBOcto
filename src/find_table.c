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

// Note that this function is very similar to find_function.c, so changes there may need to be reflected here also.
SqlTable *find_table(const char *table_name) {
	SqlTable *    table;
	SqlStatement *stmt;
	ydb_buffer_t  save_value;
	ydb_buffer_t *table_binary_b;
	ydb_buffer_t  value_b;
	char	      value_b_buffer[MAX_STR_CONST];
	char *	      buff, *cur_buff;
	const char *  c;
	int	      status;
	int	      length;
	MemoryChunk * old_chunk;
	ydb_buffer_t  varname, subs_array[3], ret;
	char	      retbuff[sizeof(void *)];
	char	      oid_buff[INT32_TO_STRING_MAX + 1]; /* + 1 for null terminator */
	boolean_t     drop_cache;
	uint64_t      db_oid;

	TRACE(CUSTOM_ERROR, "Searching for table %s", table_name);

	/* We look to see if the table has already been loaded, and if so we get the pointer to process-local memory
	 * from the YDB local variable, then setup the stmt pointer. Else, we load the binary from the database
	 * and store a pointer to the parsed schema.
	 *
	 * Once a table is loaded from the database, it is stored in process local memory and used from there unless
	 * a later CREATE TABLE or DROP TABLE statement has concurrently executed. In this case, we load the table
	 * again from the database and release the process local memory that was housing the previous table definition.
	 */
	YDB_STRING_TO_BUFFER(config->global_names.loadedschemas, &varname);
	YDB_STRING_TO_BUFFER("tables", &subs_array[0]);
	YDB_STRING_TO_BUFFER((char *)table_name, &subs_array[1]);
	ret.buf_addr = &retbuff[0];
	ret.len_alloc = sizeof(retbuff);
	status = ydb_get_s(&varname, 2, &subs_array[0], &ret);
	switch (status) {
	case YDB_OK:
		/* We have the table definition already stored in process local memory. Use that as long as the
		 * table definition has not changed in the database.
		 */
		stmt = *((SqlStatement **)ret.buf_addr);
		UNPACK_SQL_STATEMENT(table, stmt, create_table);
		/* Check if table has not changed in the database since we cached it */
		YDB_STRING_TO_BUFFER(config->global_names.schema, &varname);
		YDB_STRING_TO_BUFFER((char *)table_name, &subs_array[0]);
		YDB_STRING_TO_BUFFER("pg_class", &subs_array[1]);
		ret.buf_addr = &oid_buff[0];
		ret.len_alloc = sizeof(oid_buff);
		status = ydb_get_s(&varname, 2, &subs_array[0], &ret);
		switch (status) {
		case YDB_OK:
			assert(ret.len_alloc > ret.len_used); /* ensure space for null terminator */
			ret.buf_addr[ret.len_used] = '\0';    /* null terminate string before invoking "atoi" */
			db_oid = (uint64_t)strtoll(ret.buf_addr, NULL, 10);
			drop_cache = (db_oid != table->oid);
			break;
		case YDB_ERR_GVUNDEF:
			/* A DROP TABLE ran concurrently since we cached it. Reload cache from database. */
			drop_cache = TRUE;
			break;
		default:
			YDB_ERROR_CHECK(status);
			return NULL;
			break;
		}
		if (!drop_cache) {
			return table;
		} else {
			/* We do not expect the loaded table cache to be dropped in the normal case.
			 * This includes most of the bats tests in the YDBOcto repo. Therefore, we assert that this
			 * code path is never reached unless an env var is defined that says this is expected.
			 * This way it serves as a good test case of the fact that once a table is loaded in the process
			 * local cache, it is used almost always except for rare cases when we go reload from the database.
			 */
			assert(NULL != getenv("octo_dbg_drop_cache_expected"));
			status = drop_schema_from_local_cache(&subs_array[0], TableSchema);
			if (YDB_OK != status) {
				/* YDB_ERROR_CHECK would already have been done inside "drop_schema_from_local_cache()" */
				return NULL;
			}
		}
		break;
	case YDB_ERR_LVUNDEF:
		/* Table definition does not exist locally. Fall through to read it from database. */
		break;
	default:
		YDB_ERROR_CHECK(status);
		return NULL;
		break;
	}
	/* Find space (in bytes) used up by table definition from a global variable node */
	table_binary_b = make_buffers(config->global_names.schema, 3, table_name, "", "");
	YDB_MALLOC_BUFFER(&table_binary_b[3], INT32_TO_STRING_MAX + 1); /* + 1 for null terminator */
	/* Set gvn 2nd subscript to "l" to get the length in bytes of the binary table definition */
	YDB_LITERAL_TO_BUFFER("l", &table_binary_b[2]);
	status = ydb_get_s(table_binary_b, 2, &table_binary_b[1], &table_binary_b[3]);
	switch (status) {
	case YDB_OK:
		break;
	case YDB_ERR_GVUNDEF:
		c = table_name;
		while ('\0' != *c) {
			if (*c == '.')
				break;
			c++;
		}
		if ('\0' != *c) {
			/* Temporary workaround until we support schemas (YDBOcto#99).
			 * Try stripping off schema name and see if we find it.
			 */
			YDB_FREE_BUFFER(&table_binary_b[3]);
			free(table_binary_b);
			return find_table(c + 1);
		}
		YDB_FREE_BUFFER(&table_binary_b[3]);
		free(table_binary_b);
		return NULL;
		break;
	default:
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&table_binary_b[3]);
		free(table_binary_b);
		return NULL;
		break;
	}
	table_binary_b[3].buf_addr[table_binary_b[3].len_used] = '\0'; /* null terminate string before invoking "atoi" */
	length = atoi(table_binary_b[3].buf_addr);
	/* Switch gvn 2nd subscript from "l" back to "b" to get the binary table definition */
	YDB_LITERAL_TO_BUFFER("b", &table_binary_b[2]);

	/* Allocate a buffer to hold the full table.
	 * Note that we create a new memory chunk here (different from the memory chunk used to store octo structures
	 * for queries) so that this table structure does not get cleaned when one query finishes and the next query starts.
	 */
	old_chunk = memory_chunks;
	memory_chunks = alloc_chunk(length);
	buff = octo_cmalloc(memory_chunks, length);
	value_b.buf_addr = value_b_buffer;
	value_b.len_alloc = sizeof(value_b_buffer);
	table_binary_b[3].len_used = 0;
	cur_buff = buff;
	while (TRUE) {
		/* See "src/run_query.c" under "case create_table_STATEMENT:" for how these global variable nodes are created */
		status = ydb_subscript_next_s(table_binary_b, 3, &table_binary_b[1], &table_binary_b[3]);
		if (YDB_ERR_NODEEND == status) {
			status = YDB_OK;
			break;
		}
		assert(length > (cur_buff - buff));
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		status = ydb_get_s(table_binary_b, 3, &table_binary_b[1], &value_b);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		memcpy(cur_buff, value_b.buf_addr, value_b.len_used);
		cur_buff += value_b.len_used;
		assert(MAX_STR_CONST >= value_b.len_used);
		if (length == (cur_buff - buff)) {
			break;
		}
	}
	if (YDB_OK != status) {
		OCTO_CFREE(memory_chunks);
		memory_chunks = old_chunk;
		YDB_FREE_BUFFER(&table_binary_b[3]);
		free(table_binary_b);
		return NULL;
	}
	assert(length == (cur_buff - buff));

	stmt = (void *)buff;
	decompress_statement((char *)stmt, length);
	if (NULL == stmt) {
		memory_chunks = old_chunk;
		YDB_FREE_BUFFER(&table_binary_b[3]);
		free(table_binary_b);
		return NULL;
	}
	assign_table_to_columns(stmt);

	// Note the pointer to the loaded parse tree root
	save_value.buf_addr = (char *)&stmt;
	save_value.len_used = save_value.len_alloc = sizeof(void *);
	status = ydb_set_s(&varname, 2, &subs_array[0], &save_value);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		memory_chunks = old_chunk;
		YDB_FREE_BUFFER(&table_binary_b[3]);
		free(table_binary_b);
		return NULL;
	}
	// Note down the memory chunk so we can free it later
	YDB_STRING_TO_BUFFER("chunk", &subs_array[2]);
	save_value.buf_addr = (char *)&memory_chunks;
	save_value.len_used = save_value.len_alloc = sizeof(void *);
	status = ydb_set_s(&varname, 3, &subs_array[0], &save_value);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		memory_chunks = old_chunk;
		YDB_FREE_BUFFER(&table_binary_b[3]);
		free(table_binary_b);
		return NULL;
	}
	UNPACK_SQL_STATEMENT(table, stmt, create_table);

	// Restore memory chunks
	memory_chunks = old_chunk;

	// Free buffers
	YDB_FREE_BUFFER(&table_binary_b[3]);
	free(table_binary_b);

	return table;
}
