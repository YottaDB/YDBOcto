/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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

#define CLEAN_UP_AND_RETURN(MEMORY_CHUNKS, OLD_CHUNK, TABLE_BINARY_B, TABLE_BINARY_B_PTR, LIT_VIEWS, IS_VIEW) \
	{                                                                                                     \
		if (!IS_VIEW) {                                                                               \
			OCTO_CFREE(MEMORY_CHUNKS);                                                            \
			assert(NULL != (OLD_CHUNK));                                                          \
			(MEMORY_CHUNKS) = (OLD_CHUNK);                                                        \
		}                                                                                             \
		YDB_FREE_BUFFER(&(LIT_VIEWS));                                                                \
		YDB_FREE_BUFFER(&(TABLE_BINARY_B));                                                           \
		free(TABLE_BINARY_B_PTR);                                                                     \
		return NULL;                                                                                  \
	}
SqlStatement *find_view_or_table_in_cache(const char *view_or_table_name, const char *view_or_table_str) {
	SqlStatement *table_or_view_stmt;
	int	      status;
	ydb_buffer_t  varname, subs_array[3], ret;
	char	      retbuff[sizeof(void *)];
	char	      oid_buff[INT32_TO_STRING_MAX + 1]; /* + 1 for null terminator */
	boolean_t     drop_cache;

	YDB_STRING_TO_BUFFER(config->global_names.loadedschemas, &varname);
	YDB_STRING_TO_BUFFER((char *)view_or_table_str, &subs_array[0]);
	YDB_STRING_TO_BUFFER((char *)view_or_table_name, &subs_array[1]);
	OCTO_SET_BUFFER(ret, retbuff);
	status = ydb_get_s(&varname, 2, &subs_array[0], &ret);
	switch (status) {
	case YDB_OK:;
		ydb_buffer_t varname2, subs_array2[3]; /* needed so as to preserve "varname" and "subs_array" for later code */

		/* We have the table/view definition already stored in memory (process memory for table and memory chunks that
		 * exists for the duration of query execution for a view). Use that as long as the
		 * definition has not changed in the database.
		 */
		table_or_view_stmt = *((SqlStatement **)ret.buf_addr);

		uint64_t *oid_ptr;
		if (create_table_STATEMENT == table_or_view_stmt->type) {
			oid_ptr = &table_or_view_stmt->v.create_table->oid;
			/* Check if table has not changed in the database since we cached it */
			YDB_STRING_TO_BUFFER(config->global_names.schema, &varname2);
			YDB_STRING_TO_BUFFER((char *)view_or_table_name, &subs_array2[0]);
			YDB_STRING_TO_BUFFER(OCTOLIT_PG_CLASS, &subs_array2[1]);
			OCTO_SET_NULL_TERMINATED_BUFFER(ret, oid_buff);
			status = ydb_get_s(&varname2, 2, &subs_array2[0], &ret);
			switch (status) {
			case YDB_OK:
				assert(ret.len_alloc > ret.len_used); /* ensure space for null terminator */
				ret.buf_addr[ret.len_used] = '\0';    /* null terminate string before invoking "atoi" */
				uint64_t db_oid;
				db_oid = (uint64_t)strtoll(ret.buf_addr, NULL, 10);
				if ((LLONG_MIN == (long long)db_oid) || (LLONG_MAX == (long long)db_oid)) {
					ERROR(ERR_SYSCALL_WITH_ARG, "strtoll()", errno, strerror(errno), ret.buf_addr);
					return NULL;
				}
				drop_cache = (db_oid != (*oid_ptr));
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
				return table_or_view_stmt;
			} else {
				/* We do not expect the loaded table cache to be dropped in the normal case.
				 * This includes most of the bats tests in the YDBOcto repo. Therefore, we assert that this
				 * code path is never reached unless an env var is defined that says this is expected.
				 * This way it serves as a good test case of the fact that once a table is loaded in the
				 * cache, it is used almost always except for rare cases when we go reload from the database.
				 */
				assert(NULL != getenv("octo_dbg_drop_cache_expected"));
				status = drop_schema_from_local_cache(&subs_array2[0], TableSchema, NULL);
				if (YDB_OK != status) {
					/* YDB_ERROR_CHECK would already have been done inside "drop_schema_from_local_cache()" */
					return NULL;
				}
			}
		} else {
			/* Since we reset cache for every query, no need to worry about definition being different in cache and
			 * database for a view.
			 */
			assert(create_view_STATEMENT == table_or_view_stmt->type);
			return table_or_view_stmt;
		}
		break;
	case YDB_ERR_LVUNDEF:
		/* Table/View definition does not exist locally. Fall through to read it from database. */
		break;
	default:
		YDB_ERROR_CHECK(status);
		return NULL;
		break;
	}
	return NULL;
}
// Note that this function is very similar to find_function.c, so changes there may need to be reflected here also.
SqlStatement *find_view_or_table(const char *view_or_table_name) {
	SqlStatement *table_or_view_stmt;
	int	      status;
	int	      length;
	MemoryChunk  *old_chunk;
	ydb_buffer_t  varname, subs_array[3];
	char	     *buff, *cur_buff;
	ydb_buffer_t  save_value;
	ydb_buffer_t *table_or_view_binary_b;
	ydb_buffer_t  value_b;
	char	      value_b_buffer[MAX_DEFINITION_FRAGMENT_SIZE];
	const char   *c;

	TRACE(INFO_TABLE_OR_VIEW_SEARCH, view_or_table_name);
	/* This function is used to find both views and tables.
	 * We look to see if the table/view has already been loaded, and if so we get the pointer to memory
	 * from the YDB local variable, then setup the table_or_view_stmt pointer. Else, we load the binary from the database
	 * and store a pointer to the parsed schema.
	 *
	 * Once a table/view is loaded from the database, it is stored in memory and used from there unless
	 * a later CREATE TABLE/VIEW or DROP TABLE/VIEW statement has concurrently executed. In this case, we load the table/view
	 * again from the database and release the memory that was housing the previous table/view definition.
	 *
	 */
	table_or_view_stmt = find_view_or_table_in_cache(view_or_table_name, OCTOLIT_TABLES);
	if (NULL != table_or_view_stmt) {
		return table_or_view_stmt;
	}
	table_or_view_stmt = find_view_or_table_in_cache(view_or_table_name, OCTOLIT_VIEWS);
	if (NULL != table_or_view_stmt) {
		return table_or_view_stmt;
	}
	/* Find space (in bytes) used up by table or view definition from a global variable node */
	table_or_view_binary_b = make_buffers(config->global_names.schema, 3, view_or_table_name, "", "");
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&table_or_view_binary_b[3], INT32_TO_STRING_MAX);
	/* Set gvn 2nd subscript to OCTOLIT_LENGTH to get the length in bytes of the binary table/view definition */
	YDB_LITERAL_TO_BUFFER(OCTOLIT_LENGTH, &table_or_view_binary_b[2]);
	status = ydb_get_s(table_or_view_binary_b, 2, &table_or_view_binary_b[1], &table_or_view_binary_b[3]);
	switch (status) {
	case YDB_OK:
		break;
	case YDB_ERR_GVUNDEF:
		c = view_or_table_name;
		while ('\0' != *c) {
			if (*c == '.')
				break;
			c++;
		}
		if ('\0' != *c) {
			/* Temporary workaround until we support schemas (YDBOcto#99).
			 * Try stripping off schema name and see if we find it.
			 */
			YDB_FREE_BUFFER(&table_or_view_binary_b[3]);
			free(table_or_view_binary_b);
			return find_view_or_table(c + 1);
		}
		YDB_FREE_BUFFER(&table_or_view_binary_b[3]);
		free(table_or_view_binary_b);
		return NULL;
		break;
	default:
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&table_or_view_binary_b[3]);
		free(table_or_view_binary_b);
		return NULL;
		break;
	}
	table_or_view_binary_b[3].buf_addr[table_or_view_binary_b[3].len_used]
	    = '\0'; /* null terminate string before invoking "atoi" */
	length = atoi(table_or_view_binary_b[3].buf_addr);
	/* Determine if this is a view or a table.
	 * A view will have ^%ydboctoschema(view_name)=OCTOLIT_VIEW node
	 * A table will have ^%ydboctoschema(table_name)=OCTOLIT_TABLE node
	 */
	ydb_buffer_t lit_view;
	boolean_t    is_view = FALSE;
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&lit_view, OCTO_MAX_IDENT);
	status = ydb_get_s(table_or_view_binary_b, 1, &table_or_view_binary_b[1], &lit_view);
	switch (status) {
	case YDB_OK:
		/* Its a view, use query level memory chunk as it will be removed
		 * after the query is processed which is required as views
		 * can have `unique_id`s in its binary definition and these have to be
		 * reset to the current query's context.
		 */
		assert(lit_view.len_alloc > lit_view.len_used);
		lit_view.buf_addr[lit_view.len_used] = '\0';
		is_view = (0 == strcmp(lit_view.buf_addr, OCTOLIT_VIEW)) ? TRUE : FALSE;
		if (is_view) {
			old_chunk = NULL;
			if (config->in_auto_upgrade_binary_view_definition) {
				/* Check that the view is in %ydboctoViewCreated(viewname). This ensures that stale bin definition
				 * is not referred.
				 */
				// Setup LVN buffer
				ydb_buffer_t view_created_list_name_buffer;
				YDB_STRING_TO_BUFFER(OCTOLIT_YDBOCTOVIEWCREATED, &view_created_list_name_buffer);
				// Setup return variable
				ydb_buffer_t ret_val;
				OCTO_MALLOC_NULL_TERMINATED_BUFFER(&ret_val, OCTO_INIT_BUFFER_LEN);
				// Get LVN
				status = ydb_get_s(&view_created_list_name_buffer, 1, &table_or_view_binary_b[1], &ret_val);
				switch (status) {
				case YDB_OK:
					// The binary definition is valid. Continue processing.
					YDB_FREE_BUFFER(&ret_val);
					break;
				case YDB_ERR_LVUNDEF:
					// The binary definition is not yet upgraded. Avoid further processing.
					/* Fall through */
				default:
					YDB_FREE_BUFFER(&ret_val);
					YDB_FREE_BUFFER(&table_or_view_binary_b[3]);
					YDB_FREE_BUFFER(&lit_view);
					free(table_or_view_binary_b);
					return NULL;
					break;
				}
			}

		} else {
			/* Allocate a buffer to hold the full table.
			 * Note that we create a new memory chunk here (different from the memory chunk used to store octo
			 * structures for queries) so that this table structure does not get cleaned when one query finishes and the
			 * next query starts.
			 */
			old_chunk = memory_chunks;
			memory_chunks = alloc_chunk(length);
		}
		break;
	case YDB_ERR_GVUNDEF:
		/* Since the previous ydb_get_s checking the length succeeded, we do not expect this to fail
		 * Auto upgrade cases will fail as previous to YDBOcto#211 such a node for a table was non existant
		 */
		old_chunk = memory_chunks;
		memory_chunks = alloc_chunk(length);
		break;
	default:
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&table_or_view_binary_b[3]);
		YDB_FREE_BUFFER(&lit_view);
		free(table_or_view_binary_b);
		return NULL;
		break;
	}
	/* Switch gvn 2nd subscript from OCTOLIT_LENGTH back to OCTOLIT_BINARY to get the binary table definition */
	YDB_LITERAL_TO_BUFFER(OCTOLIT_BINARY, &table_or_view_binary_b[2]);

	buff = octo_cmalloc(memory_chunks, length);
	value_b.buf_addr = value_b_buffer;
	value_b.len_alloc = sizeof(value_b_buffer);
	table_or_view_binary_b[3].len_used = 0;
	cur_buff = buff;
	while (TRUE) {
		/* See "src/run_query.c" under "case create_table_STATEMENT:" for how these global variable nodes are created */
		status = ydb_subscript_next_s(table_or_view_binary_b, 3, &table_or_view_binary_b[1], &table_or_view_binary_b[3]);
		if (YDB_ERR_NODEEND == status) {
			status = YDB_OK;
			break;
		}
		assert(length > (cur_buff - buff));
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		status = ydb_get_s(table_or_view_binary_b, 3, &table_or_view_binary_b[1], &value_b);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		memcpy(cur_buff, value_b.buf_addr, value_b.len_used);
		cur_buff += value_b.len_used;
		assert(MAX_DEFINITION_FRAGMENT_SIZE >= value_b.len_used);
		if (length == (cur_buff - buff)) {
			break;
		}
	}
	if (YDB_OK != status) {
		CLEAN_UP_AND_RETURN(memory_chunks, old_chunk, table_or_view_binary_b[3], table_or_view_binary_b, lit_view, is_view);
	}
	assert(length == (cur_buff - buff));

	table_or_view_stmt = (void *)buff;
	decompress_statement((char *)table_or_view_stmt, length);
	if (NULL == table_or_view_stmt) {
		CLEAN_UP_AND_RETURN(memory_chunks, old_chunk, table_or_view_binary_b[3], table_or_view_binary_b, lit_view, is_view);
	}

	// Note the pointer to the loaded parse tree root
	YDB_STRING_TO_BUFFER(config->global_names.loadedschemas, &varname);
	char *view_or_table_str;
	if (is_view) {
		view_or_table_str = OCTOLIT_VIEWS;
	} else {
		view_or_table_str = OCTOLIT_TABLES;
	}
	YDB_STRING_TO_BUFFER(view_or_table_str, &subs_array[0]);
	YDB_STRING_TO_BUFFER((char *)view_or_table_name, &subs_array[1]);
	save_value.buf_addr = (char *)&table_or_view_stmt;
	save_value.len_used = save_value.len_alloc = sizeof(void *);
	status = ydb_set_s(&varname, 2, &subs_array[0], &save_value);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		assert(FALSE == is_view);
		CLEAN_UP_AND_RETURN(memory_chunks, old_chunk, table_or_view_binary_b[3], table_or_view_binary_b, lit_view, is_view);
	}
	if (!is_view) {
		// Note down the memory chunk so we can free it later
		YDB_STRING_TO_BUFFER(OCTOLIT_CHUNK, &subs_array[2]);
		save_value.buf_addr = (char *)&memory_chunks;
		save_value.len_used = save_value.len_alloc = sizeof(void *);
		status = ydb_set_s(&varname, 3, &subs_array[0], &save_value);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			assert(FALSE == is_view);
			CLEAN_UP_AND_RETURN(memory_chunks, old_chunk, table_or_view_binary_b[3], table_or_view_binary_b, lit_view,
					    is_view);
		}
		// Restore memory chunks
		assert(NULL != old_chunk);
		memory_chunks = old_chunk;
	}

	// Free buffers
	YDB_FREE_BUFFER(&lit_view);
	YDB_FREE_BUFFER(&table_or_view_binary_b[3]);
	free(table_or_view_binary_b);

	return table_or_view_stmt;
}
