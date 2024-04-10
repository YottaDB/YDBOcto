/****************************************************************
 *								*
 * Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	*
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

#define IS_CREATE_OBJECT_STATEMENT(TYPE) \
	((create_table_STATEMENT == TYPE) || (create_function_STATEMENT == TYPE) || (create_view_STATEMENT == TYPE))
#define IS_DROP_OBJECT_STATEMENT(TYPE) \
	((drop_table_STATEMENT == TYPE) || (drop_function_STATEMENT == TYPE) || (drop_view_STATEMENT == TYPE))

int get_function_hash(SqlStatement *val, char *ret_hash) {
	hash128_state_t state;
	int		status;
	INVOKE_HASH_CANONICAL_QUERY(state, val, status); /* "state" holds final hash */
	if (0 != status) {
		return 1;
	}
	generate_name_type(FunctionHash, &state, 0, ret_hash, MAX_ROUTINE_LEN + 1);
	return 0;
}
/* Following function adds object name to M global ^ydboctoseed(type,name) */
void add_seed_object_to_list(SqlStatement *result) {
	// varname
	ydb_buffer_t ydboctoseed;
	YDB_LITERAL_TO_BUFFER(OCTOLIT_SEED_OBJECT_LIST, &ydboctoseed);

	// subs
	ydb_buffer_t subs_buff[3];

	// type
	char type_str[INT32_TO_STRING_MAX];
	subs_buff[0].buf_addr = type_str;
	subs_buff[0].len_alloc = sizeof(type_str);
	subs_buff[0].len_used = snprintf(subs_buff[0].buf_addr, subs_buff[0].len_alloc, "%d", result->type);

	// obj name
	SqlValue *value;
	int	  parms;
	if (create_table_STATEMENT == result->type) {
		// Add value to gvn
		SqlTable *tbl;
		UNPACK_SQL_STATEMENT(tbl, result, create_table);
		UNPACK_SQL_STATEMENT(value, tbl->tableName, value);
		YDB_STRING_TO_BUFFER(value->v.string_literal, &subs_buff[1]);
		parms = 2;
	} else if (create_function_STATEMENT == result->type) {
		// Add value to gvn with/without args, check
		SqlFunction *function;
		UNPACK_SQL_STATEMENT(function, result, create_function);
		UNPACK_SQL_STATEMENT(value, function->function_name, value);
		YDB_STRING_TO_BUFFER(value->v.string_literal, &subs_buff[1]);
		char function_hash[MAX_ROUTINE_LEN + 1];
		int  ret = get_function_hash(result, function_hash);
		assert(0 == ret);
		UNUSED(ret);
		YDB_STRING_TO_BUFFER(function_hash, &subs_buff[2]);
		parms = 3;
	} else if (create_view_STATEMENT == result->type) {
		// Add value to gvn
		SqlView *view;
		UNPACK_SQL_STATEMENT(view, result, create_view);
		UNPACK_SQL_STATEMENT(value, view->viewName, value);
		YDB_STRING_TO_BUFFER(value->v.string_literal, &subs_buff[1]);
		parms = 2;
	} else {
		assert(FALSE);
		parms = -1;
	}
	int status;
	status = ydb_set_s(&ydboctoseed, parms, &subs_buff[0], NULL);
	assert(YDB_OK == status);
	UNUSED(status);
	return;
}

/* Following function checks if the given object name is present in octo seed list.
 * Input: SqlStatement of the object and a character pointer to hold the return value.
 * Returns:
 * 	   `FALSE` if not in list.
 * 	   `TRUE` if object name found in the list. Object name is also returned through `obj_name`.
 */
boolean_t is_seed_object_being_dropped(SqlStatement *result, char **obj_name) {
	// varname
	ydb_buffer_t ydboctoseed;
	YDB_LITERAL_TO_BUFFER(OCTOLIT_SEED_OBJECT_LIST, &ydboctoseed);

	// subs
	ydb_buffer_t subs_buff[3];

	// type
	char type_str[INT32_TO_STRING_MAX];
	subs_buff[0].buf_addr = type_str;
	subs_buff[0].len_alloc = sizeof(type_str);
	// obj name
	SqlValue *value;
	int	  parms;
	if (drop_table_STATEMENT == result->type) {
		// Add type
		subs_buff[0].len_used = snprintf(subs_buff[0].buf_addr, subs_buff[0].len_alloc, "%d", create_table_STATEMENT);
		// Add value to gvn
		SqlDropTableStatement *tbl;
		UNPACK_SQL_STATEMENT(tbl, result, drop_table);
		UNPACK_SQL_STATEMENT(value, tbl->table_name, value);
		YDB_STRING_TO_BUFFER(value->v.string_literal, &subs_buff[1]);
		parms = 2;
	} else if (drop_function_STATEMENT == result->type) {
		// Add type
		subs_buff[0].len_used = snprintf(subs_buff[0].buf_addr, subs_buff[0].len_alloc, "%d", create_function_STATEMENT);
		// Add value to gvn with/without args, check
		SqlDropFunctionStatement *function;
		UNPACK_SQL_STATEMENT(function, result, drop_function);
		UNPACK_SQL_STATEMENT(value, function->function_name, value);
		YDB_STRING_TO_BUFFER(value->v.string_literal, &subs_buff[1]);
		char function_hash[MAX_ROUTINE_LEN + 1];
		int  ret = get_function_hash(result, function_hash);
		assert(0 == ret);
		UNUSED(ret);
		YDB_STRING_TO_BUFFER(function_hash, &subs_buff[2]);
		parms = 3;
	} else if (drop_view_STATEMENT == result->type) {
		// Add type
		subs_buff[0].len_used = snprintf(subs_buff[0].buf_addr, subs_buff[0].len_alloc, "%d", create_view_STATEMENT);
		// Add value to gvn
		SqlDropViewStatement *view;
		UNPACK_SQL_STATEMENT(view, result, drop_view);
		UNPACK_SQL_STATEMENT(value, view->view_name, value);
		YDB_STRING_TO_BUFFER(value->v.string_literal, &subs_buff[1]);
		parms = 2;
	} else {
		assert(FALSE);
		value = NULL; // avoids [-Wmaybe-uninitialized] warning
		parms = -1;
	}

	// setup return buffer
	ydb_buffer_t ret_buff;
	YDB_MALLOC_BUFFER(&ret_buff, 2); // result is empty string

	// check list
	int status;
	status = ydb_get_s(&ydboctoseed, parms, &subs_buff[0], &ret_buff);
	if (YDB_OK == status) {
		*obj_name = value->v.string_literal;
		YDB_FREE_BUFFER(&ret_buff);
		return TRUE;
	} else {
		YDB_FREE_BUFFER(&ret_buff);
		return FALSE;
	}
}

/* This function creates a list of tables, views and functions in octo-seed.sql.
 * Also, checks if a drop is called in one of the above objects.
 * Returns 1 if object is part of octo-seed.sql otherwise 0.
 */
int ensure_seed_objects_are_not_dropped(SqlStatement *result) {
	if (config->in_auto_load_octo_seed) {
		if (IS_CREATE_OBJECT_STATEMENT(result->type)) {
			add_seed_object_to_list(result);
		}
	} else if (IS_DROP_OBJECT_STATEMENT(result->type)) {
		char *obj_name;
		if (is_seed_object_being_dropped(result, &obj_name)) {
			ERROR(ERR_INVALID_DROP, obj_name);
			return 1;
		}
	}
	return 0;
}
