/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "helpers.h"
#include "rocto.h"

#define FREE_HANDLE_PARSE_POINTERS()                                                    \
	status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE); \
	YDB_ERROR_CHECK(status);                                                        \
	YDB_FREE_BUFFER(&cur_parm_value_buffer);                                        \
	free(parse_context.is_bind_parm);                                               \
	free(parse_context.types);                                                      \
	free(parse_context_array);                                                      \
	status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);       \
	YDB_ERROR_CHECK(status);

int handle_parse(Parse *parse, RoctoSession *session) {
	QueryResponseParms response_parms;
	ParseComplete *	   response;
	ParseContext	   parse_context;
	ydb_buffer_t	   statement_subs[7], all_parms_subs[7], cursor_subs[4];
	ydb_buffer_t	   sql_expression, routine_buffer, tag_buffer, offset_buffer, num_parms_buffer;
	ydb_buffer_t	   parm_type_buffer, cur_parm_value_buffer;
	uint32_t	   data_ret;
	int32_t		   status, cur_type;
	int32_t *	   parse_context_array;
	int16_t		   cur_parm, cur_parm_temp, cur_bind_parm, cur_bind_parm_temp;
	char		   cursor_str[INT64_TO_STRING_MAX], cur_parm_str[INT16_TO_STRING_MAX];
	char		   parm_type_str[INT32_TO_STRING_MAX], parm_attr_str[INT32_TO_STRING_MAX];
	char		   tag_str[INT32_TO_STRING_MAX], routine_str[MAX_ROUTINE_LEN];
	char		   num_parms_str[INT16_TO_STRING_MAX], offset_str[INT16_TO_STRING_MAX];
	char		   cur_bind_parm_str[INT16_TO_STRING_MAX];

	TRACE(INFO_ENTERING_FUNCTION, "handle_parse");

	// Create separate buffer arrays for tracking all literal parameters and just user-specified ("bind") parameters
	// We track these separately as all literal parameters need to be accessible to the physical plan, while handle_bind needs
	// to perform operations using the user-specified parameters only.
	// Set the subscripts to store routine name: session(id, OCTOLIT_PREPARED, <name>, OCTOLIT_ROUTINE
	YDB_STRING_TO_BUFFER(config->global_names.session, &statement_subs[0]);
	YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &statement_subs[1]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PREPARED, &statement_subs[2]);
	YDB_STRING_TO_BUFFER(parse->dest, &statement_subs[3]);
	YDB_STRING_TO_BUFFER(OCTOLIT_ROUTINE, &statement_subs[4]);

	// Set the subscripts for all prepared statement parameters: session(id, OCTOLIT_PREPARED, <name>, OCTOLIT_PARAMETERS,
	// OCTOLIT_ALL, ...)
	YDB_STRING_TO_BUFFER(config->global_names.session, &all_parms_subs[0]);
	YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &all_parms_subs[1]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PREPARED, &all_parms_subs[2]);
	YDB_STRING_TO_BUFFER(parse->dest, &all_parms_subs[3]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PARAMETERS, &all_parms_subs[4]);
	YDB_STRING_TO_BUFFER(OCTOLIT_ALL, &all_parms_subs[5]);

	// Check if a prepared statement by the same name already exists and, if so, delete it before reusing the name for a new one
	status = ydb_data_s(&statement_subs[0], 3, &statement_subs[1], &data_ret);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	if (0 < data_ret) {
		status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			return 1;
		}
	}
	YDB_STRING_TO_BUFFER(parse->query, &sql_expression);

	// Add the new SQL query to the database
	status = ydb_set_s(&statement_subs[0], 3, &statement_subs[1], &sql_expression);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}

	// Store query in input buffer
	COPY_QUERY_TO_INPUT_BUFFER(sql_expression.buf_addr, (int)sql_expression.len_used, NEWLINE_NEEDED_FALSE);
	/* Prepare parameter offset array; the number of parameters isn't known yet, so just use the max
	 */
	parse_context_array = (int32_t *)calloc((INT16_MAX * 2), sizeof(int32_t));

	// Prepare ParseContext to store parameter information
	memset(&parse_context, 0, sizeof(ParseContext));
	parse_context.is_extended_query = TRUE;
	parse_context.parm_start = &parse_context_array[0];
	parse_context.parm_end = &parse_context_array[INT16_MAX];
	// If client specifies a number of parameter data types, start with those, otherwise start with 8 as a default
	if (0 < parse->num_parm_data_types)
		parse_context.types_size = parse->num_parm_data_types;
	else
		parse_context.types_size = 8;
	parse_context.types = (PSQL_TypeOid *)calloc(parse_context.types_size, sizeof(PSQL_TypeOid));
	parse_context.num_bind_parm_types = parse->num_parm_data_types;
	for (cur_type = 0; cur_type < parse->num_parm_data_types; cur_type++) {
		parse_context.types[cur_type] = parse->parm_data_types[cur_type];
	}
	parse_context.is_bind_parm_size = 8;
	parse_context.is_bind_parm
	    = (boolean_t *)calloc(parse_context.is_bind_parm_size, sizeof(boolean_t)); // Start with 8 booleans and expand on demand
	// Defer cursor cleanup until the end of the function since we need to copy the parameters from there to the
	// prepared statement for later use by handle_bind and handle_execute
	parse_context.skip_cursor_cleanup = TRUE;

	// Parse query to get parameter count, types, and generate plan
	response_parms.session = session;
	status = run_query(&handle_query_response, (void *)&response_parms, FALSE, &parse_context);
	if (0 != status) {
		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		return 1;
	}

	// Prepare cursor buffers
	snprintf(cursor_str, INT64_TO_STRING_MAX, "%ld", parse_context.cursorId);
	YDB_STRING_TO_BUFFER(config->global_names.cursor, &cursor_subs[0]);
	YDB_STRING_TO_BUFFER(cursor_str, &cursor_subs[1]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PARAMETERS, &cursor_subs[2]);
	// Store routine name
	routine_buffer.buf_addr = routine_str;
	routine_buffer.len_alloc = sizeof(routine_str);
	memcpy(routine_buffer.buf_addr, parse_context.routine, MAX_ROUTINE_LEN);
	routine_buffer.len_used = MAX_ROUTINE_LEN;
	status = ydb_set_s(&statement_subs[0], 4, &statement_subs[1], &routine_buffer);
	if (YDB_OK != status) {
		status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		return 1;
	}

	// Store command tag
	tag_buffer.buf_addr = tag_str;
	tag_buffer.len_alloc = sizeof(tag_str);
	OCTO_INT32_TO_BUFFER(parse_context.command_tag, &tag_buffer);
	YDB_STRING_TO_BUFFER(OCTOLIT_TAG, &statement_subs[4]);
	status = ydb_set_s(&statement_subs[0], 4, &statement_subs[1], &tag_buffer);
	if (YDB_OK != status) {
		status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		return 1;
	}

	// Set the subscripts for all prepared statement parameters: session(id, OCTOLIT_PREPARED, <name>, OCTOLIT_PARAMETERS, ...)
	YDB_STRING_TO_BUFFER(OCTOLIT_PARAMETERS, &statement_subs[4]);
	// Store number of extended query (bind) parameters: session(id, OCTOLIT_PREPARED, <name>, OCTOLIT_PARAMETERS)
	num_parms_buffer.buf_addr = num_parms_str;
	num_parms_buffer.len_alloc = sizeof(num_parms_str);
	OCTO_INT16_TO_BUFFER(parse_context.num_bind_parms, &num_parms_buffer);
	status = ydb_set_s(&statement_subs[0], 4, &statement_subs[1], &num_parms_buffer);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		return 1;
	}
	// Store total number of parameters: session(id, OCTOLIT_PREPARED, <name>, OCTOLIT_PARAMETERS, OCTOLIT_ALL)
	OCTO_INT16_TO_BUFFER(parse_context.total_parms, &num_parms_buffer);
	status = ydb_set_s(&all_parms_subs[0], 5, &all_parms_subs[1], &num_parms_buffer);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		return 1;
	}
	switch (parse_context.command_tag) {
	case select_STATEMENT:
	case insert_STATEMENT:
		/* Queries containing SELECT and INSERT have plans to execute so continue processing */
		break;
	case set_STATEMENT:
	case show_STATEMENT:
	case create_table_STATEMENT:
	case drop_table_STATEMENT:
	case create_function_STATEMENT:
	case drop_function_STATEMENT:
	case begin_STATEMENT:
	case commit_STATEMENT:
	case discard_all_STATEMENT:
		/* Queries are of type SET, SHOW, CREATE TABLE, DISCARD ALL etc. They don't have any plans. Just return. */
		response = make_parse_complete();
		send_message(session, (BaseMessage *)(&response->type));
		free(response);
		// The cursor is no longer needed as there are no parameters to extract for later binding
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		return 0;
	case index_STATEMENT: /* This is currently unimplemented and is hence placed alongside the "default:" case */
	default:
		assert(FALSE);
		break;
	}

	// Initialize buffers using stack-allocated char*s to avoid needless malloc/frees
	statement_subs[5].buf_addr = cur_bind_parm_str;
	statement_subs[5].len_alloc = sizeof(cur_bind_parm_str);
	statement_subs[6].buf_addr = parm_attr_str;
	statement_subs[6].len_alloc = sizeof(parm_attr_str);
	// In the loop below, we copy a value from a cursor LVN to a query parameter LVN, so only one string needed for this node.
	all_parms_subs[6].buf_addr = cursor_subs[3].buf_addr = cur_parm_str;
	all_parms_subs[6].len_alloc = cursor_subs[3].len_alloc = sizeof(cur_parm_str);
	YDB_MALLOC_BUFFER(&cur_parm_value_buffer, OCTO_INIT_BUFFER_LEN);
	parm_type_buffer.buf_addr = parm_type_str;
	parm_type_buffer.len_alloc = sizeof(parm_type_str);
	offset_buffer.buf_addr = offset_str;
	offset_buffer.len_alloc = sizeof(offset_str);
	/* Store number of parameters and any parameter type information for later use in handle_bind.c and in
	 * ParameterDescription messages. For more context on the parameter storage done here, please see the comment preceding the
	 * principal for loop in handle_bind.c.
	 */
	for (cur_parm = 0, cur_bind_parm = 0; cur_parm < parse_context.total_parms; cur_parm++) {
		if ((cur_parm <= parse_context.is_bind_parm_size) && (parse_context.is_bind_parm[cur_parm])) {
			// Only need type information for bind parameters
			if (cur_bind_parm < parse_context.num_bind_parms) {
				if ((0 < parse_context.num_bind_parms) && (cur_bind_parm >= parse->num_parm_data_types)) {
					assert(cur_bind_parm < parse_context.cur_type);
					OCTO_INT32_TO_BUFFER(parse_context.types[cur_bind_parm], &parm_type_buffer);
				} else {
					OCTO_INT32_TO_BUFFER(parse->parm_data_types[cur_bind_parm], &parm_type_buffer);
				}
			}
			// Update parameter number subscript
			cur_bind_parm_temp = cur_bind_parm + 1; // Convert from 0-indexing to 1-indexing
			OCTO_INT16_TO_BUFFER(cur_bind_parm_temp, &statement_subs[5]);
			// Store parameter type
			YDB_STRING_TO_BUFFER("type", &statement_subs[6]);
			status = ydb_set_s(&statement_subs[0], 6, &statement_subs[1], &parm_type_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
			// Store parameter start offset
			OCTO_INT32_TO_BUFFER(parse_context.parm_start[cur_bind_parm], &offset_buffer);
			YDB_STRING_TO_BUFFER("start", &statement_subs[6]);
			status = ydb_set_s(&statement_subs[0], 6, &statement_subs[1], &offset_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
			// Store parameter end offset
			OCTO_INT32_TO_BUFFER(parse_context.parm_end[cur_bind_parm], &offset_buffer);
			YDB_STRING_TO_BUFFER("end", &statement_subs[6]);
			status = ydb_set_s(&statement_subs[0], 6, &statement_subs[1], &offset_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
			cur_bind_parm++;
		} else {
			cur_parm_temp = cur_parm + 1;
			// These buffers share their buf_addr char*, so update len_used accordingly
			OCTO_INT16_TO_BUFFER(cur_parm_temp, &cursor_subs[3]);
			all_parms_subs[6].len_used = cursor_subs[3].len_used;
			// Copy literal parameter value from cursor to prepared statement for later retrieval by handle_execute
			status = ydb_get_s(&cursor_subs[0], 3, &cursor_subs[1], &cur_parm_value_buffer);
			if (YDB_ERR_INVSTRLEN == status) {
				EXPAND_YDB_BUFFER_T_ALLOCATION(cur_parm_value_buffer);
				status = ydb_get_s(&cursor_subs[0], 3, &cursor_subs[1], &cur_parm_value_buffer);
				assert(YDB_ERR_INVSTRLEN != status);
			}
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
			status = ydb_set_s(&all_parms_subs[0], 6, &all_parms_subs[1], &cur_parm_value_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
		}
	}
	YDB_FREE_BUFFER(&cur_parm_value_buffer);
	free(parse_context.is_bind_parm);
	free(parse_context.types);
	free(parse_context_array);

	// Cleanup cursor
	status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
	YDB_ERROR_CHECK(status);

	YDB_ERROR_CHECK(status);
	response = make_parse_complete();
	send_message(session, (BaseMessage *)(&response->type));
	free(response);

	return 0;
}
