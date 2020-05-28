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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "helpers.h"
#include "rocto.h"

#define FREE_HANDLE_PARSE_POINTERS()								\
	status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE);		\
	YDB_ERROR_CHECK(status);								\
	YDB_FREE_BUFFER(&cur_parm_value_buffer);						\
	YDB_FREE_BUFFER(&cur_bind_parm_buffer);							\
	YDB_FREE_BUFFER(&statement_subs[5]);							\
	YDB_FREE_BUFFER(&statement_subs[6]);							\
	YDB_FREE_BUFFER(&all_parms_subs[6]);							\
	YDB_FREE_BUFFER(&cursor_subs[3]);							\
	YDB_FREE_BUFFER(&parm_type_buffer);							\
	YDB_FREE_BUFFER(&cur_parm_buffer);							\
	YDB_FREE_BUFFER(&offset_buffer);							\
	free(parse_context.is_bind_parm);							\
	free(parse_context.types);								\
	free(parse_context_array);								\
	free(all_parms_subs);									\
	free(statement_subs);									\
	status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);		\
	YDB_ERROR_CHECK(status);								\
	free(cursor_subs);									\

int handle_parse(Parse *parse, RoctoSession *session) {
	QueryResponseParms	response_parms;
	ParseComplete		*response;
	ParseContext		parse_context;
	ydb_buffer_t		*statement_subs, *all_parms_subs, *cursor_subs;
	ydb_buffer_t		sql_expression, routine_buffer, tag_buffer, offset_buffer, num_parms_buffer;
	ydb_buffer_t		cur_bind_parm_buffer, parm_type_buffer, cur_parm_buffer, cur_parm_value_buffer;
	uint32_t		data_ret;
	int32_t			status, cur_type, done;
	int16_t			cur_parm, cur_parm_temp, cur_bind_parm, cur_bind_parm_temp;
	int16_t			*parse_context_array;
	size_t			query_length = 0;
	char			cursor_str[INT64_TO_STRING_MAX];

	TRACE(ERR_ENTERING_FUNCTION, "handle_parse");

	// Create separate buffer arrays for tracking all literal parameters and just user-specified ("bind") parameters
	// We track these separately as all literal parameters need to be accessible to the physical plan, while handle_bind needs
	// to perform operations using the user-specified parameters only. Keeping two subsarrays saves repeated mallocs to toggle
	// between each set of parameters.
	// Set the subscripts to store routine name: session(id, "prepared", <name>, "routine"
	statement_subs = make_buffers(config->global_names.session, 4, session->session_id->buf_addr, "prepared", parse->dest,
			"routine");
	// Set the subscripts for all prepared statement parameters: session(id, "prepared", <name>, "parameters", "all", ...)
	all_parms_subs = make_buffers(config->global_names.session, 6, session->session_id->buf_addr, "prepared", parse->dest,
			"parameters", "all", "");
	// Check if a prepared statement by the same name already exists and, if so, delete it before reusing the name for a new one
	status = ydb_data_s(&statement_subs[0], 3, &statement_subs[1], &data_ret);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		free(statement_subs);
		free(all_parms_subs);
		return 1;
	}
	if (0 < data_ret) {
		status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			free(statement_subs);
			free(all_parms_subs);
			return 1;
		}
	}
	YDB_MALLOC_BUFFER(&sql_expression, MAX_STR_CONST);
	YDB_COPY_STRING_TO_BUFFER(parse->query, &sql_expression, done);
	if (!done) {
		ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
		YDB_FREE_BUFFER(&sql_expression);
		free(statement_subs);
		free(all_parms_subs);
		return 1;
	}

	// Add the new SQL query to the database
	status = ydb_set_s(&statement_subs[0], 3, &statement_subs[1], &sql_expression);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		YDB_FREE_BUFFER(&sql_expression);
		free(statement_subs);
		free(all_parms_subs);
		return 1;
	}

	// Store query in input buffer
	query_length = sql_expression.len_used;
	memcpy(input_buffer_combined, sql_expression.buf_addr, query_length);
	eof_hit = FALSE;
	input_buffer_combined[query_length] = '\0';
	cur_input_index = 0;
	cur_input_more = &no_more;
	YDB_FREE_BUFFER(&sql_expression);

	// Prepare parameter offset array; the number of parameters isn't known yet, so just use the max
	parse_context_array = (int16_t*)calloc((INT16_MAX * 2), sizeof(int16_t));

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
	parse_context.types = (PSQL_TypeOid*)calloc(parse_context.types_size, sizeof(PSQL_TypeOid));
	parse_context.num_bind_parm_types = parse->num_parm_data_types;
	for (cur_type = 0; cur_type < parse->num_parm_data_types; cur_type++) {
		parse_context.types[cur_type] = parse->parm_data_types[cur_type];
	}
	parse_context.is_bind_parm_size = 8;
	parse_context.is_bind_parm = (boolean_t*)calloc(parse_context.is_bind_parm_size, sizeof(boolean_t));		// Start with 8 booleans and expand on demand
	// Defer cursor cleanup until the end of the function since we need to copy the parameters from there to the
	// prepared statement for later use by handle_bind and handle_execute
	parse_context.skip_cursor_cleanup = TRUE;

	// Parse query to get parameter count, types, and generate plan
	response_parms.session = session;
	status = run_query(&handle_query_response, (void*)&response_parms, FALSE, &parse_context);
	if (0 != status) {
		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		free(statement_subs);
		free(all_parms_subs);
		return 1;
	}
	// Prepare cursor buffers
	snprintf(cursor_str, INT64_TO_STRING_MAX, "%ld", parse_context.cursorId);
	cursor_subs = make_buffers(config->global_names.cursor, 3, cursor_str, "parameters", "");

	// Store routine name
	YDB_MALLOC_BUFFER(&routine_buffer, MAX_ROUTINE_LEN);
	memcpy(routine_buffer.buf_addr, parse_context.routine, MAX_ROUTINE_LEN);
	routine_buffer.len_used = MAX_ROUTINE_LEN;
	status = ydb_set_s(&statement_subs[0], 4, &statement_subs[1], &routine_buffer);
	YDB_FREE_BUFFER(&routine_buffer);
	if (YDB_OK != status) {
		status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		free(statement_subs);
		free(all_parms_subs);
		free(cursor_subs);
		return 1;
	}
	// Store command tag
	YDB_MALLOC_BUFFER(&tag_buffer, INT32_TO_STRING_MAX);
	OCTO_INT32_TO_BUFFER(parse_context.command_tag, &tag_buffer);
	YDB_MALLOC_BUFFER(&statement_subs[4], MAX_TAG_LEN);
	YDB_COPY_STRING_TO_BUFFER("tag", &statement_subs[4], done);
	if (!done) {
		ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
		status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&statement_subs[4]);
		YDB_FREE_BUFFER(&tag_buffer);
		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		free(statement_subs);
		free(all_parms_subs);
		free(cursor_subs);
		return 1;
	}
	status = ydb_set_s(&statement_subs[0], 4, &statement_subs[1], &tag_buffer);
	YDB_FREE_BUFFER(&statement_subs[4]);
	YDB_FREE_BUFFER(&tag_buffer);
	if (YDB_OK != status) {
		status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		free(statement_subs);
		free(all_parms_subs);
		free(cursor_subs);
		return 1;
	}
	free(statement_subs);

	// Set the subscripts for all prepared statement parameters: session(id, "prepared", <name>, "parameters", ...)
	statement_subs = make_buffers(config->global_names.session, 6, session->session_id->buf_addr, "prepared", parse->dest,
			"parameters", "", "");
	// Store number of extended query (bind) parameters: session(id, "prepared", <name>, "parameters")
	YDB_MALLOC_BUFFER(&num_parms_buffer, INT16_TO_STRING_MAX);
	OCTO_INT16_TO_BUFFER(parse_context.num_bind_parms, &num_parms_buffer);
	status = ydb_set_s(&statement_subs[0], 4, &statement_subs[1], &num_parms_buffer);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&num_parms_buffer);
		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		free(all_parms_subs);
		free(statement_subs);
		free(cursor_subs);
		return 1;
	}
	// Store total number of parameters: session(id, "prepared", <name>, "parameters", "all")
	OCTO_INT16_TO_BUFFER(parse_context.total_parms, &num_parms_buffer);
	status = ydb_set_s(&all_parms_subs[0], 5, &all_parms_subs[1], &num_parms_buffer);
	YDB_FREE_BUFFER(&num_parms_buffer);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&statement_subs[0], 3, &statement_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		free(all_parms_subs);
		free(statement_subs);
		free(cursor_subs);
		return 1;
	}
	// SET or SHOW statements don't have plans to execute, so just return
	if (!parse_context.is_select) {
		response = make_parse_complete();
		send_message(session, (BaseMessage*)(&response->type));
		free(response);
		// The cursor is no longer needed as there are no parameters to extract for later binding
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);

		free(parse_context.is_bind_parm);
		free(parse_context.types);
		free(parse_context_array);
		free(statement_subs);
		free(all_parms_subs);
		free(cursor_subs);
		return 0;
	}

	// Store number of parameters and any parameter type information for later use in ParameterDescription messages
	YDB_MALLOC_BUFFER(&statement_subs[5], INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&statement_subs[6], INT16_TO_STRING_MAX);	// This stores one of the following: "type", "start", "end"
	YDB_MALLOC_BUFFER(&all_parms_subs[6], INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&cursor_subs[3], INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&cur_bind_parm_buffer, INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&cur_parm_value_buffer, MAX_STR_CONST);
	YDB_MALLOC_BUFFER(&parm_type_buffer, INT32_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&cur_parm_buffer, INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&offset_buffer, INT16_TO_STRING_MAX);
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
			cur_bind_parm_temp = cur_bind_parm + 1;		// Convert from 0-indexing to 1-indexing
			OCTO_INT16_TO_BUFFER(cur_bind_parm_temp, &statement_subs[5]);
			// Store parameter type
			YDB_COPY_STRING_TO_BUFFER("type", &statement_subs[6], done);
			if (!done) {
				ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
			status = ydb_set_s(&statement_subs[0], 6, &statement_subs[1], &parm_type_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
			// Store parameter start offset
			OCTO_INT16_TO_BUFFER(parse_context.parm_start[cur_bind_parm], &offset_buffer);
			YDB_COPY_STRING_TO_BUFFER("start", &statement_subs[6], done);
			if (!done) {
				ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
			status = ydb_set_s(&statement_subs[0], 6, &statement_subs[1], &offset_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
			// Store parameter end offset
			OCTO_INT16_TO_BUFFER(parse_context.parm_end[cur_bind_parm], &offset_buffer);
			YDB_COPY_STRING_TO_BUFFER("end", &statement_subs[6], done);
			if (!done) {
				ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
			status = ydb_set_s(&statement_subs[0], 6, &statement_subs[1], &offset_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
			cur_bind_parm++;
		} else {
			cur_parm_temp = cur_parm + 1;
			OCTO_INT16_TO_BUFFER(cur_parm_temp, &cur_parm_buffer);
			// Copy literal parameter value from cursor to prepared statement for later retrieval by handle_execute
			YDB_COPY_BUFFER_TO_BUFFER(&cur_parm_buffer, &cursor_subs[3], done);
			if (!done) {
				ERROR(ERR_YOTTADB, "YDB_COPY_BUFFER_TO_BUFFER failed");
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
			YDB_COPY_BUFFER_TO_BUFFER(&cur_parm_buffer, &all_parms_subs[6], done);
			if (!done) {
				ERROR(ERR_YOTTADB, "YDB_COPY_BUFFER_TO_BUFFER failed");
				FREE_HANDLE_PARSE_POINTERS();
				return 1;
			}
			status = ydb_get_s(&cursor_subs[0], 3, &cursor_subs[1], &cur_parm_value_buffer);
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
	YDB_FREE_BUFFER(&cur_bind_parm_buffer);
	YDB_FREE_BUFFER(&cur_parm_value_buffer);
	YDB_FREE_BUFFER(&statement_subs[5]);
	YDB_FREE_BUFFER(&statement_subs[6]);
	YDB_FREE_BUFFER(&all_parms_subs[6]);
	YDB_FREE_BUFFER(&cursor_subs[3]);
	YDB_FREE_BUFFER(&parm_type_buffer);
	YDB_FREE_BUFFER(&cur_parm_buffer);
	YDB_FREE_BUFFER(&offset_buffer);
	free(parse_context.is_bind_parm);
	free(parse_context.types);
	free(parse_context_array);
	free(statement_subs);
	free(all_parms_subs);

	// Cleanup cursor
	status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
	YDB_ERROR_CHECK(status);
	free(cursor_subs);

	YDB_ERROR_CHECK(status);
	response = make_parse_complete();
	send_message(session, (BaseMessage*)(&response->type));
	free(response);

	return 0;
}
