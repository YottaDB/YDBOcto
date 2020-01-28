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
#include <errno.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

// Used to indicate index 0 for the array of bind parameter format codes specified by a PostgreSQL client in a Bind message
#define FIRST_FORMAT_CODE 0

#define FREE_HANDLE_BIND_POINTERS()							\
	status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);	\
	YDB_ERROR_CHECK(status);							\
	YDB_FREE_BUFFER(&parm_value_buf);						\
	YDB_FREE_BUFFER(&cur_bind_parm_buf);						\
	YDB_FREE_BUFFER(&all_statement_parms_subs[6]);					\
	YDB_FREE_BUFFER(&portal_subs[5]);						\
	YDB_FREE_BUFFER(&statement_subs[5]);						\
	YDB_FREE_BUFFER(&statement_subs[6]);						\
	YDB_FREE_BUFFER(&parm_type_buf);						\
	YDB_FREE_BUFFER(&parm_type_buf);						\
	YDB_FREE_BUFFER(&cur_parm_buf);							\
	YDB_FREE_BUFFER(&offset_buffer);						\
	YDB_FREE_BUFFER(&sql_expression);						\
	free(statement_subs);								\
	free(all_statement_parms_subs);							\
	free(all_portal_parms_subs);							\
	free(portal_subs);

// Args:
//	Bind *bind: A PostgreSQL Bind message
//	RoctoSession *session: Structu containing data for the current client session
// Returns:
//	0 for success, 1 for error
int handle_bind(Bind *bind, RoctoSession *session) {
	// At the moment, we don't have "bound function"
	// This feature should be implemented before 1.0
	ydb_buffer_t	*statement_subs, *portal_subs, *all_statement_parms_subs, *all_portal_parms_subs;
	ydb_buffer_t	num_parms_buf, cur_parm_buf, cur_bind_parm_buf, parm_type_buf, parm_value_buf;
	ydb_buffer_t	sql_expression, routine_buf, tag_buf, offset_buffer;
	uint32_t	data_ret;
	int32_t		status, bound_offset, prepared_offset, done, routine_len = MAX_ROUTINE_LEN;
	int16_t		*parse_context_array;
	int16_t		num_parms, num_bind_parms, cur_parm, cur_bind_parm, cur_parm_temp, cur_bind_parm_temp;
	long int	num_parms_long, offset_long, type_long;
	size_t		bound_query_size;
	char		*bound_query, binary_parm_buffer[MAX_STR_CONST];
	BindComplete	*response;
	ParseContext	parse_context;

	TRACE(ERR_ENTERING_FUNCTION, "handle_bind");

	// Create buffers to get routine name from prepared statement: ^session(id, "prepared", <name>, "routine")
	statement_subs = make_buffers(config->global_names.session, 4, session->session_id->buf_addr, "prepared", bind->source,
			"routine");
	// Create buffers to store routine name on portal: ^session(id, "bound", <name>, "routine")
	portal_subs = make_buffers(config->global_names.session, 4, session->session_id->buf_addr, "bound", bind->dest, "routine");
	// Check if a portal by the same name already exists and, if so, delete it before reusing the name for a new one
	status = ydb_data_s(&portal_subs[0], 3, &portal_subs[1], &data_ret);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	if (0 < data_ret) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			free(statement_subs);
			free(portal_subs);
			return 1;
		}
	}

	// Copy routine name to portal for later execution in handle_execute
	YDB_MALLOC_BUFFER(&routine_buf, routine_len + 1);	// Null terminator
	status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &routine_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		ERROR(ERR_ROCTO_DB_LOOKUP, "handle_bind", "routine name of prepared statement");
		YDB_FREE_BUFFER(&routine_buf);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	routine_buf.buf_addr[routine_buf.len_used] = '\0';
	status = ydb_set_s(&portal_subs[0], 4, &portal_subs[1], &routine_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		YDB_FREE_BUFFER(&routine_buf);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}

	// Copy command tag to portal for later retrieval in handle_execute
	YDB_MALLOC_BUFFER(&statement_subs[4], MAX_TAG_LEN);
	YDB_COPY_STRING_TO_BUFFER("tag", &statement_subs[4], done);
	if (!done) {
		YDB_FREE_BUFFER(&statement_subs[4]);
		YDB_FREE_BUFFER(&routine_buf);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	YDB_MALLOC_BUFFER(&tag_buf, MAX_TAG_LEN);
	status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &tag_buf);
	YDB_FREE_BUFFER(&statement_subs[4]);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		ERROR(ERR_ROCTO_DB_LOOKUP, "handle_bind", "routine name of prepared statement");
		YDB_FREE_BUFFER(&tag_buf);
		YDB_FREE_BUFFER(&routine_buf);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	YDB_MALLOC_BUFFER(&portal_subs[4], MAX_TAG_LEN);
	YDB_COPY_STRING_TO_BUFFER("tag", &portal_subs[4], done);
	if (!done) {
		YDB_FREE_BUFFER(&portal_subs[4]);
		YDB_FREE_BUFFER(&routine_buf);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	status = ydb_set_s(&portal_subs[0], 4, &portal_subs[1], &tag_buf);
	YDB_FREE_BUFFER(&portal_subs[4]);
	YDB_FREE_BUFFER(&tag_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&routine_buf);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}

	// Need to update multiple subscripts, just free and remake as this will be less expensive and more concise
	free(statement_subs);
	free(portal_subs);
	// Create buffers to access prepared statement info: ^session(id, "prepared", <name>, ...)
	statement_subs = make_buffers(config->global_names.session, 6, session->session_id->buf_addr, "prepared", bind->source,
			"parameters", "all", "");
	// Create buffers to for storing portal ("bound statement") info: ^session(id, "bound", <name>, ...)
	portal_subs = make_buffers(config->global_names.session, 6, session->session_id->buf_addr, "bound", bind->dest,
			"parameters", "", "");
	// Retrieve the number of bind parameters on the prepared statement
	YDB_MALLOC_BUFFER(&num_parms_buf, INT16_TO_STRING_MAX);
	status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &num_parms_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&num_parms_buf);
		YDB_FREE_BUFFER(&routine_buf);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	num_parms_buf.buf_addr[num_parms_buf.len_used] = '\0';
	// Store this number on the portal for use in handle_describe
	status = ydb_set_s(&portal_subs[0], 4, &portal_subs[1], &num_parms_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&num_parms_buf);
		YDB_FREE_BUFFER(&routine_buf);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	// Convert number of bind parameters to an integer for use below
	num_parms_long = strtol(num_parms_buf.buf_addr, NULL, 10);
	if ((ERANGE != errno) && (0 <= num_parms_long) && (INT16_MAX >= num_parms_long)) {
		num_bind_parms = (int16_t)num_parms_long;
	} else {
		ERROR(ERR_LIBCALL, "strtol")
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&num_parms_buf);
		YDB_FREE_BUFFER(&routine_buf);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	if (num_bind_parms != bind->num_parms) {
		ERROR(ERR_ROCTO_INVALID_NUMBER_BIND_PARAMETERS, "handle_bind", num_bind_parms, bind->num_parms);
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&num_parms_buf);
		YDB_FREE_BUFFER(&routine_buf);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	// Retrieve the total number of parameters on the prepared statement (can discard old value after it's converted and stored)
	// Set the subscripts for all prepared statement parameters: session(id, "prepared", <name>, "parameters", "all", ...)
	all_statement_parms_subs = make_buffers(config->global_names.session, 6, session->session_id->buf_addr, "prepared", bind->source,
			"parameters", "all", "");
	status = ydb_get_s(&all_statement_parms_subs[0], 5, &all_statement_parms_subs[1], &num_parms_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&num_parms_buf);
		YDB_FREE_BUFFER(&routine_buf);
		free(all_statement_parms_subs);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	num_parms_buf.buf_addr[num_parms_buf.len_used] = '\0';
	// Store the total number of parameters for use in handle_execute
	// Set the subscripts for all prepared statement parameters: session(id, "prepared", <name>, "parameters", "all", ...)
	all_portal_parms_subs = make_buffers(config->global_names.session, 6, session->session_id->buf_addr, "bound", bind->dest,
			"parameters", "all", "");
	status = ydb_set_s(&all_portal_parms_subs[0], 5, &all_portal_parms_subs[1], &num_parms_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&num_parms_buf);
		YDB_FREE_BUFFER(&routine_buf);
		free(all_statement_parms_subs);
		free(all_portal_parms_subs);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	// Convert total number of parameters to an integer for use below
	num_parms_long = strtol(num_parms_buf.buf_addr, NULL, 10);
	if ((ERANGE != errno) && (0 <= num_parms_long) && (INT16_MAX >= num_parms_long)) {
		num_parms = (int16_t)num_parms_long;
	} else {
		ERROR(ERR_LIBCALL, "strtol")
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&num_parms_buf);
		YDB_FREE_BUFFER(&routine_buf);
		free(all_statement_parms_subs);
		free(all_portal_parms_subs);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}

	// Retrieve the prepared statement query string
	YDB_MALLOC_BUFFER(&sql_expression, MAX_STR_CONST);
	status = ydb_get_s(&statement_subs[0], 3, &statement_subs[1], &sql_expression);
	if(YDB_ERR_LVUNDEF == status) {
		ERROR(ERR_ROCTO_BIND_TO_UNKNOWN_QUERY, "");
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&sql_expression);
		YDB_FREE_BUFFER(&routine_buf);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		YDB_FREE_BUFFER(&sql_expression);
		YDB_FREE_BUFFER(&routine_buf);
		free(statement_subs);
		free(portal_subs);
		return 1;
	}
	sql_expression.buf_addr[sql_expression.len_used] = '\0';

	// This check covers two cases:
	//	1. This is a SELECT query with no parameters
	//	2. This is a SET or SHOW statement (and therefore has no physical plan or parameters)
	// Just copy the query as-is and use the routine from above
	if ((0 == num_parms) || (0 == strncmp(routine_buf.buf_addr, "none", MAX_ROUTINE_LEN))) {
		// Store the query for command tag generation in handle_execute
		status = ydb_set_s(&portal_subs[0], 3, &portal_subs[1], &sql_expression);
		YDB_FREE_BUFFER(&routine_buf);
		YDB_FREE_BUFFER(&sql_expression);
		free(all_statement_parms_subs);
		free(statement_subs);
		free(portal_subs);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
			YDB_ERROR_CHECK(status);
			YDB_FREE_BUFFER(&num_parms_buf);
			free(all_portal_parms_subs);
			return 1;
		}
		// Need to set the number of parameters to 0 for case 1.
		status = ydb_set_s(&all_portal_parms_subs[0], 5, &all_portal_parms_subs[1], &num_parms_buf);
		YDB_FREE_BUFFER(&num_parms_buf);
		free(all_portal_parms_subs);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
			YDB_ERROR_CHECK(status);
			return 1;
		}
		response = make_bind_complete();
		send_message(session, (BaseMessage*)response);
		free(response);
		return 0;
	}
	YDB_FREE_BUFFER(&num_parms_buf);
	YDB_FREE_BUFFER(&routine_buf);

	// Use arrays to track start/end offsets of parameter substrings in prepared statement
	// 2 * num_parms is the number of offsets that must be mapped: one pair of start/end offsets for each parameter
	if (0 < num_bind_parms) {
		parse_context_array = (int16_t*)malloc((sizeof(int16_t) * num_bind_parms) * 2);
		memset(parse_context_array, 0, (sizeof(int16_t) * num_bind_parms) * 2);
		memset(&parse_context, 0, sizeof(ParseContext));
		parse_context.parm_start = &parse_context_array[0];
		parse_context.parm_end = &parse_context_array[num_bind_parms];
		parse_context.types = (PSQL_TypeOid*)malloc(sizeof(PSQL_TypeOid) * num_bind_parms);
	} else {
		parse_context_array = NULL;
		parse_context.types = NULL;
	}
	// Retrieve ParseContext info from prepared statement local variable
	YDB_MALLOC_BUFFER(&parm_value_buf, MAX_STR_CONST);
	YDB_MALLOC_BUFFER(&parm_type_buf, INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&cur_bind_parm_buf, INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&cur_parm_buf, INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&offset_buffer, INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&all_statement_parms_subs[6], INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&all_portal_parms_subs[6], INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&statement_subs[5], INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&portal_subs[5], INT16_TO_STRING_MAX);
	YDB_MALLOC_BUFFER(&statement_subs[6], INT16_TO_STRING_MAX);
	for (cur_parm = 0, cur_bind_parm = 0; cur_parm < num_parms; cur_parm++) {
		cur_parm_temp = cur_parm + 1;			// Convert to 1-indexing for parameter number mapping
		OCTO_INT16_TO_BUFFER(cur_parm_temp, &cur_parm_buf);
		cur_bind_parm_temp = cur_bind_parm + 1;		// Convert to 1-indexing for parameter number mapping
		OCTO_INT16_TO_BUFFER(cur_bind_parm_temp, &cur_bind_parm_buf);
		// Set the current prepared statement parameter
		YDB_COPY_BUFFER_TO_BUFFER(&cur_bind_parm_buf, &statement_subs[5], done);
		if (!done) {
			ERROR(ERR_YOTTADB, "YDB_COPY_BUFFER_TO_BUFFER failed");
			FREE_HANDLE_BIND_POINTERS();
			return 1;
		}
		// Set the current overall parameter
		YDB_COPY_BUFFER_TO_BUFFER(&cur_parm_buf, &all_statement_parms_subs[6], done);
		if (!done) {
			ERROR(ERR_YOTTADB, "YDB_COPY_BUFFER_TO_BUFFER failed");
			FREE_HANDLE_BIND_POINTERS();
			return 1;
		}
		// Check if parameter has a value. If so, retrieve it; if not, it's a bind parameter
		status = ydb_data_s(&all_statement_parms_subs[0], 6, &all_statement_parms_subs[1], &data_ret);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			FREE_HANDLE_BIND_POINTERS();
			return 1;
		}
		if (1 == data_ret) {
			status = ydb_get_s(&all_statement_parms_subs[0], 6, &all_statement_parms_subs[1], &parm_value_buf);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				ERROR(ERR_YOTTADB, "YDB_COPY_BUFFER_TO_BUFFER failed");
				FREE_HANDLE_BIND_POINTERS();
				return 1;
			}
		} else if (cur_bind_parm < num_bind_parms) {
			assert(NULL != parse_context_array);
			// This is a bind parameter, as no value was stored during the initial parse
			// Retrieve parameter offsets from database
			YDB_COPY_STRING_TO_BUFFER("start", &statement_subs[6], done);
			if (!done) {
				ERROR(ERR_YOTTADB, "YDB_COPY_BUFFER_TO_BUFFER failed");
				FREE_HANDLE_BIND_POINTERS();
				return 1;
			}
			status = ydb_get_s(&statement_subs[0], 6, &statement_subs[1], &offset_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				FREE_HANDLE_BIND_POINTERS();
				return 1;
			}
			offset_buffer.buf_addr[offset_buffer.len_used] = '\0';
			offset_long = strtol(offset_buffer.buf_addr, NULL, 10);
			if ((ERANGE != errno) && (0 <= offset_long) && (INT16_MAX >= offset_long)) {
				parse_context.parm_start[cur_bind_parm] = (int16_t)offset_long;
			} else {
				ERROR(ERR_LIBCALL, "strtol")
				FREE_HANDLE_BIND_POINTERS();
				return 1;
			}
			YDB_COPY_STRING_TO_BUFFER("end", &statement_subs[6], done);
			if (!done) {
				ERROR(ERR_YOTTADB, "YDB_COPY_BUFFER_TO_BUFFER failed");
				FREE_HANDLE_BIND_POINTERS();
				return 1;
			}
			status = ydb_get_s(&statement_subs[0], 6, &statement_subs[1], &offset_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				ERROR(ERR_YOTTADB, "YDB_COPY_BUFFER_TO_BUFFER failed");
				FREE_HANDLE_BIND_POINTERS();
				return 1;
			}
			offset_buffer.buf_addr[offset_buffer.len_used] = '\0';
			offset_long = strtol(offset_buffer.buf_addr, NULL, 10);
			if ((ERANGE != errno) && (0 <= offset_long) && (INT16_MAX >= offset_long)) {
				parse_context.parm_end[cur_bind_parm] = (int16_t)offset_long;
			} else {
				ERROR(ERR_LIBCALL, "strtol")
				FREE_HANDLE_BIND_POINTERS();
				return 1;
			}
			// Retrieve parameter type from database
			YDB_COPY_STRING_TO_BUFFER("type", &statement_subs[6], done);
			if (!done) {
				ERROR(ERR_YOTTADB, "YDB_COPY_BUFFER_TO_BUFFER failed");
				FREE_HANDLE_BIND_POINTERS();
				return 1;
			}
			status = ydb_get_s(&statement_subs[0], 6, &statement_subs[1], &parm_type_buf);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				FREE_HANDLE_BIND_POINTERS();
				return 1;
			}
			parm_type_buf.buf_addr[parm_type_buf.len_used] = '\0';
			type_long = strtol(parm_type_buf.buf_addr, NULL, 10);
			if ((ERANGE != errno) && (0 <= type_long) && (INT16_MAX >= type_long)) {
				parse_context.types[cur_bind_parm] = (int16_t)type_long;
			} else {
				ERROR(ERR_LIBCALL, "strtol")
				FREE_HANDLE_BIND_POINTERS();
				return 1;
			}
			// Get bind parameter value if it is a binary parameter and update the parm_value_buf accordingly
			// The logic here enumerates the possible combinations of bind parameter formats that may be specified
			// by clients in a PostgreSQL Bind message. These combinations follow from the definition of a Bind message
			// at https://www.postgresql.org/docs/11/protocol-message-formats.html.
			if (1 < bind->num_parm_format_codes) {
				if (1 == bind->parm_format_codes[cur_parm]) {			// Binary
					copy_binary_parameter(bind, cur_bind_parm, binary_parm_buffer, 0);
					YDB_COPY_STRING_TO_BUFFER(binary_parm_buffer, &parm_value_buf, done);
					if (!done) {
						ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
						FREE_HANDLE_BIND_POINTERS();
						return 1;
					}
				} else {							// Text
					YDB_COPY_STRING_TO_BUFFER(bind->parms[cur_bind_parm].value, &parm_value_buf, done);
					if (!done) {
						ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
						FREE_HANDLE_BIND_POINTERS();
						return 1;
					}
				}
			} else if (1 == bind->num_parm_format_codes) {
				if (1 == bind->parm_format_codes[FIRST_FORMAT_CODE]) {		// Binary
					copy_binary_parameter(bind, cur_bind_parm, binary_parm_buffer, 0);
					YDB_COPY_STRING_TO_BUFFER(binary_parm_buffer, &parm_value_buf, done);
					if (!done) {
						ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
						FREE_HANDLE_BIND_POINTERS();
						return 1;
					}
				} else {							// Text
					YDB_COPY_STRING_TO_BUFFER(bind->parms[cur_bind_parm].value, &parm_value_buf, done);
					if (!done) {
						ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
						FREE_HANDLE_BIND_POINTERS();
						return 1;
					}
				}
			} else {								// Text
				YDB_COPY_STRING_TO_BUFFER(bind->parms[cur_bind_parm].value, &parm_value_buf, done);
				if (!done) {
					ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
					FREE_HANDLE_BIND_POINTERS();
					return 1;
				}
			}
			cur_bind_parm++;
		}
		// Store the parameter value on the portal, whether bind parameter or literal from previous parse
		YDB_COPY_BUFFER_TO_BUFFER(&cur_parm_buf, &all_portal_parms_subs[6], done);
		if (!done) {
			ERROR(ERR_YOTTADB, "YDB_COPY_BUFFER_TO_BUFFER failed");
			FREE_HANDLE_BIND_POINTERS();
			return 1;
		}
		status = ydb_set_s(&all_portal_parms_subs[0], 6, &all_portal_parms_subs[1], &parm_value_buf);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			FREE_HANDLE_BIND_POINTERS();
			return 1;
		}

	}
	YDB_FREE_BUFFER(&parm_value_buf);
	YDB_FREE_BUFFER(&offset_buffer);
	YDB_FREE_BUFFER(&parm_type_buf);
	YDB_FREE_BUFFER(&parm_type_buf);
	YDB_FREE_BUFFER(&cur_bind_parm_buf);
	YDB_FREE_BUFFER(&cur_parm_buf);
	YDB_FREE_BUFFER(&all_statement_parms_subs[6]);
	YDB_FREE_BUFFER(&all_portal_parms_subs[6]);
	YDB_FREE_BUFFER(&portal_subs[5]);
	YDB_FREE_BUFFER(&statement_subs[5]);
	YDB_FREE_BUFFER(&statement_subs[6]);
	free(all_statement_parms_subs);
	free(all_portal_parms_subs);

	// Get size of final query string
	// Bind format rules at https://www.postgresql.org/docs/11/protocol-message-formats.html
	if (0 < num_bind_parms) {
		bound_query_size = sql_expression.len_used + 1;			// null terminator
		for (cur_bind_parm = 0; cur_bind_parm < num_bind_parms; cur_bind_parm++) {
			// Binary types will be converted to strings of various sizes, use converted sizes instead of listed length
			if (1 < bind->num_parm_format_codes) {
				if (1 == bind->parm_format_codes[cur_bind_parm]) {
					bound_query_size += get_binary_parameter_length(bind, cur_bind_parm);
				} else {
					bound_query_size += bind->parms[cur_bind_parm].length;
				}
			} else if (1 == bind->num_parm_format_codes) {
				if (1 == bind->parm_format_codes[0]) {
					bound_query_size += get_binary_parameter_length(bind, cur_bind_parm);
				} else {
					bound_query_size += bind->parms[cur_bind_parm].length;
				}
			} else {
				bound_query_size += bind->parms[cur_bind_parm].length;
			}
			if (PSQL_TypeOid_varchar == parse_context.types[cur_bind_parm]) {
				bound_query_size += 2;	// Add 2 for opening and closing single quotes on string parameters
			}
			// Don't count the dollar sign and parameter number as these will be replaced by a concrete parameter value
			bound_query_size -= (parse_context.parm_end[cur_bind_parm] - parse_context.parm_start[cur_bind_parm]);
		}
		bound_query = (char*)calloc(bound_query_size, sizeof(char));
		// Copy Bind parameters into bound query string
		// Store ALL parameters on portal for retrieval by handle_execute, both bind parameters and literals
		// Start with substring leading up to first parameter
		bound_offset = prepared_offset = 0;
		memcpy(&bound_query[bound_offset], &sql_expression.buf_addr[prepared_offset], parse_context.parm_start[0]);
		bound_offset += parse_context.parm_start[0];
		prepared_offset += parse_context.parm_start[0];
		for (cur_bind_parm = 0; cur_bind_parm < num_bind_parms; cur_bind_parm++) {
			// Insert quotes if parameter is a string
			if (PSQL_TypeOid_varchar == parse_context.types[cur_bind_parm]) {
				bound_query[bound_offset] = '\'';
				bound_offset++;
			}
			// Copy parameter value
			if (1 < bind->num_parm_format_codes) {
				if (0 == bind->parm_format_codes[cur_bind_parm]) {
					bound_offset = copy_text_parameter(bind, cur_bind_parm, bound_query, bound_offset);
				} else {
					// Binary
					bound_offset = copy_binary_parameter(bind, cur_bind_parm, bound_query, bound_offset);
				}
			} else if (0 == bind->num_parm_format_codes) {
				// All parameters are in text format
				bound_offset = copy_text_parameter(bind, cur_bind_parm, bound_query, bound_offset);
			} else if (1 == bind->num_parm_format_codes) {
				if (0 == bind->parm_format_codes[FIRST_FORMAT_CODE]) {
					bound_offset = copy_text_parameter(bind, cur_bind_parm, bound_query, bound_offset);
				} else {
					// Binary
					bound_offset = copy_binary_parameter(bind, cur_bind_parm, bound_query, bound_offset);
				}
			}
			// Insert quotes if parameter is a string
			if (PSQL_TypeOid_varchar == parse_context.types[cur_bind_parm]) {
				bound_query[bound_offset] = '\'';
				bound_offset++;
			}
			prepared_offset += parse_context.parm_end[cur_bind_parm] - parse_context.parm_start[cur_bind_parm];

			// Copy portion of query string between two parameters
			if ((cur_bind_parm + 1) == num_bind_parms) {
				memcpy(&bound_query[bound_offset], &sql_expression.buf_addr[prepared_offset],
						sql_expression.len_used - parse_context.parm_end[cur_bind_parm]);
				bound_offset += sql_expression.len_used - parse_context.parm_end[cur_bind_parm];
			} else {
				memcpy(&bound_query[bound_offset], &sql_expression.buf_addr[prepared_offset],
						parse_context.parm_start[cur_bind_parm+1] - parse_context.parm_end[cur_bind_parm]);
				prepared_offset += parse_context.parm_start[cur_bind_parm+1] - parse_context.parm_end[cur_bind_parm];
				prepared_offset += parse_context.parm_start[cur_bind_parm+1] - parse_context.parm_end[cur_bind_parm];
				bound_offset += parse_context.parm_start[cur_bind_parm+1] - parse_context.parm_end[cur_bind_parm];
			}
		}
		bound_query[bound_offset] = '\0';
		free(parse_context_array);
		free(parse_context.types);

		// Store the bound statement in a global for later reference, if needed
		YDB_FREE_BUFFER(&sql_expression);

		sql_expression.buf_addr = bound_query;
		sql_expression.len_alloc = bound_query_size;
		sql_expression.len_used = bound_query_size-1;	// exclude null terminator
	}

	status = ydb_set_s(&portal_subs[0], 3, &portal_subs[1], &sql_expression);
	if (0 == num_bind_parms) {
		YDB_FREE_BUFFER(&sql_expression);	// This buffer is freed above when there are bind parameters
	} else {
		free(bound_query);			// This pointer is not allocated when there are no bind parameters
	}
	free(statement_subs);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		free(portal_subs);
		return 1;
	}
	free(portal_subs);

	// Construct the response message
	response = make_bind_complete();
	send_message(session, (BaseMessage*)response);
	free(response);

	return 0;
}
