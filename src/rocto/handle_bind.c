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
// Details linked in message_formats.h
#define FIRST_FORMAT_CODE 0

#define INVOKE_YDB_DELETE_S()								\
	status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);	\
	YDB_ERROR_CHECK(status);							\

// Args:
//	Bind *bind: A PostgreSQL Bind message
//	RoctoSession *session: Structu containing data for the current client session
// Returns:
//	0 for success, 1 for error
int handle_bind(Bind *bind, RoctoSession *session) {
	// At the moment, we don't have "bound function"
	// This feature should be implemented before 1.0
	ydb_buffer_t	num_parms_buf, cur_parm_buf, cur_bind_parm_buf, parm_type_buf, parm_value_buf;
	ydb_buffer_t	sql_expression, routine_buf, tag_buf, offset_buffer, value_buffer;
	ydb_buffer_t	statement_subs[7];
	ydb_buffer_t	portal_subs[6];
	ydb_buffer_t	all_statement_parms_subs[7];
	ydb_buffer_t	all_portal_parms_subs[7];
	char		value_str[INT16_TO_STRING_MAX];
	char		sql_expression_str[MAX_STR_CONST];
	char		num_parms_str[INT16_TO_STRING_MAX];
	char		tag_str[MAX_TAG_LEN];
	char		routine_str[MAX_ROUTINE_LEN + 1];	// Null terminator
	char		parm_value_str[MAX_STR_CONST];
	char		parm_type_str[INT16_TO_STRING_MAX];
	char		offset_str[INT16_TO_STRING_MAX];
	char		cur_format_str[INT16_TO_STRING_MAX];
	char		cur_parm_str[INT16_TO_STRING_MAX];
	char		cur_bind_parm_str[INT16_TO_STRING_MAX];
	char		binary_parm_buffer[MAX_STR_CONST];
	char		*bound_query;
	uint32_t	data_ret;
	int32_t		status, bound_offset, prepared_offset, done;
	int16_t		*parse_context_array;
	int16_t		num_parms, num_bind_parms, cur_parm, cur_bind_parm, cur_parm_temp, cur_bind_parm_temp;
	int16_t		num_col_format_codes, cur_format_code;
	long int	num_parms_long, offset_long, type_long;
	size_t		bound_query_size;
	BindComplete	*response;
	ParseContext	parse_context;

	TRACE(ERR_ENTERING_FUNCTION, "handle_bind");

	// Create buffers to get routine name from prepared statement: ^session(id, "prepared", <name>, "routine")
	YDB_STRING_TO_BUFFER(config->global_names.session, &statement_subs[0]);
	YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &statement_subs[1]);
	YDB_STRING_TO_BUFFER("prepared", &statement_subs[2]);
	YDB_STRING_TO_BUFFER(bind->source, &statement_subs[3]);
	YDB_STRING_TO_BUFFER("routine", &statement_subs[4]);

	// Create buffers to store routine name on portal: ^session(id, "bound", <name>, "routine")
	YDB_STRING_TO_BUFFER(config->global_names.session, &portal_subs[0]);
	YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &portal_subs[1]);
	YDB_STRING_TO_BUFFER("bound", &portal_subs[2]);
	YDB_STRING_TO_BUFFER(bind->dest, &portal_subs[3]);
	YDB_STRING_TO_BUFFER("routine", &portal_subs[4]);

	// Check if a portal by the same name already exists and, if so, delete it before reusing the name for a new one
	status = ydb_data_s(&portal_subs[0], 3, &portal_subs[1], &data_ret);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	if (0 < data_ret) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			return 1;
		}
	}

	// Copy routine name to portal for later execution in handle_execute
	OCTO_SET_BUFFER(routine_buf, routine_str);
	status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &routine_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		ERROR(ERR_ROCTO_DB_LOOKUP, "handle_bind", "routine name of prepared statement");
		return 1;
	}
	routine_buf.buf_addr[routine_buf.len_used] = '\0';
	status = ydb_set_s(&portal_subs[0], 4, &portal_subs[1], &routine_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}

	// Copy command tag to portal for later retrieval in handle_execute
	OCTO_SET_BUFFER(tag_buf, tag_str);
	YDB_STRING_TO_BUFFER("tag", &statement_subs[4]);
	status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &tag_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		ERROR(ERR_ROCTO_DB_LOOKUP, "handle_bind", "routine name of prepared statement");
		return 1;
	}
	YDB_STRING_TO_BUFFER("tag", &portal_subs[4]);
	status = ydb_set_s(&portal_subs[0], 4, &portal_subs[1], &tag_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		return 1;
	}

	// Reassign buffers to access prepared statement info: ^session(id, "prepared", <name>, ...)
	YDB_STRING_TO_BUFFER("parameters", &statement_subs[4]);
	YDB_STRING_TO_BUFFER("all", &statement_subs[5]);

	// Reassign buffer for storing portal ("bound statement") info: ^session(id, "bound", <name>, ...)
	YDB_STRING_TO_BUFFER("parameters", &portal_subs[4]);

	// Retrieve the number of bind parameters on the prepared statement
	OCTO_SET_BUFFER(num_parms_buf, num_parms_str);
	status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &num_parms_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		return 1;
	}
	num_parms_buf.buf_addr[num_parms_buf.len_used] = '\0';
	// Store this number on the portal for use in handle_describe
	status = ydb_set_s(&portal_subs[0], 4, &portal_subs[1], &num_parms_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		return 1;
	}
	// Convert number of bind parameters to an integer for use below
	num_parms_long = strtol(num_parms_buf.buf_addr, NULL, 10);
	if ((ERANGE != errno) && (0 <= num_parms_long) && (INT16_MAX >= num_parms_long)) {
		num_bind_parms = (int16_t)num_parms_long;
	} else {
		ERROR(ERR_LIBCALL, "strtol");
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		return 1;
	}
	if (num_bind_parms != bind->num_parms) {
		ERROR(ERR_ROCTO_INVALID_NUMBER_BIND_PARAMETERS, "handle_bind", num_bind_parms, bind->num_parms);
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		return 1;
	}

	// Retrieve the total number of parameters on the prepared statement (can discard old value after it's converted and stored)
	// Set the subscripts for all prepared statement parameters: session(id, "prepared", <name>, "parameters", "all", ...)
	YDB_STRING_TO_BUFFER(config->global_names.session, &all_statement_parms_subs[0]);
	YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &all_statement_parms_subs[1]);
	YDB_STRING_TO_BUFFER("prepared", &all_statement_parms_subs[2]);
	YDB_STRING_TO_BUFFER(bind->source, &all_statement_parms_subs[3]);
	YDB_STRING_TO_BUFFER("parameters", &all_statement_parms_subs[4]);
	YDB_STRING_TO_BUFFER("all", &all_statement_parms_subs[5]);
	status = ydb_get_s(&all_statement_parms_subs[0], 5, &all_statement_parms_subs[1], &num_parms_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		return 1;
	}
	num_parms_buf.buf_addr[num_parms_buf.len_used] = '\0';

	// Store the total number of parameters for use in handle_execute
	// Set the subscripts for all prepared statement parameters: session(id, "prepared", <name>, "parameters", "all", ...)
	YDB_STRING_TO_BUFFER(config->global_names.session, &all_portal_parms_subs[0]);
	YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &all_portal_parms_subs[1]);
	YDB_STRING_TO_BUFFER("bound", &all_portal_parms_subs[2]);
	YDB_STRING_TO_BUFFER(bind->dest, &all_portal_parms_subs[3]);
	YDB_STRING_TO_BUFFER("parameters", &all_portal_parms_subs[4]);
	YDB_STRING_TO_BUFFER("all", &all_portal_parms_subs[5]);
	status = ydb_set_s(&all_portal_parms_subs[0], 5, &all_portal_parms_subs[1], &num_parms_buf);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
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
		return 1;
	}

	// Retrieve the prepared statement query string
	OCTO_SET_BUFFER(sql_expression, sql_expression_str);
	status = ydb_get_s(&statement_subs[0], 3, &statement_subs[1], &sql_expression);
	if(YDB_ERR_LVUNDEF == status) {
		ERROR(ERR_ROCTO_BIND_TO_UNKNOWN_QUERY, "");
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		return 1;
	}
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		return 1;
	}
	sql_expression.buf_addr[sql_expression.len_used] = '\0';

	// Revise portal buffers to set column format information
	YDB_STRING_TO_BUFFER("col_formats", &portal_subs[4]);
	// Store number of column format codes: 0, 1, or > 1
	num_col_format_codes = bind->num_result_col_format_codes;
	OCTO_SET_BUFFER(value_buffer, value_str);
	OCTO_INT16_TO_BUFFER(num_col_format_codes, &value_buffer);
	status = ydb_set_s(&portal_subs[0], 4, &portal_subs[1], &value_buffer);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	// Store the format code for all columns:
	//	Number of codes == 0: All are text format
	//	Number of codes == 1: All are text or all are binary format, depending on what code is specified
	//	Number of codes > 1:  Column formats may be a combination of text and binary, specified per column
	OCTO_SET_BUFFER(portal_subs[5], cur_format_str);
	if (1 >= num_col_format_codes) {
		// Store the sole format code under the first column
		OCTO_INT16_TO_BUFFER((int16_t)1, &portal_subs[5]);
		if (0 == num_col_format_codes) {
			// Use the default code (0) to specify text format
			OCTO_INT16_TO_BUFFER((int16_t)TEXT_FORMAT, &value_buffer);
		} else {
			// Use the sole specified format code
			OCTO_INT16_TO_BUFFER(bind->result_col_format_codes[0], &value_buffer);
		}
		status = ydb_set_s(&portal_subs[0], 5, &portal_subs[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			return 1;
		}
	} else {
		// Store each specified format code under its matching column.
		// Columns are indexed from 1, so start there instead of 0.
		for (cur_format_code = 1; cur_format_code <= num_col_format_codes; cur_format_code++) {
			OCTO_INT16_TO_BUFFER(cur_format_code, &portal_subs[5]);
			OCTO_INT16_TO_BUFFER(bind->result_col_format_codes[cur_format_code-1], &value_buffer);	// 0-indexing here
			status = ydb_set_s(&portal_subs[0], 5, &portal_subs[1], &value_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				return 1;
			}
		}
	}

	// This check covers two cases:
	//	1. This is a SELECT query with no parameters
	//	2. This is a SET or SHOW statement (and therefore has no physical plan or parameters)
	// Just copy the query as-is and use the routine from above
	if ((0 == num_parms) || (0 == strncmp(routine_buf.buf_addr, "none", MAX_ROUTINE_LEN))) {
		// Store the query for command tag generation in handle_execute
		status = ydb_set_s(&portal_subs[0], 3, &portal_subs[1], &sql_expression);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
			YDB_ERROR_CHECK(status);
			return 1;
		}
		// Need to set the number of parameters to 0 for case 1.
		status = ydb_set_s(&all_portal_parms_subs[0], 5, &all_portal_parms_subs[1], &num_parms_buf);
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

	OCTO_SET_BUFFER(cur_parm_buf, cur_parm_str);
	OCTO_SET_BUFFER(cur_bind_parm_buf, cur_bind_parm_str);
	OCTO_SET_BUFFER(parm_value_buf, parm_value_str);
	OCTO_SET_BUFFER(parm_type_buf, parm_type_str);
	OCTO_SET_BUFFER(offset_buffer, offset_str);
	for (cur_parm = 0, cur_bind_parm = 0; cur_parm < num_parms; cur_parm++) {
		cur_parm_temp = cur_parm + 1;			// Convert to 1-indexing for parameter number mapping
		OCTO_INT16_TO_BUFFER(cur_parm_temp, &cur_parm_buf);
		cur_bind_parm_temp = cur_bind_parm + 1;		// Convert to 1-indexing for parameter number mapping
		OCTO_INT16_TO_BUFFER(cur_bind_parm_temp, &cur_bind_parm_buf);
		// Set the current prepared statement parameter
		YDB_STRING_TO_BUFFER(cur_bind_parm_buf.buf_addr, &statement_subs[5]);
		// Set the current overall parameter
		YDB_STRING_TO_BUFFER(cur_parm_buf.buf_addr, &all_statement_parms_subs[6]);
		// Check if parameter has a value. If so, retrieve it; if not, it's a bind parameter
		status = ydb_data_s(&all_statement_parms_subs[0], 6, &all_statement_parms_subs[1], &data_ret);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			INVOKE_YDB_DELETE_S();
			return 1;
		}
		if (1 == data_ret) {
			status = ydb_get_s(&all_statement_parms_subs[0], 6, &all_statement_parms_subs[1], &parm_value_buf);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				INVOKE_YDB_DELETE_S();
				return 1;
			}
		} else if (cur_bind_parm < num_bind_parms) {
			assert(NULL != parse_context_array);
			// This is a bind parameter, as no value was stored during the initial parse
			// Retrieve parameter offsets from database
			YDB_STRING_TO_BUFFER("start", &statement_subs[6]);
			status = ydb_get_s(&statement_subs[0], 6, &statement_subs[1], &offset_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				INVOKE_YDB_DELETE_S();
				return 1;
			}
			offset_buffer.buf_addr[offset_buffer.len_used] = '\0';
			offset_long = strtol(offset_buffer.buf_addr, NULL, 10);
			if ((ERANGE != errno) && (0 <= offset_long) && (INT16_MAX >= offset_long)) {
				parse_context.parm_start[cur_bind_parm] = (int16_t)offset_long;
			} else {
				ERROR(ERR_LIBCALL, "strtol")
				INVOKE_YDB_DELETE_S();
				return 1;
			}
			YDB_STRING_TO_BUFFER("end", &statement_subs[6]);
			status = ydb_get_s(&statement_subs[0], 6, &statement_subs[1], &offset_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				INVOKE_YDB_DELETE_S();
				return 1;
			}
			offset_buffer.buf_addr[offset_buffer.len_used] = '\0';
			offset_long = strtol(offset_buffer.buf_addr, NULL, 10);
			if ((ERANGE != errno) && (0 <= offset_long) && (INT16_MAX >= offset_long)) {
				parse_context.parm_end[cur_bind_parm] = (int16_t)offset_long;
			} else {
				ERROR(ERR_LIBCALL, "strtol")
				INVOKE_YDB_DELETE_S();
				return 1;
			}
			// Retrieve parameter type from database
			YDB_STRING_TO_BUFFER("type", &statement_subs[6]);
			status = ydb_get_s(&statement_subs[0], 6, &statement_subs[1], &parm_type_buf);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				INVOKE_YDB_DELETE_S();
				return 1;
			}
			parm_type_buf.buf_addr[parm_type_buf.len_used] = '\0';
			type_long = strtol(parm_type_buf.buf_addr, NULL, 10);
			if ((ERANGE != errno) && (0 <= type_long) && (INT16_MAX >= type_long)) {
				parse_context.types[cur_bind_parm] = (int16_t)type_long;
			} else {
				ERROR(ERR_LIBCALL, "strtol")
				INVOKE_YDB_DELETE_S();
				return 1;
			}
			// Get bind parameter value if it is a binary parameter and update the parm_value_buf accordingly
			// The logic here enumerates the possible combinations of bind parameter formats that may be specified
			// by clients in a PostgreSQL Bind message. These combinations follow from the definition of a Bind message
			// at https://www.postgresql.org/docs/11/protocol-message-formats.html.
			if (1 < bind->num_parm_format_codes) {
				if (BINARY_FORMAT == bind->parm_format_codes[cur_parm]) {			// Binary
					copy_binary_parameter(bind, cur_bind_parm, binary_parm_buffer, 0);
					YDB_COPY_STRING_TO_BUFFER(binary_parm_buffer, &parm_value_buf, done);
					if (!done) {
						ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
						INVOKE_YDB_DELETE_S();
						return 1;
					}
				} else {							// Text
					YDB_COPY_STRING_TO_BUFFER(bind->parms[cur_bind_parm].value, &parm_value_buf, done);
					if (!done) {
						ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
						INVOKE_YDB_DELETE_S();
						return 1;
					}
				}
			} else if (1 == bind->num_parm_format_codes) {
				if (BINARY_FORMAT == bind->parm_format_codes[FIRST_FORMAT_CODE]) {		// Binary
					copy_binary_parameter(bind, cur_bind_parm, binary_parm_buffer, 0);
					YDB_COPY_STRING_TO_BUFFER(binary_parm_buffer, &parm_value_buf, done);
					if (!done) {
						ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
						INVOKE_YDB_DELETE_S();
						return 1;
					}
				} else {							// Text
					YDB_COPY_STRING_TO_BUFFER(bind->parms[cur_bind_parm].value, &parm_value_buf, done);
					if (!done) {
						ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
						INVOKE_YDB_DELETE_S();
						return 1;
					}
				}
			} else {								// Text
				YDB_COPY_STRING_TO_BUFFER(bind->parms[cur_bind_parm].value, &parm_value_buf, done);
				if (!done) {
					ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
					INVOKE_YDB_DELETE_S();
					return 1;
				}
			}
			cur_bind_parm++;
		}
		// Store the parameter value on the portal, whether bind parameter or literal from previous parse
		YDB_STRING_TO_BUFFER(cur_parm_buf.buf_addr, &all_portal_parms_subs[6]);
		status = ydb_set_s(&all_portal_parms_subs[0], 6, &all_portal_parms_subs[1], &parm_value_buf);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			INVOKE_YDB_DELETE_S();
			return 1;
		}

	}

	// Get size of final query string
	// Bind format rules at https://www.postgresql.org/docs/11/protocol-message-formats.html
	if (0 < num_bind_parms) {
		bound_query_size = sql_expression.len_used + 1;			// null terminator
		for (cur_bind_parm = 0; cur_bind_parm < num_bind_parms; cur_bind_parm++) {
			// Binary types will be converted to strings of various sizes, use converted sizes instead of listed length
			if (1 < bind->num_parm_format_codes) {
				if (BINARY_FORMAT == bind->parm_format_codes[cur_bind_parm]) {
					bound_query_size += get_binary_parameter_length(bind, cur_bind_parm);
				} else {
					bound_query_size += bind->parms[cur_bind_parm].length;
				}
			} else if (1 == bind->num_parm_format_codes) {
				if (BINARY_FORMAT == bind->parm_format_codes[0]) {
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
				if (TEXT_FORMAT == bind->parm_format_codes[cur_bind_parm]) {
					bound_offset = copy_text_parameter(bind, cur_bind_parm, bound_query, bound_offset);
				} else {
					// Binary
					bound_offset = copy_binary_parameter(bind, cur_bind_parm, bound_query, bound_offset);
				}
			} else if (0 == bind->num_parm_format_codes) {
				// All parameters are in text format
				bound_offset = copy_text_parameter(bind, cur_bind_parm, bound_query, bound_offset);
			} else if (1 == bind->num_parm_format_codes) {
				if (TEXT_FORMAT == bind->parm_format_codes[FIRST_FORMAT_CODE]) {
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
		sql_expression.buf_addr = bound_query;
		sql_expression.len_alloc = bound_query_size;
		sql_expression.len_used = bound_query_size-1;	// exclude null terminator
	}

	status = ydb_set_s(&portal_subs[0], 3, &portal_subs[1], &sql_expression);
	if (0 != num_bind_parms) {
		free(bound_query);			// This pointer is not allocated when there are no bind parameters
	}
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		status = ydb_delete_s(&portal_subs[0], 3, &portal_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		return 1;
	}

	// Construct the response message
	response = make_bind_complete();
	send_message(session, (BaseMessage*)response);
	free(response);

	return 0;
}
