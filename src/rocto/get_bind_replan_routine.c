/****************************************************************
 *								*
 * Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	*
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
#include "rocto.h"

/* Free everything "get_bind_replan_routine()" allocated */
#define CLEANUP_BIND_REPLAN_ROUTINE()             \
	{                                         \
		free(parse_context.types);        \
		free(parse_context.is_bind_parm); \
		free(bind_parm_values);           \
		free(parse_context_array);        \
		YDB_FREE_BUFFER(&sql_expression); \
	}

/* Called by "handle_bind()" once the values of the Bind message are known (YDBOcto#1158).
 *
 * A query whose plan depends on the values of bind parameters (see "plan_depends_on_bind_values" in "ParseContext") was
 * parsed for the Parse message without knowing those values, and got the plan that is right for any value.
 * "store_bind_replan_info()" stored the data types of the Parse message for such a query. Here, the query is parsed again
 * exactly as "handle_parse()" did, but with the bound values in "parse_context.bind_parm_values", so each optimization
 * that depends on a value can pick the plan for it. Any future such optimization works the same way.
 *
 * Currently the only such optimization is a LIKE or ILIKE whose pattern is a bind parameter (e.g. "col LIKE $1").
 * "get_like_prefix_operation()" classifies the value of $1 exactly as it does a literal pattern:
 *	'PAT'		 : "=", like a literal 'PAT' that "regex_has_no_special_characters()" converts
 *	'PAT%'		 : walk the cross reference over 'PAT', no LIKE check
 *	'PAT%ENT'	 : walk the cross reference over 'PAT', check the LIKE
 *	'%PAT', '', NULL : table scan, no cross reference built
 * For an ILIKE pattern the prefix ends at the first letter ('12%' walks over '12', 'pat%' is a table scan).
 * The bound value is not part of the plan hash, so all values of the same shape share one plan: the parse generates it
 * once and finds it on later Bind messages.
 *
 * Returns
 *	0 if the plan does not depend on the bound values ("routine_buf" is untouched; the plan from the Parse message is used),
 *	1 if "routine_buf" was set to the plan for the values of this Bind message,
 *	-1 on error.
 */
int32_t get_bind_replan_routine(Bind *bind, RoctoSession *session, ydb_buffer_t *routine_buf) {
	QueryResponseParms response_parms;
	ParseContext	   parse_context;
	BindParmValue	  *bind_parm_values;
	ydb_buffer_t	   statement_subs[6], cursor_subs[2], sql_expression, value_buffer;
	char		   parm_num_str[INT16_TO_STRING_MAX], value_str[INT32_TO_STRING_MAX];
	char		   cursor_str[INT64_TO_STRING_MAX];
	int32_t		   status, *parse_context_array, cur_type;
	int16_t		   cur_parm;
	unsigned int	   data_ret;

	YDB_STRING_TO_BUFFER(config->global_names.session, &statement_subs[0]);
	YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &statement_subs[1]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PREPARED, &statement_subs[2]);
	YDB_STRING_TO_BUFFER(bind->source, &statement_subs[3]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PARSETYPES, &statement_subs[4]);
	// $DATA(%ydboctosession(id,"prepared",<name>,"parsetypes")) (see "store_bind_replan_info()")
	status = ydb_data_s(&statement_subs[0], 4, &statement_subs[1], &data_ret);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return -1;
	}
	if (0 == data_ret) {
		/* The plan does not depend on the bound values. The plan from the Parse message is right for any value. */
		return 0;
	}
	/* The query has at least one bind parameter (whose value the plan depends on), and "handle_bind()" already checked
	 * that the Bind message has one value per bind parameter.
	 */
	assert(0 < bind->num_parms);

	memset(&parse_context, 0, sizeof(ParseContext));
	parse_context_array = (int32_t *)calloc((INT16_MAX * 2), sizeof(int32_t));
	YDB_MALLOC_BUFFER(&sql_expression, OCTO_INIT_BUFFER_LEN);

	/* The bound values, for the optimizations that depend on them (e.g. "get_like_prefix_operation()"). A binary format
	 * value is converted to text later ("copy_binary_parameter()"), so its bytes here may not be the text of the value:
	 * leave it NULL like an SQL NULL.
	 */
	bind_parm_values = (BindParmValue *)calloc(bind->num_parms, sizeof(BindParmValue));
	for (cur_parm = 0; cur_parm < bind->num_parms; cur_parm++) {
		BindParm *parm;
		int16_t	  format_code;

		parm = &bind->parms[cur_parm];
		if (0 == bind->num_parm_format_codes) {
			format_code = TEXT_FORMAT;
		} else if (1 == bind->num_parm_format_codes) {
			format_code = bind->parm_format_codes[0];
		} else {
			format_code = bind->parm_format_codes[cur_parm];
		}
		if ((PSQL_NULL != parm->length) && (TEXT_FORMAT == format_code)) {
			bind_parm_values[cur_parm].value = (const char *)parm->value;
			bind_parm_values[cur_parm].length = parm->length;
		}
	}

	// The query of the Parse message: SET sql_expression=%ydboctosession(id,"prepared",<name>)
	status = ydb_get_s(&statement_subs[0], 3, &statement_subs[1], &sql_expression);
	if (YDB_ERR_INVSTRLEN == status) {
		EXPAND_YDB_BUFFER_T_ALLOCATION(sql_expression);
		status = ydb_get_s(&statement_subs[0], 3, &statement_subs[1], &sql_expression);
		assert(YDB_ERR_INVSTRLEN != status);
	}
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		CLEANUP_BIND_REPLAN_ROUTINE();
		return -1;
	}
	COPY_QUERY_TO_INPUT_BUFFER(sql_expression.buf_addr, (int)sql_expression.len_used, NEWLINE_NEEDED_FALSE);

	/* The data types of the Parse message (see "store_bind_replan_info()"):
	 *	SET num=%ydboctosession(id,"prepared",<name>,"parsetypes")
	 *	FOR i=1:1:num SET types(i)=%ydboctosession(id,"prepared",<name>,"parsetypes",i)
	 */
	OCTO_SET_BUFFER(value_buffer, value_str);
	status = ydb_get_s(&statement_subs[0], 4, &statement_subs[1], &value_buffer);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		CLEANUP_BIND_REPLAN_ROUTINE();
		return -1;
	}
	value_buffer.buf_addr[value_buffer.len_used] = '\0';
	parse_context.num_bind_parm_types = (int16_t)strtol(value_buffer.buf_addr, NULL, 10);
	parse_context.types_size = ((0 < parse_context.num_bind_parm_types) ? parse_context.num_bind_parm_types : 8);
	parse_context.types = (PSQL_TypeOid *)calloc(parse_context.types_size, sizeof(PSQL_TypeOid));
	OCTO_SET_BUFFER(statement_subs[5], parm_num_str);
	for (cur_type = 0; cur_type < parse_context.num_bind_parm_types; cur_type++) {
		OCTO_INT16_TO_BUFFER((int16_t)(cur_type + 1), &statement_subs[5]);
		status = ydb_get_s(&statement_subs[0], 5, &statement_subs[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			CLEANUP_BIND_REPLAN_ROUTINE();
			return -1;
		}
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		parse_context.types[cur_type] = (PSQL_TypeOid)strtol(value_buffer.buf_addr, NULL, 10);
	}

	// Parse the query again, as "handle_parse()" did, but with the bound values
	parse_context.is_extended_query = TRUE;
	parse_context.parm_start = &parse_context_array[0];
	parse_context.parm_end = &parse_context_array[INT16_MAX];
	parse_context.is_bind_parm_size = 8;
	parse_context.is_bind_parm = (boolean_t *)calloc(parse_context.is_bind_parm_size, sizeof(boolean_t));
	parse_context.bind_parm_values = bind_parm_values;
	parse_context.num_bind_parm_values = bind->num_parms;
	/* Keep the cursor until the plan name is copied below, as "handle_parse()" does. It is deleted right after. */
	parse_context.skip_cursor_cleanup = TRUE;
	response_parms.session = session;
	response_parms.parm_name = bind->source;
	/* With PSQL_Parse, "run_query()" parses the query and generates its plan (or finds it, if it already exists), but does
	 * not run it. All this function needs from it is the name of that plan, in "parse_context.routine".
	 */
	status = run_query(&handle_query_response, (void *)&response_parms, PSQL_Parse, &parse_context);
	if (0 != status) {
		/* The same query parsed fine for the Parse message, but something it uses can have changed since then (e.g.
		 * another connection dropped the table). The error was already issued.
		 */
		CLEANUP_BIND_REPLAN_ROUTINE();
		return -1;
	}

	/* The cursor holds the literal parameters of this parse. They are the same as those stored on the portal (same query
	 * text, so same numbering). Only the plan name is needed, so delete the cursor.
	 */
	// KILL %ydboctocursor(cursorId)
	snprintf(cursor_str, sizeof(cursor_str), "%ld", parse_context.cursorId);
	YDB_STRING_TO_BUFFER(config->global_names.cursor, &cursor_subs[0]);
	YDB_STRING_TO_BUFFER(cursor_str, &cursor_subs[1]);
	status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
	YDB_ERROR_CHECK(status);

	assert(MAX_ROUTINE_LEN <= routine_buf->len_alloc);
	memcpy(routine_buf->buf_addr, parse_context.routine, MAX_ROUTINE_LEN);
	routine_buf->len_used = MAX_ROUTINE_LEN;
	CLEANUP_BIND_REPLAN_ROUTINE();
	return ((YDB_OK == status) ? 1 : -1);
}
