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

/* Called by "handle_parse()" after the query of a Parse message was parsed, if the parse set
 * "parse_context->plan_depends_on_bind_values" (YDBOcto#1158).
 *
 * The plan of such a query depends on the values of bind parameters. The parse used the plan that is right for any value
 * (for example the table scan for "col LIKE $1", see "get_like_prefix_operation()"). The best plan depends on the values
 * that a later Bind message supplies, so "get_bind_replan_routine()" parses the query again at Bind, with the same data
 * types as here:
 *	^session(id, OCTOLIT_PREPARED, <name>, OCTOLIT_PARSETYPES)	: number of data types in the Parse message
 *	^session(id, OCTOLIT_PREPARED, <name>, OCTOLIT_PARSETYPES, <i>) : data type of $i in the Parse message
 * These nodes exist only for such a query, so they also tell "get_bind_replan_routine()" whether to parse again.
 *
 * Returns 0 on success and 1 on error.
 */
int32_t store_bind_replan_info(Parse *parse, RoctoSession *session) {
	ydb_buffer_t statement_subs[6], value_buffer;
	char	     parm_num_str[INT16_TO_STRING_MAX], value_str[INT32_TO_STRING_MAX];
	int32_t	     status;
	int16_t	     parm_index;

	YDB_STRING_TO_BUFFER(config->global_names.session, &statement_subs[0]);
	YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &statement_subs[1]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PREPARED, &statement_subs[2]);
	YDB_STRING_TO_BUFFER(parse->dest, &statement_subs[3]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PARSETYPES, &statement_subs[4]);
	OCTO_SET_BUFFER(statement_subs[5], parm_num_str);
	OCTO_SET_BUFFER(value_buffer, value_str);

	/* M equivalent of this block:
	 *	SET %ydboctosession(id,"prepared",<name>,"parsetypes")=<number of data types>
	 *	FOR i=1:1:<number of data types> SET %ydboctosession(id,"prepared",<name>,"parsetypes",i)=<data type of $i>
	 */
	OCTO_INT16_TO_BUFFER(parse->num_parm_data_types, &value_buffer);
	status = ydb_set_s(&statement_subs[0], 4, &statement_subs[1], &value_buffer);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	for (parm_index = 0; parm_index < parse->num_parm_data_types; parm_index++) {
		OCTO_INT16_TO_BUFFER((int16_t)(parm_index + 1), &statement_subs[5]); // $i is 1-indexed
		OCTO_INT32_TO_BUFFER((int32_t)parse->parm_data_types[parm_index], &value_buffer);
		status = ydb_set_s(&statement_subs[0], 5, &statement_subs[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			return 1;
		}
	}
	return 0;
}
