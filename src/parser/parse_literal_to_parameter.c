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
#include <ctype.h>

#include "octo.h"
#include "octo_types.h"
#include "parser.h"
#include "helpers.h"

int parse_literal_to_parameter(ParseContext *parse_context, SqlValue *value, boolean_t update_existing) {
	// Increment local variable to track query literals for "prepared statement" optimization
	ydb_buffer_t parm_count, literal_buf;
	ydb_buffer_t parm_count_subs[4];
	int	     status = 0;
	char	     parm_count_buff[INT64_TO_STRING_MAX];

	YDB_STRING_TO_BUFFER(config->global_names.cursor, &parm_count_subs[0]);
	YDB_STRING_TO_BUFFER(parse_context->cursorIdString, &parm_count_subs[1]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PARAMETERS, &parm_count_subs[2]);
	parm_count.buf_addr = parm_count_buff;
	parm_count.len_alloc = sizeof(parm_count_buff);
	parm_count.len_used = 0;
	if (update_existing) {
		parm_count.len_used = snprintf(parm_count.buf_addr, sizeof(parm_count_buff), "%d", value->parameter_index);
		assert(parm_count.len_used < parm_count.len_alloc);
		assert('\0' == parm_count.buf_addr[parm_count.len_used]);
	} else {
		// ROCTO ONLY: Track total number of parameters, both literals and PARAMETER_VALUES
		if (config->is_rocto) {
			assert(0 <= parse_context->total_parms);
			parse_context->total_parms++;
		}
		status = ydb_incr_s(&parm_count_subs[0], 2, &parm_count_subs[1], NULL, &parm_count);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		parm_count.buf_addr[parm_count.len_used] = '\0';
		// Store current parameter count as index for later lookup by physical plan
		value->parameter_index = atoi(parm_count.buf_addr);
	}
	// Prepare parameter count subscripts to store literal in database
	parm_count_subs[3] = parm_count;
	// Store literal value in database (mapped to above index) for later lookup by physical plan
	if (PARAMETER_VALUE == value->type) {
		YDB_STRING_TO_BUFFER("", &literal_buf); // If an extended query parameter, e.g. $1, we have no value to
							// store here. Use an empty string for now and populate in
							// handle_execute
	} else {
		YDB_STRING_TO_BUFFER(value->v.string_literal, &literal_buf);
	}
	status = ydb_set_s(&parm_count_subs[0], 3, &parm_count_subs[1], &literal_buf);
	if (YDB_OK != status) {
		OCTO_CFREE(memory_chunks);
		return 1;
	}
	return 0;
}
