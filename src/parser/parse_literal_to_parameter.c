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
	ydb_buffer_t  parm_count, literal_buf;
	ydb_buffer_t *parm_count_subs;
	int	      status = 0;

	if (update_existing) {
		// Prepare parameter count subscripts to store literal in database
		parm_count_subs = make_buffers(config->global_names.cursor, 3, parse_context->cursorIdString, OCTOLIT_PARAMETERS,
					       value->parameter_index);
	} else {
		// ROCTO ONLY: Track total number of parameters, both literals and PARAMETER_VALUES
		if (config->is_rocto) {
			assert(0 <= parse_context->total_parms);
			parse_context->total_parms++;
		}
		parm_count_subs = make_buffers(config->global_names.cursor, 2, parse_context->cursorIdString, OCTOLIT_PARAMETERS);
		YDB_MALLOC_BUFFER(&parm_count, INT64_TO_STRING_MAX);
		status = ydb_incr_s(&parm_count_subs[0], 2, &parm_count_subs[1], NULL, &parm_count);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&parm_count);
			free(parm_count_subs);
			OCTO_CFREE(memory_chunks);
			return 1;
		}
		parm_count.buf_addr[parm_count.len_used] = '\0';
		parm_count.len_used += 1;

		// Store current parameter count as index for later lookup by physical plan
		value->parameter_index = octo_cmalloc(memory_chunks, parm_count.len_used); // null terminator
		memcpy(value->parameter_index, parm_count.buf_addr, parm_count.len_used);

		// Prepare parameter count subscripts to store literal in database
		free(parm_count_subs);
		parm_count_subs = make_buffers(config->global_names.cursor, 3, parse_context->cursorIdString, OCTOLIT_PARAMETERS,
					       parm_count.buf_addr);
	}
	// Store literal value in database (mapped to above index) for later lookup by physical plan
	if (PARAMETER_VALUE == value->type) {
		YDB_STRING_TO_BUFFER("", &literal_buf); // If an extended query parameter, e.g. $1, we have no value to
							// store here. Use an empty string for now and populate in
							// handle_execute
	} else {
		YDB_STRING_TO_BUFFER(value->v.string_literal, &literal_buf);
	}
	status = ydb_set_s(&parm_count_subs[0], 3, &parm_count_subs[1], &literal_buf);
	free(parm_count_subs);
	if (!update_existing)
		// make_buffers reuses parm_count.buf_addr instead making a copy, so must maintain allocation until after ydb_set_s
		YDB_FREE_BUFFER(&parm_count);
	if (YDB_OK != status) {
		OCTO_CFREE(memory_chunks);
		return 1;
	}
	return 0;
}
