/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include "octo.h"

/* Given a plan name corresponding to an INSERT INTO query, it returns the number of rows that were inserted.
 * This function will be used for DELETE queries too in the future (to return the number of rows that were deleted)
 * hence the generic function name (i.e. no hardcoding of INSERT INTO in the function name).
 * In case of any errors, this returns the number 0.
 */
int get_row_count_from_plan_name(char *plan_name, ydb_long_t cursorId) {
	int	     row_count, status;
	ydb_buffer_t plan_meta_buffers[4], ret_buff;
	char	     row_count_buff[INT64_TO_STRING_MAX];

	YDB_STRING_TO_BUFFER(config->global_names.octo, &plan_meta_buffers[0]);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_PLAN_METADATA, &plan_meta_buffers[1]);
	YDB_STRING_TO_BUFFER(plan_name, &plan_meta_buffers[2]);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_OUTPUT_KEY, &plan_meta_buffers[3]);
	OCTO_SET_BUFFER(ret_buff, row_count_buff);
	status = ydb_get_s(&plan_meta_buffers[0], 3, &plan_meta_buffers[1], &ret_buff);
	YDB_ERROR_CHECK(status);
	row_count = 0; /* Default value is 0 in case of errors */
	if (YDB_OK == status) {
		char cursorId_str[INT64_TO_STRING_MAX];

		/* Now that we know the output key id, get the number of inserted rows using that */
		YDB_STRING_TO_BUFFER(config->global_names.cursor, &plan_meta_buffers[0]);
		snprintf(cursorId_str, INT64_TO_STRING_MAX, "%ld", cursorId);
		YDB_STRING_TO_BUFFER(cursorId_str, &plan_meta_buffers[1]);
		YDB_STRING_TO_BUFFER(OCTOLIT_ROW_COUNT, &plan_meta_buffers[2]);
		plan_meta_buffers[3] = ret_buff;
		status = ydb_get_s(&plan_meta_buffers[0], 3, &plan_meta_buffers[1], &ret_buff);
		YDB_ERROR_CHECK(status);
		if (YDB_OK == status) {
			int64_t tmp_long;

			ret_buff.buf_addr[ret_buff.len_used] = '\0';
			tmp_long = strtol(ret_buff.buf_addr, NULL, 10);
			if ((LONG_MAX != tmp_long) && (LONG_MIN != tmp_long) && (0 <= tmp_long)) {
				row_count = (int)tmp_long;
			}
		}
	}
	return row_count;
}
