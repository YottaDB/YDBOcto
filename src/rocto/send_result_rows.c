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
#include "message_formats.h"
#include "rocto.h"
#include "physical_plan.h"
#include "helpers.h"

#define	DATA_ROW_PARMS_ARRAY_INIT_ALLOC	16

int send_result_rows(int32_t cursor_id, void *_parms, char *plan_name) {
	QueryResponseParms	*parms = (QueryResponseParms*)_parms;
	RoctoSession		*session = parms->session;

	ydb_buffer_t		*cursor_subs, *outkey_buffers, *portal_subs = NULL;
	ydb_buffer_t		value_buffer, total_rows_buffer;

	char			cursor_id_str[INT64_TO_STRING_MAX];
	int			status;
	DataRow			*data_row;
	DataRowParm		*data_row_parms, *cur_row_parms;
	int32_t			data_row_parms_alloc_len = 0;
	int32_t			total_rows, cur_row, last_row, rows_remaining;
	int64_t			tmp_long;

	YDB_MALLOC_BUFFER(&value_buffer, MAX_STR_CONST);
	data_row_parms = (DataRowParm *)malloc(DATA_ROW_PARMS_ARRAY_INIT_ALLOC * sizeof(DataRowParm));
	data_row_parms_alloc_len = DATA_ROW_PARMS_ARRAY_INIT_ALLOC;

	// Go through and make rows for each row in the output plan
	parms->data_sent = TRUE;

	// Populate buffers for cursor LVN and metadata GVN
	snprintf(cursor_id_str, INT64_TO_STRING_MAX, "%d", cursor_id);
	cursor_subs = make_buffers(config->global_names.cursor, 6, cursor_id_str, "keys", "", "", "", "");
	outkey_buffers = make_buffers("^%ydboctoocto", 3, "plan_metadata", plan_name, "output_key");

	// Retrieve output key ID for result rows
	YDB_MALLOC_BUFFER(&cursor_subs[3], INT32_TO_STRING_MAX);	// Output key ID
	status = ydb_get_s(&outkey_buffers[0], 3, &outkey_buffers[1], &cursor_subs[3]);
	free(outkey_buffers);
	YDB_ERROR_CHECK(status);
	if(YDB_OK != status) {
		ERROR(ERR_DATABASE_FILES_OOS, "");
		YDB_FREE_BUFFER(&cursor_subs[3]);
		YDB_FREE_BUFFER(&value_buffer);
		free(cursor_subs);
		free(data_row_parms);
		return 1;
	}
	cursor_subs[3].buf_addr[cursor_subs[3].len_used] = '\0';

	// Retrieve the total number of rows for the given output key.
	YDB_MALLOC_BUFFER(&total_rows_buffer, INT32_TO_STRING_MAX);
	status = ydb_get_s(&cursor_subs[0], 5, &cursor_subs[1], &total_rows_buffer);
	if (YDB_ERR_LVUNDEF == status) {
		total_rows = 0;
		status = YDB_OK;
	} else {
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&total_rows_buffer);
			YDB_FREE_BUFFER(&cursor_subs[3]);
			YDB_FREE_BUFFER(&value_buffer);
			free(cursor_subs);
			free(data_row_parms);
			return 1;
		} else {
			total_rows_buffer.buf_addr[total_rows_buffer.len_used] = '\0';
			// Convert row ID from M string to integer
			tmp_long = strtol(total_rows_buffer.buf_addr, NULL, 10);
			YDB_FREE_BUFFER(&total_rows_buffer);
			// PostgreSQL protocol specifies a 32-bit integer to store the maximum number of rows to return
			if ((ERANGE != errno) && (0 <= tmp_long) && (INT32_MAX >= tmp_long)) {
				total_rows = (int32_t)tmp_long;
			} else {
				ERROR(ERR_LIBCALL, "strtol");
				YDB_FREE_BUFFER(&cursor_subs[3]);
				YDB_FREE_BUFFER(&value_buffer);
				free(cursor_subs);
				free(data_row_parms);
				return 1;
			}
		}
	}
	if (0 == total_rows) {
		// There are no rows to send, just return cleanup and return
		YDB_FREE_BUFFER(&cursor_subs[3]);
		YDB_FREE_BUFFER(&value_buffer);
		free(cursor_subs);
		free(data_row_parms);
		return 0;
	}
	if (0 >= parms->max_data_to_send) {
		// Send all rows if no limit is specified
		parms->max_data_to_send = total_rows;
	}

	// Retrieve the number of rows remaining to be sent. If this LVN doesn't exist, this is the first
	// Execute request for this query, so all rows are remaining.
	if (NULL == parms->portal_name) {
		// This is a Simple Query request, so no portal exists and all rows should be returned.
		rows_remaining = total_rows;
		last_row = rows_remaining;
		cur_row = 1;
		UNUSED(portal_subs);	// Avoid compiler warning
	} else {
		portal_subs = make_buffers(config->global_names.session, 4, session->session_id->buf_addr, "bound",
				parms->portal_name, "rows_remaining");
		status = ydb_get_s(&portal_subs[0], 4, &portal_subs[1], &value_buffer);
		if (YDB_ERR_LVUNDEF == status) {
			// This is the first Execute request, so all rows are remaining.
			rows_remaining = total_rows;
			cur_row = 1;
		} else {
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				YDB_FREE_BUFFER(&cursor_subs[3]);
				YDB_FREE_BUFFER(&value_buffer);
				free(cursor_subs);
				free(data_row_parms);
				free(portal_subs);
				return 1;
			}
			value_buffer.buf_addr[value_buffer.len_used] = '\0';
			rows_remaining = strtol(value_buffer.buf_addr, NULL, 10);
			// PostgreSQL protocol specifies a 32-bit integer to store the maximum number of rows to return
			if ((ERANGE != errno) && (0 <= tmp_long) && (INT32_MAX >= tmp_long)) {
				rows_remaining = (int32_t)tmp_long;
			} else {
				ERROR(ERR_LIBCALL, "strtol");
				YDB_FREE_BUFFER(&cursor_subs[3]);
				YDB_FREE_BUFFER(&value_buffer);
				free(cursor_subs);
				free(data_row_parms);
				free(portal_subs);
				return 1;
			}
			assert(total_rows >= rows_remaining);
			cur_row = total_rows - rows_remaining;
		}
		if (rows_remaining <= parms->max_data_to_send)
			last_row = total_rows;
		else
			last_row = cur_row + parms->max_data_to_send - 1;	// Offset the initial cur_row value of 1
		assert(last_row <= total_rows);
	}

	YDB_MALLOC_BUFFER(&cursor_subs[6], INT32_TO_STRING_MAX);
	// Retrieve the value of each row
	assert(0 == parms->rows_sent);
	while (cur_row <= last_row) {
		int		number_of_columns;
		unsigned char	*buff, *buff_top;

		OCTO_INT32_TO_BUFFER(cur_row, &cursor_subs[6]);
		status = ydb_get_s(&cursor_subs[0], 6, &cursor_subs[1], &value_buffer);
		// Expand value_buffer allocation until it's large enough to store the retrieved row value
		if (YDB_ERR_INVSTRLEN == status) {
			int	newsize = value_buffer.len_used;

			YDB_FREE_BUFFER(&value_buffer);
			YDB_MALLOC_BUFFER(&value_buffer, newsize);
			status = ydb_get_s(&cursor_subs[0], 6, &cursor_subs[1], &value_buffer);
			assert(YDB_ERR_INVSTRLEN != status);
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&cursor_subs[6]);
			YDB_FREE_BUFFER(&cursor_subs[3]);
			YDB_FREE_BUFFER(&value_buffer);
			free(cursor_subs);
			free(data_row_parms);
			return 1;
		}
		assert(0 < data_row_parms_alloc_len);
		buff = (unsigned char *)value_buffer.buf_addr;
		buff_top = buff + value_buffer.len_used;
		// Loop over row columns to build up DataRowParms for transmission to client
		for (number_of_columns = 0, cur_row_parms = data_row_parms; ; cur_row_parms++) {
			int	hdr_len, data_len;

			hdr_len = get_mval_len(buff, &data_len);
			cur_row_parms->length = data_len;
			buff += hdr_len;
			cur_row_parms->value = (char *)buff;
			buff += data_len;
			assert(number_of_columns < data_row_parms_alloc_len);
			number_of_columns++;
			if (buff_top <= buff) {
				assert(buff_top == buff);
				break;
			}
			// Current allocation is not enough. Expand to twice current size.
			if (number_of_columns >= data_row_parms_alloc_len) {
				DataRowParm	*tmp;

				tmp = (DataRowParm*)malloc(data_row_parms_alloc_len * 2 * sizeof(DataRowParm));
				memcpy(tmp, data_row_parms, (data_row_parms_alloc_len * sizeof(DataRowParm)));
				free(data_row_parms);
				cur_row_parms = tmp + (cur_row_parms - data_row_parms);	/* Fix `cur_row_parms` to new base */
				data_row_parms = tmp;
				data_row_parms_alloc_len = data_row_parms_alloc_len * 2;
			}
			assert(number_of_columns < data_row_parms_alloc_len);
		}
		// Send row data to client
		data_row = make_data_row(data_row_parms, number_of_columns);
		send_message(parms->session, (BaseMessage*)(&data_row->type));
		free(data_row);
		// Move to the next index
		cur_row++;
		parms->rows_sent++;
	}
	YDB_FREE_BUFFER(&cursor_subs[6]);
	YDB_FREE_BUFFER(&cursor_subs[3]);
	free(data_row_parms);

	rows_remaining -= parms->rows_sent;

	// Cleanup tables
	if (0 == rows_remaining) {
		YDB_FREE_BUFFER(&value_buffer);
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		free(cursor_subs);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			return 1;
	} else {
		assert(NULL != portal_subs);	// There is no portal and all rows are sent in the Simple Query case.
		free(cursor_subs);
		OCTO_INT32_TO_BUFFER(rows_remaining, &value_buffer);
		status = ydb_set_s(&portal_subs[0], 4, &portal_subs[1], &value_buffer);
		free(portal_subs);
		YDB_FREE_BUFFER(&value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			return 1;
		return PORTAL_SUSPENDED;	// Signal there are rows remaining to be sent.
	}
	return 0;
}
