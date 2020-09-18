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
#include "template_helpers.h"

#define DATA_ROW_PARMS_ARRAY_INIT_ALLOC 16
#define NO_GLOBAL_COLUMN_FORMAT		-1

int get_column_type_oid(ydb_buffer_t *plan_meta, ydb_buffer_t *value_buffer, int32_t *col_data_type) {
	int	status;
	int64_t tmp_long;

	// Retrieve the type OID for a single column, as this is needed to convert some types to binary format
	status = ydb_get_s(&plan_meta[0], 5, &plan_meta[1], value_buffer);
	assert(YDB_ERR_INVSTRLEN != status); // INT64_TO_STRING_MAX is enough to store any format code
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status)
		return 1;
	value_buffer->buf_addr[value_buffer->len_used] = '\0';

	tmp_long = strtol(value_buffer->buf_addr, NULL, 10);
	// PostgreSQL protocol specifies a 32-bit integer to store the column data type; details linked in message_formats.h
	if ((ERANGE != errno) && (0 <= tmp_long) && (INT32_MAX >= tmp_long)) {
		*col_data_type = (int32_t)tmp_long;
	} else {
		ERROR(ERR_LIBCALL_WITH_ARG, "strtol", value_buffer->buf_addr);
		return 1;
	}
	return 0;
}

int send_result_rows(int32_t cursor_id, void *_parms, char *plan_name) {
	QueryResponseParms *parms = (QueryResponseParms *)_parms;
	RoctoSession *	    session = parms->session;

	ydb_buffer_t cursor_subs[7], plan_meta[6], portal_subs[6];
	ydb_buffer_t value_buffer, row_value_buffer, total_rows_buffer;

	char cursor_id_str[INT64_TO_STRING_MAX];
	char value_str[INT64_TO_STRING_MAX];
	char output_key_str[INT32_TO_STRING_MAX];
	char total_rows_str[INT32_TO_STRING_MAX];
	char cur_column_str[INT16_TO_STRING_MAX];
	char cur_row_str[INT32_TO_STRING_MAX];

	int	     status;
	DataRow *    data_row;
	DataRowParm *data_row_parms, *cur_row_parms;
	int16_t *    col_format_codes = NULL;
	int32_t *    col_data_types = NULL;
	int16_t	     num_columns, cur_column, num_format_codes, global_column_format;
	int32_t	     data_row_parms_alloc_len = 0;
	int32_t	     total_rows, cur_row, last_row, rows_remaining;
	int64_t	     tmp_long;

	OCTO_SET_BUFFER(value_buffer, value_str);
	// Go through and make rows for each row in the output plan
	parms->data_sent = TRUE;

	// Populate buffers for cursor LVN and metadata GVN
	snprintf(cursor_id_str, INT64_TO_STRING_MAX, "%d", cursor_id);
	YDB_STRING_TO_BUFFER(config->global_names.cursor, &cursor_subs[0]);
	YDB_STRING_TO_BUFFER(cursor_id_str, &cursor_subs[1]);
	YDB_STRING_TO_BUFFER(OCTOLIT_KEYS, &cursor_subs[2]);
	YDB_STRING_TO_BUFFER("", &cursor_subs[4]);
	YDB_STRING_TO_BUFFER("", &cursor_subs[5]);

	YDB_STRING_TO_BUFFER(config->global_names.octo, &plan_meta[0]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PLAN_METADATA, &plan_meta[1]);
	YDB_STRING_TO_BUFFER(plan_name, &plan_meta[2]);
	YDB_STRING_TO_BUFFER(OCTOLIT_OUTPUT_KEY, &plan_meta[3]);

	// Retrieve output key ID for result rows
	OCTO_SET_BUFFER(cursor_subs[3], output_key_str); // Output key ID
	status = ydb_get_s(&plan_meta[0], 3, &plan_meta[1], &cursor_subs[3]);
	assert(YDB_ERR_INVSTRLEN != status);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		ERROR(ERR_DATABASE_FILES_OOS, "");
		return 1;
	}
	cursor_subs[3].buf_addr[cursor_subs[3].len_used] = '\0';

	// Retrieve the total number of columns for the given output key.
	YDB_STRING_TO_BUFFER(OCTOLIT_OUTPUT_COLUMNS, &plan_meta[3]);
	status = ydb_get_s(&plan_meta[0], 3, &plan_meta[1], &value_buffer);
	assert(YDB_ERR_INVSTRLEN != status); // INT64_TO_STRING_MAX should be enough to store the total number of columns
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		ERROR(ERR_DATABASE_FILES_OOS, "");
		return 1;
	}
	value_buffer.buf_addr[value_buffer.len_used] = '\0';
	tmp_long = strtol(value_buffer.buf_addr, NULL, 10);
	// PostgreSQL protocol specifies a 16-bit integer to store the total number of columns; details linked in message_formats.h
	if ((ERANGE != errno) && (0 <= tmp_long) && (INT16_MAX >= tmp_long)) {
		num_columns = (int16_t)tmp_long;
	} else {
		ERROR(ERR_LIBCALL, "strtol");
		return 1;
	}

	// Retrieve the total number of rows for the given output key.
	OCTO_SET_BUFFER(total_rows_buffer, total_rows_str);
	status = ydb_get_s(&cursor_subs[0], 5, &cursor_subs[1], &total_rows_buffer);
	assert(YDB_ERR_INVSTRLEN != status);
	if (YDB_ERR_LVUNDEF == status) {
		total_rows = 0;
	} else {
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			return 1;
		} else {
			total_rows_buffer.buf_addr[total_rows_buffer.len_used] = '\0';
			// Convert row ID from M string to integer
			tmp_long = strtol(total_rows_buffer.buf_addr, NULL, 10);
			// PostgreSQL protocol specifies a 32-bit integer to store the maximum number of rows to return
			// Details linked in message_formats.h
			if ((ERANGE != errno) && (0 <= tmp_long) && (INT32_MAX >= tmp_long)) {
				total_rows = (int32_t)tmp_long;
			} else {
				ERROR(ERR_LIBCALL, "strtol");
				return 1;
			}
		}
	}
	if (0 == total_rows) {
		// There are no rows to send, just return cleanup and return
		return 0;
	}
	if (0 >= parms->max_data_to_send) {
		// Send all rows if no limit is specified
		parms->max_data_to_send = total_rows;
	}

	// Retrieve the number of rows remaining to be sent and, if this is an Execute request, the result row format code(s).
	// If this LVN doesn't exist, this is the first Execute request for this query, so all rows are remaining.
	if (NULL == parms->portal_name) {
		// This is a Simple Query request, so no portal exists and all rows should be returned.
		rows_remaining = total_rows;
		last_row = rows_remaining;
		cur_row = 1;
		UNUSED(portal_subs);	  // Avoid unused variable compiler warning
		global_column_format = 0; // Avoid uninitialized variable compiler warning
	} else {
		// Create buffers to retrieve the number of row formats specified for this portal
		YDB_STRING_TO_BUFFER(config->global_names.session, &portal_subs[0]);
		YDB_STRING_TO_BUFFER(session->session_id->buf_addr, &portal_subs[1]);
		YDB_STRING_TO_BUFFER(OCTOLIT_BOUND, &portal_subs[2]);
		YDB_STRING_TO_BUFFER(parms->portal_name, &portal_subs[3]);
		YDB_STRING_TO_BUFFER("col_formats", &portal_subs[4]);

		status = ydb_get_s(&portal_subs[0], 4, &portal_subs[1], &value_buffer);
		assert(YDB_ERR_INVSTRLEN != status); // INT64_TO_STRING_MAX should be enough to store the number of row formats
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			return 1;
		}
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		tmp_long = strtol(value_buffer.buf_addr, NULL, 10);
		// PostgreSQL protocol specifies a 16-bit integer to store the number of column format codes
		// Details linked in message_formats.h
		if ((ERANGE != errno) && (0 <= tmp_long) && (INT16_MAX >= tmp_long)) {
			num_format_codes = (int16_t)tmp_long;
		} else {
			ERROR(ERR_LIBCALL, "strtol");
			return 1;
		}
		// If the number of format codes is 0 or 1, a single format is used for all column:
		//	Number of codes == 0: All are text format
		//	Number of codes == 1: All are text or all are binary format, depending on what code is specified
		//	Number of codes > 1:  Column formats may be a combination of text and binary, specified per column
		// Details linked in message_formats.h
		if (0 == num_format_codes) {
			global_column_format = 0;
		} else if (1 == num_format_codes) {
			// A particular format code was specified for all result columns - retrieve from under the first column
			YDB_LITERAL_TO_BUFFER("1", &portal_subs[5]);
			status = ydb_get_s(&portal_subs[0], 5, &portal_subs[1], &value_buffer);
			assert(YDB_ERR_INVSTRLEN != status); // INT64_TO_STRING_MAX is enough to store any format code
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				return 1;
			}
			value_buffer.buf_addr[value_buffer.len_used] = '\0';
			tmp_long = strtol(value_buffer.buf_addr, NULL, 10);
			// PostgreSQL protocol specifies a 16-bit integer to store a column format code
			// Details linked in message_formats.h
			if ((ERANGE != errno) && (0 <= tmp_long) && (INT16_MAX >= tmp_long)) {
				global_column_format = (int16_t)tmp_long;
			} else {
				ERROR(ERR_LIBCALL, "strtol");
				return 1;
			}
			if (1 == global_column_format) { // Client requested binary format, so we need the column data types
				OCTO_SET_BUFFER(portal_subs[5], cur_column_str);
				OCTO_SET_BUFFER(plan_meta[4], cur_column_str);
				col_data_types = calloc(num_columns, sizeof(int32_t));
				for (cur_column = 0; cur_column < num_columns; cur_column++) {
					OCTO_INT16_TO_BUFFER((int16_t)(cur_column + 1), &portal_subs[5]); // Columns indexed from 1
					// Using same underlying string for both buffers, so update len_used to match
					plan_meta[4].len_used = portal_subs[5].len_used;
					// Retrieve the type OID for each column: needed to convert some types to binary format
					YDB_STRING_TO_BUFFER(OCTOLIT_DATA_TYPE, &plan_meta[5]);
					status = get_column_type_oid(plan_meta, &value_buffer, &col_data_types[cur_column]);
					if (0 != status) {
						free(col_data_types);
						return 1;
					}
				}
			}
		} else {
			global_column_format = NO_GLOBAL_COLUMN_FORMAT;
			// The client should specify a format code for each column when not using the same format for all columns
			if (num_format_codes != num_columns) {
				ERROR(ERR_ROCTO_INVALID_NUMBER_COLUMN_FORMAT_CODES, "send_result_rows", parms->portal_name,
				      num_columns, num_format_codes);
				return 1;
			}
			// Unique format codes were specified for all result for all columns, retrieve them
			OCTO_SET_BUFFER(portal_subs[5], cur_column_str);
			OCTO_SET_BUFFER(plan_meta[4], cur_column_str);
			col_data_types = calloc(num_columns, sizeof(int32_t));
			col_format_codes = (int16_t *)calloc(num_format_codes, sizeof(int16_t));
			for (cur_column = 0; cur_column < num_columns; cur_column++) {
				OCTO_INT16_TO_BUFFER((int16_t)(cur_column + 1), &portal_subs[5]); // Columns indexed from 1
				// Using same underlying string for both buffers, so update len_used to match
				plan_meta[4].len_used = portal_subs[5].len_used;
				status = ydb_get_s(&portal_subs[0], 5, &portal_subs[1], &value_buffer);
				assert(YDB_ERR_INVSTRLEN != status); // INT64_TO_STRING_MAX is enough to store any format code
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					free(col_data_types);
					free(col_format_codes);
					return 1;
				}
				value_buffer.buf_addr[value_buffer.len_used] = '\0';
				tmp_long = strtol(value_buffer.buf_addr, NULL, 10);
				// PostgreSQL protocol specifies a 16-bit integer to store each column format code
				// Details linked in message_formats.h
				if ((ERANGE != errno) && (0 <= tmp_long) && (INT16_MAX >= tmp_long)) {
					col_format_codes[cur_column] = (int16_t)tmp_long;
				} else {
					ERROR(ERR_LIBCALL, "strtol");
					free(col_data_types);
					free(col_format_codes);
					return 1;
				}
				// Retrieve the type OID for each column, as this is needed to convert some types to binary format
				YDB_STRING_TO_BUFFER(OCTOLIT_DATA_TYPE, &plan_meta[5]);
				status = get_column_type_oid(plan_meta, &value_buffer, &col_data_types[cur_column]);
				if (0 != status) {
					free(col_data_types);
					free(col_format_codes);
					return 1;
				}
			}
		}

		// Retrieve the number of rows remaining for this portal
		YDB_STRING_TO_BUFFER("rows_remaining", &portal_subs[4]);
		status = ydb_get_s(&portal_subs[0], 4, &portal_subs[1], &value_buffer);
		assert(YDB_ERR_INVSTRLEN != status); // INT64_TO_STRING_MAX is enough to store any row number total
		if (YDB_ERR_LVUNDEF == status) {
			// This is the first Execute request, so all rows are remaining.
			rows_remaining = total_rows;
			cur_row = 1;
		} else {
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				free(col_data_types);
				free(col_format_codes);
				return 1;
			}
			value_buffer.buf_addr[value_buffer.len_used] = '\0';
			tmp_long = strtol(value_buffer.buf_addr, NULL, 10);
			// PostgreSQL protocol specifies a 32-bit integer to store the maximum number of rows to return
			// Details linked in message_formats.h
			if ((ERANGE != errno) && (0 <= tmp_long) && (INT32_MAX >= tmp_long)) {
				rows_remaining = (int32_t)tmp_long;
			} else {
				ERROR(ERR_LIBCALL, "strtol");
				free(col_data_types);
				free(col_format_codes);
				return 1;
			}
			assert(total_rows >= rows_remaining);
			cur_row = total_rows - rows_remaining;
		}
		if (rows_remaining <= parms->max_data_to_send)
			last_row = total_rows;
		else
			last_row = cur_row + parms->max_data_to_send - 1; // Offset the initial cur_row value of 1
		assert(last_row <= total_rows);
	}

	OCTO_SET_BUFFER(cursor_subs[6], cur_row_str);
	data_row_parms = (DataRowParm *)malloc(DATA_ROW_PARMS_ARRAY_INIT_ALLOC * sizeof(DataRowParm));
	data_row_parms_alloc_len = DATA_ROW_PARMS_ARRAY_INIT_ALLOC;
	// Retrieve the value of each row
	assert(0 == parms->rows_sent);
	YDB_MALLOC_BUFFER(&row_value_buffer, OCTO_INIT_BUFFER_LEN);
	while (cur_row <= last_row) {
		int	       cur_column;
		unsigned char *buff, *buff_top;

		OCTO_INT32_TO_BUFFER(cur_row, &cursor_subs[6]);
		status = ydb_get_s(&cursor_subs[0], 6, &cursor_subs[1], &row_value_buffer);
		// Expand row_value_buffer allocation until it's large enough to store the retrieved row value
		if (YDB_ERR_INVSTRLEN == status) {
			int newsize = row_value_buffer.len_used;

			YDB_FREE_BUFFER(&row_value_buffer);
			YDB_MALLOC_BUFFER(&row_value_buffer, newsize);
			status = ydb_get_s(&cursor_subs[0], 6, &cursor_subs[1], &row_value_buffer);
			assert(YDB_ERR_INVSTRLEN != status);
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&row_value_buffer);
			free(data_row_parms);
			free(col_data_types);
			free(col_format_codes);
			return 1;
		}
		assert(0 < data_row_parms_alloc_len);
		buff = (unsigned char *)row_value_buffer.buf_addr;
		buff_top = buff + row_value_buffer.len_used;
		// Loop over row columns to build up DataRowParms for transmission to client
		for (cur_column = 0, cur_row_parms = data_row_parms;; cur_row_parms++) {
			int hdr_len, data_len;

			// Assign column format code for the current column
			if (NO_GLOBAL_COLUMN_FORMAT == global_column_format) {
				assert(NULL != col_format_codes); /* We should only be here if
								   * the client specified column formats
								   */
				cur_row_parms->format = col_format_codes[cur_column];
			} else {
				cur_row_parms->format = global_column_format;
			}

			hdr_len = get_mval_len(buff, &data_len);
			// Store length of the M value for constructing a DataRow in make_data_row
			if (0 == buff[0]) { // This mval was $ZYSQLNULL per comment on str2mval in aux/_ydboctoplanhelpers.m
				cur_row_parms->length = PSQL_NULL;
			} else {
				cur_row_parms->length = data_len;
			}
			buff += hdr_len;
			cur_row_parms->value = (char *)buff;
			buff += data_len;
			assert(cur_column < data_row_parms_alloc_len);

			cur_column++;
			if (buff_top <= buff) {
				assert(buff_top == buff);
				break;
			}
			// Current allocation is not enough. Expand to twice current size.
			if (cur_column >= data_row_parms_alloc_len) {
				DataRowParm *tmp;

				tmp = (DataRowParm *)malloc(data_row_parms_alloc_len * 2 * sizeof(DataRowParm));
				memcpy(tmp, data_row_parms, (data_row_parms_alloc_len * sizeof(DataRowParm)));
				free(data_row_parms);
				cur_row_parms = tmp + (cur_row_parms - data_row_parms); /* Fix `cur_row_parms` to new base */
				data_row_parms = tmp;
				data_row_parms_alloc_len = data_row_parms_alloc_len * 2;
			}
			assert(cur_column < data_row_parms_alloc_len);
		}
		// Send row data to client
		data_row = make_data_row(data_row_parms, cur_column, col_data_types);
		send_message(parms->session, (BaseMessage *)(&data_row->type));
		free(data_row);
		// Move to the next index
		cur_row++;
		parms->rows_sent++;
	}
	rows_remaining -= parms->rows_sent;
	free(data_row_parms);
	free(col_data_types);
	free(col_format_codes);

	// Cleanup tables
	if (0 == rows_remaining) {
		YDB_FREE_BUFFER(&row_value_buffer);
		status = ydb_delete_s(&cursor_subs[0], 1, &cursor_subs[1], YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			return 1;
	} else {
		OCTO_INT32_TO_BUFFER(rows_remaining, &row_value_buffer);
		status = ydb_set_s(&portal_subs[0], 4, &portal_subs[1], &row_value_buffer);
		YDB_FREE_BUFFER(&row_value_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			return 1;
		return PORTAL_SUSPENDED; // Signal there are rows remaining to be sent.
	}
	return 0;
}
