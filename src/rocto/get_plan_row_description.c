/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"

RowDescription *get_plan_row_description(ydb_buffer_t *plan_filename) {
	RowDescriptionParm *parms;
	RowDescription	   *ret;
	ydb_buffer_t	   *plan_meta, value_buffer;
	int64_t		    tmp_long, i;
	int32_t		    status;
	int16_t		    num_columns, cur_column;
	char		   *column_name = NULL;
	char		    cur_column_str[INT16_TO_STRING_MAX];

	plan_meta = make_buffers(config->global_names.octo, 5, OCTOLIT_PLAN_METADATA, "", OCTOLIT_OUTPUT_COLUMNS, "", "");
	plan_meta[2] = *plan_filename;
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&value_buffer, OCTO_INIT_BUFFER_LEN);

	// Get total number of columns for the given plan
	status = ydb_get_s(&plan_meta[0], 3, &plan_meta[1], &value_buffer);
	if (YDB_ERR_INVSTRLEN == status) {
		EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
		status = ydb_get_s(&plan_meta[0], 3, &plan_meta[1], &value_buffer);
		assert(YDB_ERR_INVSTRLEN != status);
	}
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		YDB_FREE_BUFFER(&value_buffer);
		free(plan_meta);
		return NULL;
	}
	value_buffer.buf_addr[value_buffer.len_used] = '\0';
	tmp_long = strtol(value_buffer.buf_addr, NULL, 10);
	// PostgreSQL protocol specifies a 16-bit integer to store the number of columns; details linked in rocto/message_formats.h
	if ((ERANGE != errno) && (0 <= tmp_long) && (INT16_MAX >= tmp_long)) {
		num_columns = (int16_t)tmp_long;
	} else {
		ERROR(ERR_INVALID_NUMBER, "get_plan_row_description", "number of select columns", tmp_long * -1, 0, INT16_MAX);
		YDB_FREE_BUFFER(&value_buffer);
		free(plan_meta);
		return NULL;
	}

	// Populate RowDescriptionParms with data from plan, column by column
	OCTO_SET_BUFFER(plan_meta[4], cur_column_str);
	parms = calloc(num_columns, sizeof(RowDescriptionParm));
	for (cur_column = 0; cur_column < num_columns; cur_column++) {
		OCTO_INT16_TO_BUFFER((int16_t)(cur_column + 1), &plan_meta[4]); // Columns are indexed from 1, not 0

		YDB_LITERAL_TO_BUFFER(OCTOLIT_NAME, &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		if (YDB_ERR_INVSTRLEN == status) {
			EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
			status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			assert(YDB_ERR_INVSTRLEN != status);
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		column_name = (char *)malloc(sizeof(char) * (value_buffer.len_used + 1));
		memcpy(column_name, value_buffer.buf_addr, value_buffer.len_used);
		column_name[value_buffer.len_used] = '\0';
		parms[cur_column].name = column_name;

		YDB_LITERAL_TO_BUFFER(OCTOLIT_TABLE_ID, &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		if (YDB_ERR_INVSTRLEN == status) {
			EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
			status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			assert(YDB_ERR_INVSTRLEN != status);
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		tmp_long = strtol(value_buffer.buf_addr, NULL, 10);
		// PostgreSQL protocol specifies a 32-bit integer to store the table OID; details linked in rocto/message_formats.h
		if ((ERANGE != errno) && (0 <= tmp_long) && (INT32_MAX >= tmp_long)) {
			parms[cur_column].table_id = (int32_t)tmp_long;
		} else {
			ERROR(ERR_LIBCALL_WITH_ARG, "strtol()", value_buffer.buf_addr);
			status = -1;
			break;
		}

		YDB_LITERAL_TO_BUFFER(OCTOLIT_COLUMN_ID, &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		if (YDB_ERR_INVSTRLEN == status) {
			EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
			status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			assert(YDB_ERR_INVSTRLEN != status);
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		tmp_long = strtol(value_buffer.buf_addr, NULL, 10);
		// PostgreSQL protocol specifies a 16-bit integer to store the column OID; details linked in rocto/message_formats.h
		if ((ERANGE != errno) && (0 <= tmp_long) && (INT16_MAX >= tmp_long)) {
			parms[cur_column].column_id = (int16_t)tmp_long;
		} else {
			ERROR(ERR_LIBCALL_WITH_ARG, "strtol()", value_buffer.buf_addr);
			status = -1;
			break;
		}

		YDB_LITERAL_TO_BUFFER(OCTOLIT_DATA_TYPE, &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		if (YDB_ERR_INVSTRLEN == status) {
			EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
			status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			assert(YDB_ERR_INVSTRLEN != status);
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		tmp_long = strtol(value_buffer.buf_addr, NULL, 10);
		// PostgreSQL protocol specifies a 32-bit integer to store the column data type;
		// details linked in rocto/message_formats.h
		if ((ERANGE != errno) && (0 <= tmp_long) && (INT32_MAX >= tmp_long)) {
			parms[cur_column].data_type = (int32_t)tmp_long;
		} else {
			ERROR(ERR_LIBCALL_WITH_ARG, "strtol()", value_buffer.buf_addr);
			status = -1;
			break;
		}

		YDB_LITERAL_TO_BUFFER(OCTOLIT_DATA_TYPE_SIZE, &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		if (YDB_ERR_INVSTRLEN == status) {
			EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
			status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			assert(YDB_ERR_INVSTRLEN != status);
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		tmp_long = strtol(value_buffer.buf_addr, NULL, 10);
		// PostgreSQL protocol specifies a 16-bit integer to store the column data type size,
		// where -1 indicates an indeterminate type size, -2 indicates unknown; details linked in rocto/message_formats.h
		if ((ERANGE != errno) && (-2 <= tmp_long) && (INT16_MAX >= tmp_long)) {
			parms[cur_column].data_type_size = (int16_t)tmp_long;
		} else {
			ERROR(ERR_LIBCALL_WITH_ARG, "strtol()", value_buffer.buf_addr);
			status = -1;
			break;
		}

		YDB_LITERAL_TO_BUFFER(OCTOLIT_TYPE_MODIFIER, &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		if (YDB_ERR_INVSTRLEN == status) {
			EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
			status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			assert(YDB_ERR_INVSTRLEN != status);
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		tmp_long = strtol(value_buffer.buf_addr, NULL, 10);
		// PostgreSQL protocol specifies a 32-bit integer to store the column data type modifier,
		// where -1 indicates no type modifier is necessary; details linked in rocto/message_formats.h
		if ((ERANGE != errno) && (-1 <= tmp_long) && (INT32_MAX >= tmp_long)) {
			parms[cur_column].type_modifier = (int32_t)tmp_long;
		} else {
			ERROR(ERR_LIBCALL_WITH_ARG, "strtol()", value_buffer.buf_addr);
			status = -1;
			break;
		}

		YDB_LITERAL_TO_BUFFER(OCTOLIT_FORMAT_CODE, &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		if (YDB_ERR_INVSTRLEN == status) {
			EXPAND_YDB_BUFFER_T_ALLOCATION(value_buffer);
			status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
			assert(YDB_ERR_INVSTRLEN != status);
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status)
			break;
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		tmp_long = strtol(value_buffer.buf_addr, NULL, 10);
		// PostgreSQL protocol specifies a 16-bit integer to store the column format code: text (0) or binary (1)
		// Details linked in rocto/message_formats.h
		if ((ERANGE != errno) && (0 <= tmp_long) && (INT16_MAX >= tmp_long)) {
			parms[cur_column].format_code = (int16_t)tmp_long;
		} else {
			ERROR(ERR_LIBCALL_WITH_ARG, "strtol()", value_buffer.buf_addr);
			status = -1;
			break;
		}
	}
	YDB_FREE_BUFFER(&value_buffer);
	free(plan_meta);
	if (YDB_OK == status) {
		ret = make_row_description(parms, num_columns);
	} else {
		ret = NULL;
	}
	// Cleanup multiple buffs stored in parameter name fields
	for (i = 0; i < cur_column; i++) {
		if (NULL != parms[i].name)
			free(parms[i].name);
	}
	free(parms);
	return ret;
}
