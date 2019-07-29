/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"
#include "helpers.h"


RowDescription *get_plan_row_description(ydb_buffer_t *plan_filename) {
	RowDescription *ret;
	RowDescriptionParm *parms;
	ydb_buffer_t *plan_meta, value_buffer;
	int status;
	char *buff;

	plan_meta = make_buffers(config->global_names.octo, 5, "plan_metadata", "", "output_columns", "", "");
	plan_meta[2] = *plan_filename;
	YDB_MALLOC_BUFFER(&plan_meta[4], MAX_STR_CONST);
	YDB_MALLOC_BUFFER(&plan_meta[5], MAX_STR_CONST);
	YDB_MALLOC_BUFFER(&value_buffer, MAX_STR_CONST);

	int num_columns = 0;
	while(TRUE) {
		status = ydb_subscript_next_s(plan_meta, 4, &plan_meta[1], &plan_meta[4]);
		if(status == YDB_ERR_NODEEND) {
			break;
		}
		YDB_ERROR_CHECK(status);
		num_columns++;
	}

	parms = malloc(sizeof(RowDescriptionParm) * num_columns);
	memset(parms, 0, sizeof(RowDescriptionParm) * num_columns);

	int i = 0;
	plan_meta[4].len_used = 0;
	while(TRUE) {
		status = ydb_subscript_next_s(plan_meta, 4, &plan_meta[1], &plan_meta[4]);
		if(status == YDB_ERR_NODEEND) {
			break;
		}
		YDB_ERROR_CHECK(status);
		YDB_LITERAL_TO_BUFFER("name", &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		buff = malloc(value_buffer.len_used + 1);
		memcpy(buff, value_buffer.buf_addr, value_buffer.len_used);
		buff[value_buffer.len_used] = '\0';
		parms[i].name = buff;

		YDB_LITERAL_TO_BUFFER("table_id", &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		parms[i].table_id = atoi(value_buffer.buf_addr);

		YDB_LITERAL_TO_BUFFER("column_id", &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		parms[i].column_id = atoi(value_buffer.buf_addr);

		YDB_LITERAL_TO_BUFFER("data_type", &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		parms[i].data_type = atoi(value_buffer.buf_addr);

		YDB_LITERAL_TO_BUFFER("data_type_size", &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		parms[i].data_type_size = atoi(value_buffer.buf_addr);

		YDB_LITERAL_TO_BUFFER("type_modifier", &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		parms[i].type_modifier = atoi(value_buffer.buf_addr);

		YDB_LITERAL_TO_BUFFER("format_code", &plan_meta[5]);
		status = ydb_get_s(plan_meta, 5, &plan_meta[1], &value_buffer);
		YDB_ERROR_CHECK(status);
		value_buffer.buf_addr[value_buffer.len_used] = '\0';
		parms[i].format_code = atoi(value_buffer.buf_addr);
		i++;
	}

	ret = make_row_description(parms, num_columns);
	free(parms);
	return ret;
}
