/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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

ParameterDescription *make_parameter_description(char *statement, RoctoSession *session) {
	ParameterDescription *ret;
	int32_t		      status = 0, *parm_data_types;
	int16_t		      num_parms = 0, cur_parm_type = 0, cur_parm_type_temp;
	long int	      test_type = 0, num_parms_from_str = 0;
	ydb_buffer_t *	      src_subs;
	ydb_buffer_t	      num_parms_buf, parm_type_buf;

	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&num_parms_buf, INT16_TO_STRING_MAX);
	src_subs = make_buffers(config->global_names.session, 6, session->session_id->buf_addr, OCTOLIT_PREPARED, statement,
				OCTOLIT_PARAMETERS, "", "type");
	status = ydb_get_s(&src_subs[0], 4, &src_subs[1], &num_parms_buf);
	if (YDB_OK != status) {
		ERROR(ERR_ROCTO_DB_LOOKUP, "make_parameter_description", "number of prepared statement parameters");
		YDB_FREE_BUFFER(&num_parms_buf);
		free(src_subs);
		return NULL;
	}
	num_parms_buf.buf_addr[num_parms_buf.len_used] = '\0';
	test_type = strtol(num_parms_buf.buf_addr, NULL, 10);
	YDB_FREE_BUFFER(&num_parms_buf);
	if ((0 <= test_type) || (INT16_MIN <= test_type) || (INT16_MAX >= test_type)) {
		num_parms = (int16_t)test_type;
	} else {
		ERROR(ERR_LIBCALL, "strtol")
		free(src_subs);
		return NULL;
	}

	if (0 == num_parms) {
		ret = (ParameterDescription *)malloc(sizeof(ParameterDescription));
		ret->type = PSQL_ParameterDescription;
		ret->length = htonl(sizeof(int32_t) + sizeof(int16_t) + (sizeof(int32_t) * num_parms));
		ret->num_parms = htons(num_parms);
		free(src_subs);
		return ret;
	}
	assert(0 < num_parms);
	ret = (ParameterDescription *)malloc(sizeof(ParameterDescription) + sizeof(int32_t) * num_parms);
	ret->type = PSQL_ParameterDescription;
	ret->length = htonl(sizeof(int32_t) + sizeof(int16_t) + (sizeof(int32_t) * num_parms));
	ret->num_parms = htons(num_parms);

	// Loop over each parameter using ydb_subscript_next to retrieve type values and add to ParameterDescription
	parm_data_types = (int32_t *)malloc(num_parms * sizeof(int32_t));
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&parm_type_buf, INT16_TO_STRING_MAX);
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&src_subs[5], INT16_TO_STRING_MAX);
	for (cur_parm_type = 0; cur_parm_type < num_parms; cur_parm_type++) {
		cur_parm_type_temp = cur_parm_type + 1; // Convert from 0-indexed to 1-indexed
		OCTO_INT16_TO_BUFFER(cur_parm_type_temp, &src_subs[5]);
		status = ydb_get_s(&src_subs[0], 6, &src_subs[1], &parm_type_buf);
		if (YDB_OK != status) {
			ERROR(ERR_ROCTO_DB_LOOKUP, "make_parameter_description", "parameter type");
			YDB_FREE_BUFFER(&parm_type_buf);
			YDB_FREE_BUFFER(&src_subs[5]);
			free(parm_data_types);
			free(src_subs);
			free(ret);
			return NULL;
		}
		parm_type_buf.buf_addr[parm_type_buf.len_used] = '\0';
		num_parms_from_str = strtol(parm_type_buf.buf_addr, NULL, 10);
		if ((ERANGE != errno) && (0 <= num_parms_from_str) && (INT32_MAX >= num_parms_from_str)) {
			((int32_t *)ret->data)[cur_parm_type] = htonl((int32_t)(num_parms_from_str));
		} else {
			ERROR(ERR_LIBCALL, "strtol")
			YDB_FREE_BUFFER(&parm_type_buf);
			YDB_FREE_BUFFER(&src_subs[5]);
			free(parm_data_types);
			free(src_subs);
			free(ret);
			return NULL;
		}
	}
	YDB_FREE_BUFFER(&parm_type_buf);
	YDB_FREE_BUFFER(&src_subs[5]);
	free(parm_data_types);
	free(src_subs);

	return ret;
}
