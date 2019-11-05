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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"

#define INT16_MAX_DIGITS 5
#define INT32_MAX_DIGITS 10
#define INT64_MAX_DIGITS 20

char *copy_text_parameter(RoctoSession *session, Bind *bind, const int32_t cur_parm, char *query, const char *end_query) {
	char *text_parm_start, *text_parm_end;

	// Currently unused parameters
	UNUSED(session);

	// Copy text value
	text_parm_start = (char*)bind->parms[cur_parm].value;
	text_parm_end = text_parm_start + bind->parms[cur_parm].length;
	while(text_parm_start < text_parm_end) {
		*query = *text_parm_start;
		query++;
		text_parm_start++;
		// We need to leave an extra place for the closing quote, hence
		//  the +1
		if(query + 1 >= end_query) {
			ERROR(ERR_ROCTO_QUERY_TOO_LONG, "");
			return NULL;
		}
	}
	return query;
}

char *copy_binary_parameter(RoctoSession *session, Bind *bind, const int32_t cur_parm, char *query, const char *end_query) {
	// Currently unused parameters
	UNUSED(session);

	char str_value[MAX_STR_CONST];
	int64_t value = 0;
	int32_t copied = 0;
	char uuid[MAX_STR_CONST];

	switch (bind->parms[cur_parm].length) {
		case 1:
			value = bin_to_char(bind->parms[cur_parm].value);
			copied = snprintf(str_value, MAX_STR_CONST, "%lld", (long long int)value);
			break;
		case 2:
			value = bin_to_int16(bind->parms[cur_parm].value);
			copied = snprintf(str_value, MAX_STR_CONST, "%lld", (long long int)value);
			break;
		case 4:
			value = bin_to_int32(bind->parms[cur_parm].value);
			copied = snprintf(str_value, MAX_STR_CONST, "%lld", (long long int)value);
			break;
		case 8:
			// This covers the OID case, as it is just an integer
			value = bin_to_int64(bind->parms[cur_parm].value);
			copied = snprintf(str_value, MAX_STR_CONST, "%lld", (long long int)value);
			break;
		case 16:
			bin_to_uuid(bind->parms[cur_parm].value, uuid, MAX_STR_CONST);
			copied = snprintf(str_value, MAX_STR_CONST, "%s", uuid);
			break;
		default:
			ERROR(ERR_ROCTO_UNSUPPORTED_BIND_PARAMETER, PSQL_Code_Syntax_Error, "unsupported bind parameter type received", 0);
			return NULL;
			break;
	}
	if ((0 > copied) || ((query + copied) > end_query)) {
		ERROR(ERR_ROCTO_BIND_PARAMETER_DECODE_FAILURE, PSQL_Code_Syntax_Error, "failed to decode binary bind parameter", 0);
		return NULL;
	}
	strncpy(query, str_value, copied);
	query += copied;
	return query;
}
