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

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"

#define INT16_MAX_DIGITS 5
#define INT32_MAX_DIGITS 10
#define INT64_MAX_DIGITS 20

int32_t copy_text_parameter(Bind *bind, const int32_t cur_parm, char *bound_query, int32_t bound_offset) {
	memcpy(&bound_query[bound_offset], bind->parms[cur_parm].value, bind->parms[cur_parm].length);
	bound_offset += bind->parms[cur_parm].length;
	return bound_offset;
}

// TODO: Confirm that this function does what clients expect it to. This has not yet been done as the only clients known to use this
// feature are proprietary and therefore difficult to test.
int32_t copy_binary_parameter(Bind *bind, const int32_t cur_parm, char *bound_query, int32_t bound_offset) {
	int64_t value = 0;
	int32_t copied = 0;

	switch (bind->parms[cur_parm].length) {
		case 1:
			value = bin_to_char(bind->parms[cur_parm].value);
			copied = snprintf(&bound_query[bound_offset], INT64_TO_STRING_MAX, "%lld", (long long int)value);
			break;
		case 2:
			value = bin_to_int16(bind->parms[cur_parm].value);
			copied = snprintf(&bound_query[bound_offset], INT64_TO_STRING_MAX, "%lld", (long long int)value);
			break;
		case 4:
			value = bin_to_int32(bind->parms[cur_parm].value);
			copied = snprintf(&bound_query[bound_offset], INT64_TO_STRING_MAX, "%lld", (long long int)value);
			break;
		case 8:
			// This covers the OID case, as it is just an integer
			value = bin_to_int64(bind->parms[cur_parm].value);
			copied = snprintf(&bound_query[bound_offset], INT64_TO_STRING_MAX, "%lld", (long long int)value);
			break;
		case 16:
			// TODO: Allow this case to fallthrough until binary formats are understood or at least testable
			// assert(sizeof(uuid) >= UUID_CHARACTER_LENGTH);
			// bin_to_uuid(bind->parms[cur_parm].value, uuid);
			// copied = snprintf(&bound_query[bound_offset], UUID_CHARACTER_LENGTH, "%s", uuid);
			// break;
		default:
			// TODO: Assume the data can be used without conversion until more types are supported and binary formats
			// are fully understood
			memcpy(&bound_query[bound_offset], bind->parms[cur_parm].value, bind->parms[cur_parm].length);
			copied = bind->parms[cur_parm].length;
			break;
	}
	return bound_offset + copied;
}

int32_t get_binary_parameter_length(Bind *bind, const int32_t cur_parm) {
	int64_t value = 0;
	int32_t copied = 0;
	char buffer[MAX_STR_CONST];

	switch (bind->parms[cur_parm].length) {
		case 1:
			value = bin_to_char(bind->parms[cur_parm].value);
			copied = snprintf(buffer, INT64_TO_STRING_MAX, "%lld", (long long int)value);
			break;
		case 2:
			value = bin_to_int16(bind->parms[cur_parm].value);
			copied = snprintf(buffer, INT64_TO_STRING_MAX, "%lld", (long long int)value);
			break;
		case 4:
			value = bin_to_int32(bind->parms[cur_parm].value);
			copied = snprintf(buffer, INT64_TO_STRING_MAX, "%lld", (long long int)value);
			break;
		case 8:
			// This covers the OID case, as it is just an integer
			value = bin_to_int64(bind->parms[cur_parm].value);
			copied = snprintf(buffer, INT64_TO_STRING_MAX, "%lld", (long long int)value);
			break;
		case 16:
			// TODO: Allow this case to fallthrough until binary formats are understood or at least testable
			// assert(sizeof(buffer) >= UUID_CHARACTER_LENGTH);
			// bin_to_uuid(bind->parms[cur_parm].value, uuid);
			// copied = snprintf(buffer, UUID_CHARACTER_LENGTH, "%s", uuid);
			// break;
		default:
			// TODO: Assume the data can be used without conversion until more types are supported and binary formats
			// are fully understood
			copied = bind->parms[cur_parm].length;
			break;
	}
	return copied;
}
