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

#include "octo.h"
#include "octo_types.h"
#include "message_formats.h"
#include "rocto.h"

#define COPY_TEXT_PARM(DATA_PTR, PARM)                       \
	{                                                    \
		*((int32_t *)DATA_PTR) = htonl(PARM.length); \
		DATA_PTR += sizeof(int32_t);                 \
		if (PSQL_NULL != PARM.length) {              \
			memcpy(c, PARM.value, PARM.length);  \
			DATA_PTR += PARM.length;             \
		}                                            \
	}

DataRow *make_data_row(DataRowParm *parms, int16_t num_parms, int32_t *col_data_types) {
	PSQL_TypeSize type_size;
	int32_t	      length;
	int32_t	      i;
	DataRow	     *ret;
	long	      int4_value;
	char	     *c;
	char	      int_buffer[INT32_TO_STRING_MAX];

	// Get the length we need to malloc
	length = 0;
	if (num_parms != 0) {
		// We dereference this in the loops below; this turns UB into a crash with a backtrace.
		assert(parms != NULL);
	}
	for (i = 0; i < num_parms; i++) {
		// Assign column length for the current column based on column format
		if (TEXT_FORMAT == parms[i].format) { // Text format
			// -1 == PSQL_NULL, so convert to 0 to prevent erroneously reducing length to malloc
			length += ((PSQL_NULL == parms[i].length) ? 0 : parms[i].length);
		} else { // Binary format
			assert(NULL != col_data_types);
			type_size = get_type_size_from_psql_type(col_data_types[i]);
			if (0 > type_size) { // This means a variable type size, so don't convert to fixed size
				// -1 == PSQL_NULL, so convert to 0 to prevent erroneously reducing length to malloc
				length += ((PSQL_NULL == parms[i].length) ? 0 : parms[i].length);
			} else {
				length += type_size;
			}
		}
		length += sizeof(int32_t);
	}

	ret = (DataRow *)malloc(sizeof(DataRow) + length);
	// Include the length of the length field
	length += sizeof(int32_t);
	// Include the length of the num_parms field
	length += sizeof(int16_t);
	ret->type = PSQL_DataRow;
	ret->length = htonl(length);
	ret->num_columns = htons(num_parms);

	c = ret->data;
	if (num_parms != 0) {
		for (i = 0; i < num_parms; i++) {
			if (TEXT_FORMAT == parms[i].format) { // Text format
				COPY_TEXT_PARM(c, parms[i]);
			} else { // Binary format
				assert(NULL != col_data_types);
				switch (col_data_types[i]) {
				case PSQL_TypeOid_int4:
					if (PSQL_NULL == parms[i].length) {
						*((int32_t *)c) = htonl(PSQL_NULL);
						c += sizeof(int32_t);
					} else {
						*((int32_t *)c) = htonl(PSQL_TypeSize_int4);
						c += sizeof(int32_t);
						// Convert parameter value to null-terminated string for conversion into an integer
						memcpy(int_buffer, parms[i].value, parms[i].length);
						int_buffer[parms[i].length] = '\0';
						int4_value = strtol(int_buffer, NULL, 10);
						/* PostgreSQL protocol specifies a 16-bit integer to store the total
						 * number of columns. Details linked in message_formats.h.
						 */
						if (!STRTOL_VALUE_OUT_OF_RANGE(int4_value) && (INT32_MIN <= int4_value)
						    && (INT32_MAX >= int4_value)) {
							*((int32_t *)c) = htonl((int32_t)int4_value);
							c += sizeof(int32_t);
						} else {
							ERROR(ERR_LIBCALL_WITH_ARG, "strtol()", int_buffer);
							free(ret);
							return NULL;
						}
					}
					break;
				case PSQL_TypeOid_numeric:
				case PSQL_TypeOid_unknown:
				case PSQL_TypeOid_varchar:
				case PSQL_TypeOid_date:
				case PSQL_TypeOid_time:
				case PSQL_TypeOid_timetz:
				case PSQL_TypeOid_timestamp:
				case PSQL_TypeOid_timestamptz:
				default:
					COPY_TEXT_PARM(c, parms[i]);
					break;
				}
			}
		}
	}

	return ret;
}
