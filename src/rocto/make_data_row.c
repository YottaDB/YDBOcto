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

#define COPY_TEXT_PARM(DATA_PTR, PARM)                        \
	{                                                     \
		*((uint32_t *)DATA_PTR) = htonl(PARM.length); \
		DATA_PTR += sizeof(uint32_t);                 \
		memcpy(c, PARM.value, PARM.length);           \
		DATA_PTR += PARM.length;                      \
	}

DataRow *make_data_row(DataRowParm *parms, int16_t num_parms, int32_t *col_data_types) {
	PSQL_TypeSize type_size;
	uint32_t      length;
	int32_t	      i;
	DataRow *     ret;
	long	      int4_value;
	char *	      c;
	char	      int_buffer[INT32_TO_STRING_MAX];

	// Get the length we need to malloc
	length = 0;
	for (i = 0; i < num_parms; i++) {
		// Assign column length for the current column based on column format
		if (0 == parms[i].format) { // Text format
			length += parms[i].length;
		} else { // Binary format
			assert(NULL != col_data_types);
			type_size = get_type_size_from_psql_type(col_data_types[i]);
			if (0 > type_size) // This means a variable type size, so don't convert to fixed size
				length += parms[i].length;
			else
				length += type_size;
		}
		length += sizeof(uint32_t);
	}

	ret = (DataRow *)malloc(sizeof(DataRow) + length);
	// Include the length of the length field
	length += sizeof(uint32_t);
	// Include the length of the num_parms field
	length += sizeof(int16_t);
	ret->type = PSQL_DataRow;
	ret->length = htonl(length);
	ret->num_columns = htons(num_parms);

	c = ret->data;
	if (num_parms == 0 || parms == NULL) {
		*((uint32_t *)c) = htonl(0);
	} else {
		for (i = 0; i < num_parms; i++) {
			if (TEXT_FORMAT == parms[i].format) { // Text format
				COPY_TEXT_PARM(c, parms[i]);
			} else { // Binary format
				assert(NULL != col_data_types);
				switch (col_data_types[i]) {
				case PSQL_TypeOid_int4:
					*((uint32_t *)c) = htonl(PSQL_TypeSize_int4);
					c += sizeof(uint32_t);
					// Convert parameter value to null-terminated string for conversion into an integer
					memcpy(int_buffer, parms[i].value, parms[i].length);
					int_buffer[parms[i].length] = '\0';
					int4_value = strtol(int_buffer, NULL, 10);
					// PostgreSQL protocol specifies a 16-bit integer to store the total number of columns
					// Details linked in message_formats.h
					if ((ERANGE != errno) && (INT32_MIN <= int4_value) && (INT32_MAX >= int4_value)) {
						*((int32_t *)c) = htonl((int32_t)int4_value);
						c += sizeof(int32_t);
					} else {
						ERROR(ERR_LIBCALL_WITH_ARG, "strtol", int_buffer);
						free(ret);
						return NULL;
					}
					break;
				case PSQL_TypeOid_numeric:
				case PSQL_TypeOid_unknown:
				case PSQL_TypeOid_varchar:
				default:
					COPY_TEXT_PARM(c, parms[i]);
					break;
				}
			}
		}
	}

	return ret;
}
