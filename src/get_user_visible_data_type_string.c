/****************************************************************
 *								*
 * Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

/* Returns the string representation for the data type passed in as "data_type_ptr" in the output buffer "ret_buff"
 * which has an allocation size of "ret_buff_size". An example string representation would be "VARCHAR(20)".
 * Returns
 *   -1 if "ret_buff_size" is not enough to hold the entire string representation (fills it as much as possible in that case).
 *    0 otherwise (i.e. normal/success return).
 */
int get_user_visible_data_type_string(SqlDataTypeStruct *data_type_ptr, char *ret_buff, int ret_buff_size) {
	char *ptr;
	int   len, avail;

	ptr = ret_buff;
	avail = ret_buff_size;
	switch (data_type_ptr->data_type) {
	case BOOLEAN_TYPE:
		/* For BOOLEAN, neither PRECISION nor SCALE apply. Assert that. */
		assert(SIZE_OR_PRECISION_UNSPECIFIED == data_type_ptr->size_or_precision);
		assert(SCALE_UNSPECIFIED == data_type_ptr->scale);
		len = snprintf(ptr, avail, "%s", "BOOLEAN");
		if ((0 > len) || (len >= avail)) {
			assert(FALSE);
			return -1;
		}
		break;
	case INTEGER_TYPE:
		/* For INTEGER, only PRECISION may apply. Assert that. */
		assert(SCALE_UNSPECIFIED == data_type_ptr->scale);
		len = snprintf(ptr, avail, "%s", "INTEGER");
		if ((0 > len) || (len >= avail)) {
			assert(FALSE);
			return -1;
		}
		if (SIZE_OR_PRECISION_UNSPECIFIED != data_type_ptr->size_or_precision) {
			ptr += len;
			avail -= len;
			/* SIZE was specified (e.g. INTEGER(8)). In that case, write out the "8" here */
			len = snprintf(ptr, avail, "(%d)", data_type_ptr->size_or_precision);
			if ((0 > len) || (len >= avail)) {
				assert(FALSE);
				return -1;
			}
		}
		break;
	case NUMERIC_TYPE:
		/* For NUMERIC, both PRECISION and SCALE may apply. Check both. */
		len = snprintf(ptr, avail, "%s", "NUMERIC");
		if ((0 > len) || (len >= avail)) {
			assert(FALSE);
			return -1;
		}
		if (SIZE_OR_PRECISION_UNSPECIFIED != data_type_ptr->size_or_precision) {
			ptr += len;
			avail -= len;
			if (SCALE_UNSPECIFIED != data_type_ptr->scale) {
				/* PRECISION and SCALE were both specified (e.g. NUMERIC(8,4)).
				 * In that case, write out the "(8,4)" here.
				 */
				len = snprintf(ptr, avail, "(%d,%d)", data_type_ptr->size_or_precision, data_type_ptr->scale);
			} else {
				/* Only PRECISION was specified (e.g. NUMERIC(8)).
				 * In that case, write out the "(8)" here.
				 */
				len = snprintf(ptr, avail, "(%d)", data_type_ptr->size_or_precision);
			}
			if ((0 > len) || (len >= avail)) {
				assert(FALSE);
				return -1;
			}
		} else {
			assert(SCALE_UNSPECIFIED == data_type_ptr->scale);
			/* Neither PRECISION nor SCALE were specified. No need to write anything more. */
		}
		break;
	case STRING_TYPE:
		/* For STRING, only SIZE may apply. Assert that. */
		assert(SCALE_UNSPECIFIED == data_type_ptr->scale);
		len = snprintf(ptr, avail, "%s", "VARCHAR");
		if ((0 > len) || (len >= avail)) {
			assert(FALSE);
			return -1;
		}
		if (SIZE_OR_PRECISION_UNSPECIFIED != data_type_ptr->size_or_precision) {
			ptr += len;
			avail -= len;
			/* SIZE was specified (e.g. VARCHAR(30)). In that case, write out the "30" here */
			len = snprintf(ptr, avail, "(%d)", data_type_ptr->size_or_precision);
			if ((0 > len) || (len >= avail)) {
				assert(FALSE);
				return -1;
			}
		}
		break;
	default:
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		assert(FALSE);
		return -1;
		break;
	}
	return 0;
}
