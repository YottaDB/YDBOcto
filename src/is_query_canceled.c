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

#include <assert.h>

#include <libyottadb.h>

#include "octo.h"

boolean_t is_query_canceled(callback_fnptr_t callback) {
	ydb_buffer_t ydboctoCancel;
	unsigned int cancel_result = 0;
	int	     status = 0;

	// Check if execution was interrupted by a CancelRequest by checking local variable
	YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOCANCEL, &ydboctoCancel);
	status = ydb_data_s(&ydboctoCancel, 0, NULL, &cancel_result);
	YDB_ERROR_CHECK(status);
	if (0 != cancel_result) {
		// Omit results after handling CancelRequest
		/* Note: All parameters to "*callback()" (which is basically the "handle_query_response()" function
		 * that is hidden inside a function pointer due to static linking issues in the "octo" executable against
		 * a function defined in "librocto.so") except for the 1st are unused so pass dummy values.
		 */
		status = (*callback)(NULL, 0, NULL, NULL, FALSE);
		if (0 != status) {
			// This should never happen
			assert(0 == status);
			FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
			return TRUE;
		}
		/* Now that the CancelRequest has been handled, delete related local variable so next
		 * query can proceed fine without being treated as a canceled query.
		 */
		status = ydb_delete_s(&ydboctoCancel, 0, NULL, YDB_DEL_TREE);
		assert(YDB_OK == status);
		YDB_ERROR_CHECK(status);
		return TRUE;
	}
	return FALSE;
}
