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

#include <assert.h>

#include "octo.h"
#include "errors.h"

void ydb_error_check(int status, char *file, int line)
{
	ydb_buffer_t	varname, ret_value;
	int		severity = 0, ydboctoerrcodemin, ydboctoerrcodemax, ydboctoerrcode;

	if (YDB_OK != status) {
		/* Check if the error code returned is an Octo-specific error code.
		 *	(i.e. %ydboctoerrcodemin < error-code < %ydboctoerrcodemax).
		 * If so handle that separately. Else treat it as a YDB error code.
		 */
		YDB_MALLOC_BUFFER(&ret_value, MAX_STR_CONST);
		YDB_LITERAL_TO_BUFFER("%ydboctoerrcodemin", &varname);
		ydb_get_s(&varname, 0, NULL, &ret_value);
		ret_value.buf_addr[ret_value.len_used] = '\0';
		ydboctoerrcodemin = atoi(ret_value.buf_addr);
		YDB_LITERAL_TO_BUFFER("%ydboctoerrcodemax", &varname);
		ydb_get_s(&varname, 0, NULL, &ret_value);
		ret_value.buf_addr[ret_value.len_used] = '\0';
		ydboctoerrcodemax = atoi(ret_value.buf_addr);
		if ((ydboctoerrcodemin < status) && (status < ydboctoerrcodemax)) {
			/* Note: The below logic does ++ just like "$incr" is done in src/aux/_ydboctoInit.m
			 *       Any changes here will most likely need to also be done in src/aux/_ydboctoInit.m
			 */
			ydboctoerrcode = ydboctoerrcodemin;
			/* Check if %ydboctoerror("SUBQUERYMULTIPLEROWS")	*/
			ydboctoerrcode++;
			if (status == ydboctoerrcode) {
				octo_log(line, file, ERROR, ERR_SUBQUERY_MULTIPLE_ROWS, NULL);
			}
			ydboctoerrcode++;
			assert(ydboctoerrcode == ydboctoerrcodemax);
			/* Clear "$ECODE" now that we have handled the Octo-internal error.
			 * Otherwise "ydb_etrap" would be invoked at a later point in time.
			 */
			YDB_LITERAL_TO_BUFFER("$ECODE", &varname);
			ydb_set_s(&varname, 0, NULL, NULL);	/* M equivalent is : SET $ECODE="" */
		} else {
			ydb_get_s(&varname, 0, NULL, &ret_value);
			YDB_LITERAL_TO_BUFFER("$ZSTATUS", &varname);
			ydb_get_s(&varname, 0, NULL, &ret_value);
			ret_value.buf_addr[ret_value.len_used] = '\0';
			YDB_SEVERITY(status, severity);
			switch (severity) {
				case YDB_SEVERITY_SUCCESS:
					octo_log(line, file, TRACE, ERR_YOTTADB, ret_value.buf_addr);
					status = YDB_OK;
					break;
				case YDB_SEVERITY_INFORMATIONAL:
					octo_log(line, file, INFO, ERR_YOTTADB, ret_value.buf_addr);
					status = YDB_OK;
					break;
				case YDB_SEVERITY_WARNING:
					octo_log(line, file, WARNING, ERR_YOTTADB, ret_value.buf_addr);
					break;
				case YDB_SEVERITY_ERROR:
					octo_log(line, file, ERROR, ERR_YOTTADB, ret_value.buf_addr);
					break;
				case YDB_SEVERITY_FATAL:
					octo_log(line, file, FATAL, ERR_YOTTADB, ret_value.buf_addr);
					break;
				default:
					status = YDB_OK;
					break;
			}
		}
		YDB_FREE_BUFFER(&ret_value);
	}
}
