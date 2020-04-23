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

#include <assert.h>

#include "octo.h"
#include "errors.h"

void ydb_error_check(int status, char *file, int line)
{
	ydb_buffer_t	varname, ret_value;
	int		severity = 0, ydboctoerrcodemin, ydboctoerrcodemax, ydboctoerrcode;
	unsigned int	ydb_data_ret_value;
	boolean_t	is_octo_internal_error;

	/* First check for known YDB error codes that do not populate $ZSTATUS */
	switch(status) {
	case YDB_OK:
		return;
	case YDB_TP_RESTART:
	case YDB_TP_ROLLBACK:
	case YDB_NOTOK:
		assert(FALSE);
		return;
		break;
	case YDB_LOCK_TIMEOUT:
		octo_log(line, file, ERROR, ERR_YOTTADB, "ydb_lock_s()/ydb_lock_incr_s() call timed out. "
				"Another process with schema change rights, or a long-running query, is active.");
		return;
	default:
		/* It is an Octo internal error code or a YDB error code that populates $ZSTATUS (i.e. YDB_ERR_*).
		 * Fall through to code that handles these two cases.
		 */
		break;
	}
	/* Check if the error code returned is an Octo-internal error code.
	 *	(i.e. %ydboctoerrcodemin < error-code < %ydboctoerrcodemax).
	 * If so handle that separately. Else treat it as a YDB error code.
	 */
	YDB_MALLOC_BUFFER(&ret_value, MAX_STR_CONST);
	YDB_LITERAL_TO_BUFFER("%ydboctoerrcodemin", &varname);
	/* It is possible we got a ZLINKFILE error in the `ydb_ci()` call done in `octo_init.c` due to `ydb_routines`
	 * not being properly set up. In that case, `%ydboctoerrcodemin` and `%ydboctoerrcodemax` will not be properly
	 * set up either. So avoid doing a `ydb_get_s()` on them as if that fails $ZSTATUS would be overwritten and
	 * we will lose the primary ZLINKFILE error. Do a `ydb_data_s()` first to see if the min/max variables are
	 * defined. If so, one can safely do a `ydb_get_s()`. If not skip that part and assume it is a YDB error.
	 */
	ydb_data_s(&varname, 0, NULL, &ydb_data_ret_value);
	if (ydb_data_ret_value) {
		ydb_get_s(&varname, 0, NULL, &ret_value);
		ret_value.buf_addr[ret_value.len_used] = '\0';
		ydboctoerrcodemin = atoi(ret_value.buf_addr);
		YDB_LITERAL_TO_BUFFER("%ydboctoerrcodemax", &varname);
		ydb_get_s(&varname, 0, NULL, &ret_value);
		ret_value.buf_addr[ret_value.len_used] = '\0';
		ydboctoerrcodemax = atoi(ret_value.buf_addr);
		is_octo_internal_error = ((ydboctoerrcodemin < status) && (status < ydboctoerrcodemax));
	} else {
		/* Treat this as a case of a YDB error */
		is_octo_internal_error = FALSE;
	}
	if (is_octo_internal_error) {
		/* Check for each possible Octo internal error (full list can be seen in src/aux/_ydboctoInit.m).
		 * Note: The below logic does ++ just like "$increment" is done in src/aux/_ydboctoInit.m
		 *       Any changes here will most likely need to also be done in src/aux/_ydboctoInit.m
		 */
		ydboctoerrcode = ydboctoerrcodemin;
		/* Check if %ydboctoerror("SUBQUERYMULTIPLEROWS")	*/
		ydboctoerrcode++;
		if (status == ydboctoerrcode) {
			octo_log(line, file, ERROR, ERR_SUBQUERY_MULTIPLE_ROWS, NULL);
		}
		/* Check if %ydboctoerror("INVALIDINPUTSYNTAXBOOL")	*/
		ydboctoerrcode++;
		if (status == ydboctoerrcode) {
			ydb_buffer_t	subs[2];

			/* M code would have passed the actual string involved in an M node. Get that before printing error. */
			YDB_LITERAL_TO_BUFFER("%ydboctoerror", &varname);
			YDB_LITERAL_TO_BUFFER("INVALIDINPUTSYNTAXBOOL", &subs[0]);
			YDB_LITERAL_TO_BUFFER("1", &subs[1]);
			ydb_get_s(&varname, 2, subs, &ret_value);
			ret_value.buf_addr[ret_value.len_used] = '\0';
			octo_log(line, file, ERROR, ERR_INVALID_INPUT_SYNTAX_BOOL, ret_value.buf_addr);
			/* Now that we have got the value, delete the M node */
			ydb_delete_s(&varname, 2, subs, YDB_DEL_NODE);
		}
		ydboctoerrcode++;
		assert(ydboctoerrcode == ydboctoerrcodemax);
		/* Clear "$ECODE" now that we have handled the Octo-internal error.
		 * Otherwise "ydb_etrap" would be invoked at a later point in time.
		 */
		YDB_LITERAL_TO_BUFFER("$ECODE", &varname);
		ydb_set_s(&varname, 0, NULL, NULL);	/* M equivalent is : SET $ECODE="" */
	} else {
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
