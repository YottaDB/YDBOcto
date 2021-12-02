/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
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

void ydb_error_check(int status, char *file, int line) {
	ydb_buffer_t varname, ret_value;
	int	     severity = 0, ydboctoerrcodemin, ydboctoerrcodemax, ydboctoerrcode, positive_status;
	unsigned int ydb_data_ret_value;
	boolean_t    is_octo_internal_error;

	/* First check for known YDB error codes that do not populate $ZSTATUS */
	switch (status) {
	case YDB_OK:
		return;
	case YDB_TP_RESTART:
	case YDB_TP_ROLLBACK:
	case YDB_NOTOK:
		assert(FALSE);
		return;
		break;
	case YDB_LOCK_TIMEOUT:
		octo_log(line, file, ERROR, ERROR_Severity, ERR_YOTTADB,
			 "ydb_lock_s()/ydb_lock_incr_s() call timed out likely due to a concurrent long-running query.");
		return;
	default:
		/* It is an Octo internal error code or a YDB error code that populates $ZSTATUS (i.e. YDB_ERR_*).
		 * Fall through to code that handles these two cases.
		 */
		break;
	}
	/* "ydb_ci()"/"ydb_cip()" would return a negated error code. So turn it back to a positive error code before
	 * comparing it against known error codes.
	 */
	assert(0 > status);
	positive_status = -status;
	/* Check if the error code returned is an Octo-internal error code.
	 *	(i.e. %ydboctoerrcodemin < error-code < %ydboctoerrcodemax).
	 * If so handle that separately. Else treat it as a YDB error code.
	 */
	OCTO_MALLOC_NULL_TERMINATED_BUFFER(&ret_value, YDB_MAX_ERRORMSG);
	YDB_LITERAL_TO_BUFFER("%ydboctoerrcodemin", &varname);
	/* It is possible we got a ZLINKFILE error in the `ydb_ci()` call done in `octo_init.c` due to `ydb_routines`
	 * not being properly set up. In that case, `%ydboctoerrcodemin` and `%ydboctoerrcodemax` will not be properly
	 * set up either. So avoid doing a `ydb_get_s()` on them as if that fails $ZSTATUS would be overwritten and
	 * we will lose the primary ZLINKFILE error. Do a `ydb_data_s()` first to see if the min/max variables are
	 * defined. If so, one can safely do a `ydb_get_s()`. If not skip that part and assume it is a YDB error.
	 */
	ydb_data_s(&varname, 0, NULL, &ydb_data_ret_value);
	if (ydb_data_ret_value) {
		status = ydb_get_s(&varname, 0, NULL, &ret_value);
		if (YDB_ERR_INVSTRLEN == status) {
			EXPAND_YDB_BUFFER_T_ALLOCATION(ret_value);
			status = ydb_get_s(&varname, 0, NULL, &ret_value);
			assert(YDB_OK == status);
			UNUSED(status); // Prevent 'value never read' compiler warning
		}
		ret_value.buf_addr[ret_value.len_used] = '\0';
		ydboctoerrcodemin = atoi(ret_value.buf_addr);
		YDB_LITERAL_TO_BUFFER("%ydboctoerrcodemax", &varname);
		status = ydb_get_s(&varname, 0, NULL, &ret_value);
		if (YDB_ERR_INVSTRLEN == status) {
			EXPAND_YDB_BUFFER_T_ALLOCATION(ret_value);
			status = ydb_get_s(&varname, 0, NULL, &ret_value);
			assert(YDB_OK == status);
			UNUSED(status); // Prevent 'value never read' compiler warning
		}
		ret_value.buf_addr[ret_value.len_used] = '\0';
		ydboctoerrcodemax = atoi(ret_value.buf_addr);
		is_octo_internal_error = ((ydboctoerrcodemin < positive_status) && (positive_status < ydboctoerrcodemax));
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
		if (positive_status == ydboctoerrcode) {
			octo_log(line, file, ERROR, ERROR_Severity, ERR_SUBQUERY_MULTIPLE_ROWS, NULL);
		}
		/* Check if %ydboctoerror("INVALIDINPUTSYNTAXBOOL")	*/
		ydboctoerrcode++;
		if (positive_status == ydboctoerrcode) {
			ydb_buffer_t subs[2];

			/* M code would have passed the actual string involved in an M node. Get that before printing error. */
			YDB_LITERAL_TO_BUFFER("%ydboctoerror", &varname);
			YDB_LITERAL_TO_BUFFER("INVALIDINPUTSYNTAXBOOL", &subs[0]);
			YDB_LITERAL_TO_BUFFER("1", &subs[1]);
			status = ydb_get_s(&varname, 2, subs, &ret_value);
			if (YDB_ERR_INVSTRLEN == status) {
				EXPAND_YDB_BUFFER_T_ALLOCATION(ret_value);
				status = ydb_get_s(&varname, 2, subs, &ret_value);
				assert(YDB_OK == status);
				UNUSED(status); // Prevent 'value never read' compiler warning
			}
			ret_value.buf_addr[ret_value.len_used] = '\0';
			octo_log(line, file, ERROR, ERROR_Severity, ERR_INVALID_INPUT_SYNTAX_BOOL, ret_value.buf_addr);
			/* Now that we have got the value, delete the M node */
			ydb_delete_s(&varname, 2, subs, YDB_DEL_NODE);
		}
		/* Check if %ydboctoerror("INVALIDESCAPEPATTERN")	*/
		ydboctoerrcode++;
		if (positive_status == ydboctoerrcode) {
			ydb_buffer_t subs[2];

			/* M code would have passed the actual string involved in an M node. Get that before printing error. */
			YDB_LITERAL_TO_BUFFER("%ydboctoerror", &varname);
			YDB_LITERAL_TO_BUFFER("INVALIDESCAPEPATTERN", &subs[0]);
			YDB_LITERAL_TO_BUFFER("1", &subs[1]);
			status = ydb_get_s(&varname, 2, subs, &ret_value);
			if (YDB_ERR_INVSTRLEN == status) {
				EXPAND_YDB_BUFFER_T_ALLOCATION(ret_value);
				status = ydb_get_s(&varname, 2, subs, &ret_value);
				assert(YDB_OK == status);
				UNUSED(status); // Prevent 'value never read' compiler warning
			}
			ret_value.buf_addr[ret_value.len_used] = '\0';
			octo_log(line, file, ERROR, ERROR_Severity, ERR_INVALID_ESCAPE_PATTERN, ret_value.buf_addr);
			/* Now that we have got the value, delete the M node */
			ydb_delete_s(&varname, 2, subs, YDB_DEL_NODE);
		}
		/* Check if %ydboctoerror("NUMERICOVERFLOW")	*/
		ydboctoerrcode++;
		if (positive_status == ydboctoerrcode) {
			ydb_buffer_t subs[2], ret_buff;
			char precision_buff[INT64_TO_STRING_MAX], scale_buff[INT64_TO_STRING_MAX], value_buff[INT64_TO_STRING_MAX];

			/* M code would have passed the parameters for the error message in M nodes.
			 * Get that before printing error.
			 */
			YDB_LITERAL_TO_BUFFER("%ydboctoerror", &varname);
			YDB_LITERAL_TO_BUFFER("NUMERICOVERFLOW", &subs[0]);
			/* Get precision */
			YDB_LITERAL_TO_BUFFER("1", &subs[1]);
			ret_buff.len_alloc = sizeof(precision_buff) - 1; /* Leave 1 byte for null terminator */
			ret_buff.buf_addr = precision_buff;
			status = ydb_get_s(&varname, 2, subs, &ret_buff);
			assert(YDB_OK == status);
			UNUSED(status); /* needed to avoid a [clang-analyzer-deadcode.DeadStores] warning */
			ret_buff.buf_addr[ret_buff.len_used] = '\0';
			ydb_delete_s(&varname, 2, subs, YDB_DEL_NODE); /* Now that we have got the value, delete the M node */
			/* Get scale */
			YDB_LITERAL_TO_BUFFER("2", &subs[1]);
			ret_buff.len_alloc = sizeof(scale_buff) - 1; /* Leave 1 byte for null terminator */
			ret_buff.buf_addr = scale_buff;
			status = ydb_get_s(&varname, 2, subs, &ret_buff);
			assert(YDB_OK == status);
			UNUSED(status); /* needed to avoid a [clang-analyzer-deadcode.DeadStores] warning */
			ret_buff.buf_addr[ret_buff.len_used] = '\0';
			ydb_delete_s(&varname, 2, subs, YDB_DEL_NODE); /* Now that we have got the value, delete the M node */
			/* Get value */
			YDB_LITERAL_TO_BUFFER("3", &subs[1]);
			ret_buff.len_alloc = sizeof(value_buff) - 1; /* Leave 1 byte for null terminator */
			ret_buff.buf_addr = value_buff;
			status = ydb_get_s(&varname, 2, subs, &ret_buff);
			assert(YDB_OK == status);
			UNUSED(status); /* needed to avoid a [clang-analyzer-deadcode.DeadStores] warning */
			ret_buff.buf_addr[ret_buff.len_used] = '\0';
			ydb_delete_s(&varname, 2, subs, YDB_DEL_NODE); /* Now that we have got the value, delete the M node */
			/* Issue error */
			octo_log(line, file, ERROR, ERROR_Severity, ERR_NUMERIC_OVERFLOW, precision_buff, scale_buff, value_buff);
		}
		/* Check if %ydboctoerror("VARCHARTOOLONG")	*/
		ydboctoerrcode++;
		if (positive_status == ydboctoerrcode) {
			ydb_buffer_t subs[2];

			/* M code would have passed the actual string involved in an M node. Get that before printing error. */
			YDB_LITERAL_TO_BUFFER("%ydboctoerror", &varname);
			YDB_LITERAL_TO_BUFFER("VARCHARTOOLONG", &subs[0]);
			YDB_LITERAL_TO_BUFFER("1", &subs[1]);
			status = ydb_get_s(&varname, 2, subs, &ret_value);
			/* No possibility of YDB_ERR_INVSTRLEN because the return is an integer number ("size")
			 * and so will occupy only less than a dozen bytes/characters in "ret_value.buf_addr".
			 */
			assert(YDB_OK == status);
			UNUSED(status); // Prevent 'value never read' compiler warning
			ret_value.buf_addr[ret_value.len_used] = '\0';
			octo_log(line, file, ERROR, ERROR_Severity, ERR_VARCHAR_TOO_LONG, ret_value.buf_addr);
			/* Now that we have got the value, delete the M node */
			ydb_delete_s(&varname, 2, subs, YDB_DEL_NODE);
		}
		/* Check if %ydboctoerror("DUPLICATEKEYVALUE")	*/
		ydboctoerrcode++;
		if (positive_status == ydboctoerrcode) {
			ydb_buffer_t subs[2];

			/* M code would have passed the actual string involved in an M node. Get that before printing error. */
			YDB_LITERAL_TO_BUFFER("%ydboctoerror", &varname);
			YDB_LITERAL_TO_BUFFER("DUPLICATEKEYVALUE", &subs[0]);
			YDB_LITERAL_TO_BUFFER("1", &subs[1]);
			status = ydb_get_s(&varname, 2, subs, &ret_value);
			if (YDB_ERR_INVSTRLEN == status) {
				EXPAND_YDB_BUFFER_T_ALLOCATION(ret_value);
				status = ydb_get_s(&varname, 2, subs, &ret_value);
				assert(YDB_OK == status);
				UNUSED(status); // Prevent 'value never read' compiler warning
			}
			ret_value.buf_addr[ret_value.len_used] = '\0';
			octo_log(line, file, ERROR, ERROR_Severity, ERR_DUPLICATE_KEY_VALUE, ret_value.buf_addr);
			/* Now that we have got the value, delete the M node */
			ydb_delete_s(&varname, 2, subs, YDB_DEL_NODE);
		}
		/* Check if %ydboctoerror("NULLKEYVALUE")	*/
		ydboctoerrcode++;
		if (positive_status == ydboctoerrcode) {
			ydb_buffer_t subs[2];

			/* M code would have passed the actual string involved in an M node. Get that before printing error. */
			YDB_LITERAL_TO_BUFFER("%ydboctoerror", &varname);
			YDB_LITERAL_TO_BUFFER("NULLKEYVALUE", &subs[0]);
			YDB_LITERAL_TO_BUFFER("1", &subs[1]);
			status = ydb_get_s(&varname, 2, subs, &ret_value);
			if (YDB_ERR_INVSTRLEN == status) {
				EXPAND_YDB_BUFFER_T_ALLOCATION(ret_value);
				status = ydb_get_s(&varname, 2, subs, &ret_value);
				assert(YDB_OK == status);
				UNUSED(status); // Prevent 'value never read' compiler warning
			}
			ret_value.buf_addr[ret_value.len_used] = '\0';
			octo_log(line, file, ERROR, ERROR_Severity, ERR_NULL_KEY_VALUE, ret_value.buf_addr);
			/* Now that we have got the value, delete the M node */
			ydb_delete_s(&varname, 2, subs, YDB_DEL_NODE);
		}
		/* Check if %ydboctoerror("UNKNOWNFUNCTION")	*/
		ydboctoerrcode++;
		if (positive_status == ydboctoerrcode) {
			ydb_buffer_t subs[2], ret_buff;
			char	     funcname_buff[MAX_ROUTINE_LEN + 1], numparm_buff[INT64_TO_STRING_MAX],
			    emulation_buff[MAX_EMULATION_STRING_LEN + 1];

			/* M code would have passed the parameters for the error message in M nodes.
			 * Get that before printing error.
			 */
			YDB_LITERAL_TO_BUFFER("%ydboctoerror", &varname);
			YDB_LITERAL_TO_BUFFER("UNKNOWNFUNCTION", &subs[0]);
			/* Get function name */
			YDB_LITERAL_TO_BUFFER("1", &subs[1]);
			ret_buff.len_alloc = sizeof(funcname_buff) - 1; /* Leave 1 byte for null terminator */
			ret_buff.buf_addr = funcname_buff;
			status = ydb_get_s(&varname, 2, subs, &ret_buff);
			assert(YDB_OK == status);
			UNUSED(status); /* needed to avoid a [clang-analyzer-deadcode.DeadStores] warning */
			ret_buff.buf_addr[ret_buff.len_used] = '\0';
			ydb_delete_s(&varname, 2, subs, YDB_DEL_NODE); /* Now that we have got the value, delete the M node */
			/* Get number of parameters */
			YDB_LITERAL_TO_BUFFER("2", &subs[1]);
			ret_buff.len_alloc = sizeof(numparm_buff) - 1; /* Leave 1 byte for null terminator */
			ret_buff.buf_addr = numparm_buff;
			status = ydb_get_s(&varname, 2, subs, &ret_buff);
			assert(YDB_OK == status);
			UNUSED(status); /* needed to avoid a [clang-analyzer-deadcode.DeadStores] warning */
			ret_buff.buf_addr[ret_buff.len_used] = '\0';
			ydb_delete_s(&varname, 2, subs, YDB_DEL_NODE); /* Now that we have got the value, delete the M node */
			/* Get database emulation mode */
			YDB_LITERAL_TO_BUFFER("3", &subs[1]);
			ret_buff.len_alloc = sizeof(emulation_buff) - 1; /* Leave 1 byte for null terminator */
			ret_buff.buf_addr = emulation_buff;
			status = ydb_get_s(&varname, 2, subs, &ret_buff);
			assert(YDB_OK == status);
			UNUSED(status); /* needed to avoid a [clang-analyzer-deadcode.DeadStores] warning */
			ret_buff.buf_addr[ret_buff.len_used] = '\0';
			ydb_delete_s(&varname, 2, subs, YDB_DEL_NODE); /* Now that we have got the value, delete the M node */
			/* Issue error */
			octo_log(line, file, ERROR, ERROR_Severity, ERR_UNKNOWN_FUNCTION_EMULATION, numparm_buff, funcname_buff,
				 emulation_buff);
		}
		/* Not an Octo internal error */
		ydboctoerrcode++;
		assert(ydboctoerrcode == ydboctoerrcodemax);
		/* Clear "$ECODE" now that we have handled the Octo-internal error.
		 * Otherwise "ydb_etrap" would be invoked at a later point in time.
		 */
		YDB_LITERAL_TO_BUFFER("$ECODE", &varname);
		ydb_set_s(&varname, 0, NULL, NULL); /* M equivalent is : SET $ECODE="" */
	} else {
		/* Assert that the error code falls in the range of a valid YDB error code */
		assert(YDB_MIN_YDBERR <= positive_status);
		assert(YDB_MAX_YDBERR > positive_status);
		/* Use ydb_status instead of ydb_get_s to preserve the original error message in $ZSTATUS
		 * in case the buffer needs to be resized, i.e. the call returns YDB_ERR_INVSTRLEN
		 */
		status = ydb_zstatus(ret_value.buf_addr, ret_value.len_alloc);
		while (YDB_ERR_INVSTRLEN == status) {
			ret_value.len_used = ret_value.len_alloc * 2; // Update len_used to specify new size for following macro
			EXPAND_YDB_BUFFER_T_ALLOCATION(ret_value);
			status = ydb_zstatus(ret_value.buf_addr, ret_value.len_alloc);
		}
		assert(YDB_OK == status);
		YDB_SEVERITY(positive_status, severity);
		switch (severity) {
		case YDB_SEVERITY_SUCCESS:
			octo_log(line, file, TRACE, TRACE_Severity, ERR_YOTTADB, ret_value.buf_addr);
			break;
		case YDB_SEVERITY_INFORMATIONAL:
			octo_log(line, file, INFO, INFO_Severity, ERR_YOTTADB, ret_value.buf_addr);
			break;
		case YDB_SEVERITY_WARNING:
			octo_log(line, file, ERROR, WARNING_Severity, ERR_YOTTADB, ret_value.buf_addr);
			break;
		case YDB_SEVERITY_ERROR:
			octo_log(line, file, ERROR, ERROR_Severity, ERR_YOTTADB, ret_value.buf_addr);
			break;
		case YDB_SEVERITY_FATAL:
			octo_log(line, file, ERROR, FATAL_Severity, ERR_YOTTADB, ret_value.buf_addr);
			break;
		default:
			break;
		}
	}
	YDB_FREE_BUFFER(&ret_value);
}
