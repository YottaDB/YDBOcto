/****************************************************************
 *								*
 * Copyright (c) 2019-2025 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <assert.h>

#include <libyottadb.h>

#include "octo.h"
#include "errors.h"

#ifdef IS_ROCTO
#include "rocto/rocto.h"

enum MessageType { NOTIFICATION_MESSAGE, ERROR_MESSAGE };

#define ERROR_DEF(name, format_string) format_string,
#define ERROR_END(name, format_string) format_string
const char *psql_error_severity_str[] = {
#include "rocto/error_severity.hd"
};
#undef ERROR_DEF
#undef ERROR_END

#endif

// Define PSQL error code strings
#define ERROR_DEF(name, format_string) format_string,
#define ERROR_END(name, format_string) format_string
const char *psql_sqlstate_codes_str[] = {
#include "error_codes.hd"
};
#undef ERROR_DEF
#undef ERROR_END

// Define the mnemonic error names we expect for each error message
#define ERROR_DEF(name, format_string, psql_error_code) #name,
#define ERROR_END(name, format_string, psql_error_code) #name
const char *err_name_str[] = {
#include "errors.hd"
};
#undef ERROR_DEF
#undef ERROR_END

// Define the strings we expect for constants
#define ERROR_DEF(name, format_string, psql_error_code) format_string,
#define ERROR_END(name, format_string, psql_error_code) format_string
const char *err_format_str[] = {
#include "errors.hd"
};
#undef ERROR_DEF
#undef ERROR_END

// Map error definition to PSQL error code enum value
#define ERROR_DEF(name, format_string, psql_error_code) psql_error_code,
#define ERROR_END(name, format_string, psql_error_code) psql_error_code
const int err_code_map[] = {
#include "errors.hd"
};
#undef ERROR_DEF
#undef ERROR_END

const char *terse_log_prefix = "[%5s]: %s: ";
const char *terse_rocto_log_prefix = "[%s:%s] [%5s]: %s: ";
const char *verbose_log_prefix = "[%5s] %s:%d %04d-%02d-%02d %02d:%02d:%02d: %s: ";
const char *verbose_rocto_log_prefix = "[%s:%s] [%5s] %s:%d %04d-%02d-%02d %02d:%02d:%02d: %s: ";

#define MAX_ERR_PREFIX_LEN 1024

void populate_and_print_full_err_str(enum ERROR error, char **full_err_str, int *full_err_len, char *err_prefix,
				     va_list orig_args) {
	while (TRUE) {
		va_list args;
		int	copied;

		__va_copy(args, orig_args);
		if (CUSTOM_ERROR == error) {
			// Combine populated prefix with given error format string into new format string
#ifdef IS_ROCTO
			UNUSED(err_prefix); // Avoid compiler warning
			copied = vsnprintf(*full_err_str, *full_err_len, va_arg(args, const char *), args);
#else
			copied = snprintf(*full_err_str, *full_err_len, "%s%s\n", err_prefix, va_arg(args, const char *));
#endif
		} else {
			// Combine populated prefix with given error format string into new format string
#ifdef IS_ROCTO
			UNUSED(err_prefix); // Avoid compiler warning
			copied = vsnprintf(*full_err_str, *full_err_len, err_format_str[error], args);
#else
			copied = snprintf(*full_err_str, *full_err_len, "%s%s\n", err_prefix, err_format_str[error]);
#endif
		}
		va_end(args);
		// The buffer wasn't large enough, resize to fit
		if (*full_err_len <= copied) {
			free(*full_err_str);
			*full_err_len = copied + 1; /* Null terminator */
			*full_err_str = (char *)malloc(sizeof(char) * *full_err_len);
		} else {
#ifndef IS_ROCTO
			if (0 < copied) {
				__va_copy(args, orig_args);
				SAFE_PRINTF(vfprintf, stderr, FALSE, FALSE, *full_err_str, args);
				va_end(args);
			}
#endif
			break;
		}
	}
}

/**
 * Logs error at verbosity level, formatting output and sending to the correct location.
 *
 * If severity is FATAL, terminates the process
 */
void octo_log(int line, char *file, enum VERBOSITY_LEVEL level, enum SEVERITY_LEVEL severity, enum ERROR error, ...) {
	va_list	    args;
	const char *type;
	time_t	    log_time;
	struct tm   local_time;
	int	    copied, full_err_len;
	char	    err_prefix[MAX_ERR_PREFIX_LEN + 1]; // Null terminator
	char	   *full_err_str;

	if (level < config->verbosity_level)
		return;

	va_start(args, error);
	log_time = time(NULL);
	local_time = *localtime(&log_time);
	full_err_len = OCTO_INIT_BUFFER_LEN + 1; // Null terminator
	full_err_str = (char *)malloc(sizeof(char) * full_err_len);

	switch (severity) {
	case TRACE_Severity:
		type = "TRACE";
		break;
	case INFO_Severity:
		type = "INFO";
		break;
	case DEBUG_Severity:
		type = "DEBUG";
		break;
	case WARNING_Severity:
		type = "WARN";
		break;
	case FATAL_Severity:
		type = "FATAL";
		break;
	case ERROR_Severity:
	default:
		type = "ERROR";
		break;
	}
#ifdef IS_ROCTO
	const char    *line_start, *line_end;
	int	       err_level;
	ErrorResponse *err;

	if (ERROR == level) {
		// Verbosity is set to the default/minimum. In that case, omit C code line number, date, and time information.
		copied = snprintf(err_prefix, MAX_ERR_PREFIX_LEN, terse_rocto_log_prefix, rocto_session.ip, rocto_session.port,
				  type, err_name_str[error]);
	} else {
		/* A verbosity level greater than the default was specified. In that case, output also C code line number, date, and
		 * time information.
		 */
		copied = snprintf(err_prefix, MAX_ERR_PREFIX_LEN, verbose_rocto_log_prefix, rocto_session.ip, rocto_session.port,
				  type, file, line, local_time.tm_year + 1900, local_time.tm_mon + 1, local_time.tm_mday,
				  local_time.tm_hour, local_time.tm_min, local_time.tm_sec, err_name_str[error]);
	}
	assert(MAX_ERR_PREFIX_LEN > copied);
	if (MAX_ERR_PREFIX_LEN <= copied) {
		err_prefix[MAX_ERR_PREFIX_LEN] = '\0';
	}
	UNUSED(copied); // UNUSED macro needed to avoid 'never read' warning from clang-analyzer in RelWithDebInfo builds
	populate_and_print_full_err_str(error, &full_err_str, &full_err_len, err_prefix, args);

	/* At this point, we have potentially a bunch of lines to log to stderr. If we are the "rocto" server process,
	 * "stderr" would point to a file that is shared by multiple rocto server processes (each of which was forked off
	 * as part of connecting to multiple clients under the same listener rocto process). In that case, we do not want
	 * concurrent writes to the same rocto log file by multiple rocto server processes to overwrite each other.
	 * Therefore, we write out the bunch of lines into a memory buffer first and then do the "fprintf()" of that buffer
	 * to stderr. Since stderr is unbuffered by default (see man pages of "setvbuf()"), it is okay to do a "fprintf()"
	 * of the final accumulated memory buffer and write to "stderr" without risking an overwrite due to concurrent writes.
	 */
	char  *buffer;
	size_t buffer_size;
	FILE  *memstream;
	memstream = open_memstream(&buffer, &buffer_size);
	if (NULL == memstream) {
		/* If we cannot open a memstream, use "stderr" instead. We could potentially have overwrites in this case
		 * but it is considered acceptable since we will have some output show up instead of none.
		 */
		memstream = stderr;
	}
	line_start = full_err_str;
	line_end = line_start;
	while ('\0' != *line_end) {
		if ('\n' == *line_end) {
			copied = line_end - line_start;
			if (0 < copied) {
				if (stderr != memstream) {
					fprintf(memstream, "%s%.*s\n", err_prefix, copied, line_start);
				} else {
					SAFE_PRINTF(fprintf, stderr, FALSE, FALSE, "%s%.*s\n", err_prefix, copied, line_start);
				}
			}
			line_start = line_end + 1;
		}
		line_end++;
	}
	copied = line_end - line_start;
	if (0 < copied) {
		if (stderr != memstream) {
			fprintf(memstream, "%s%.*s\n", err_prefix, copied, line_start);
		} else {
			SAFE_PRINTF(fprintf, stderr, FALSE, FALSE, "%s%.*s\n", err_prefix, copied, line_start);
		}
	}
	if (stderr != memstream) {
		fclose(memstream); // at this point "buffer" and "buffer_size" are usable
		SAFE_PRINTF(fprintf, stderr, FALSE, FALSE, "%s", buffer);
		free(buffer);
	}
	if (!rocto_session.sending_message && rocto_session.connection_fd != 0) {
		rocto_session.sending_message = TRUE;
		if (TRACE < level) {
			switch (severity) {
			case TRACE_Severity:
				err_level = PSQL_Error_INFO;
				break;
			case INFO_Severity:
				err_level = PSQL_Error_INFO;
				break;
			case DEBUG_Severity:
				err_level = PSQL_Error_DEBUG;
				break;
			case WARNING_Severity:
				err_level = PSQL_Error_WARNING;
				break;
			case FATAL_Severity:
				err_level = PSQL_Error_FATAL;
				break;
			case ERROR_Severity:
			default:
				err_level = PSQL_Error_ERROR;
				break;
			}
			err = make_error_response(err_level, err_code_map[error], full_err_str, 0);
			send_message(&rocto_session, (BaseMessage *)(&err->type));
			free_error_response(err);
			rocto_session.sending_message = FALSE;
		}
	}
#else
	if (ERROR == level) {
		// Verbosity is set to the default/minimum. In that case, omit C code line number, date, and time information.
		copied = snprintf(err_prefix, MAX_ERR_PREFIX_LEN, terse_log_prefix, type, err_name_str[error]);
	} else {
		/* A verbosity level greater than the default was specified. In that case, output also C code line number, date, and
		 * time information.
		 */
		copied = snprintf(err_prefix, MAX_ERR_PREFIX_LEN, verbose_log_prefix, type, file, line, local_time.tm_year + 1900,
				  local_time.tm_mon + 1, local_time.tm_mday, local_time.tm_hour, local_time.tm_min,
				  local_time.tm_sec, err_name_str[error]);
	}
	assert(MAX_ERR_PREFIX_LEN > copied);
	if (MAX_ERR_PREFIX_LEN <= copied) {
		err_prefix[MAX_ERR_PREFIX_LEN] = '\0';
	}
	UNUSED(copied); // UNUSED macro needed to avoid 'never read' warning from clang-analyzer in RelWithDebInfo builds
	populate_and_print_full_err_str(error, &full_err_str, &full_err_len, err_prefix, args);
#endif
	va_end(args);
	if (FATAL_Severity == severity) {
#ifdef IS_ROCTO
		shutdown(rocto_session.connection_fd, SHUT_RDWR);
		close(rocto_session.connection_fd);
#endif
		exit(error);
	}
	free(full_err_str);
	return;
}
