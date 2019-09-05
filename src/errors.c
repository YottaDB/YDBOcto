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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <stdlib.h>
#include <sys/socket.h>

#include <libyottadb.h>

#include "octo.h"
#include "errors.h"

#ifdef IS_ROCTO
#include "rocto/rocto.h"

enum MessageType {
	NOTIFICATION_MESSAGE,
	ERROR_MESSAGE
};


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

const char *log_prefix = "[%5s] %s:%d %04d-%02d-%02d %02d:%02d:%02d : ";
const char *rocto_log_prefix = "[%s:%s] [%5s] %s:%d %04d-%02d-%02d %02d:%02d:%02d : ";

/**
 * Logs error at level, formatting output and sending to the correct location.
 *
 * If level is FATAL, terminates the process and calls ydb_fork_n_core
 */
void octo_log(int line, char *file, enum ERROR_LEVEL level, enum ERROR error, ...) {
	va_list args;
	const char *type;
	time_t log_time;
	struct tm local_time;
	char err_prefix[MAX_STR_CONST];
	char full_err_format_str[MAX_STR_CONST];

	if(level < config->record_error_level)
		return;

	va_start(args, error);
	log_time = time(NULL);
	local_time = *localtime(&log_time);

	switch(level) {
	case TRACE:
		type = "TRACE";
		break;
	case INFO:
		type = "INFO";
		break;
	case DEBUG:
		type = "DEBUG";
		break;
	case WARNING:
		type = "WARN";
		break;
	case ERROR:
		type = "ERROR";
		break;
	case FATAL:
		type = "FATAL";
		break;
	}
#	ifdef IS_ROCTO
	snprintf(err_prefix, MAX_STR_CONST, rocto_log_prefix,
		rocto_session.ip,
		rocto_session.port,
		type,
		file,
		line,
	        local_time.tm_year + 1900,
	        local_time.tm_mon + 1,
	        local_time.tm_mday,
	        local_time.tm_hour,
	        local_time.tm_min,
	        local_time.tm_sec);
#	else
	snprintf(err_prefix, MAX_STR_CONST, log_prefix, type,
		file,
		line,
	        local_time.tm_year + 1900,
	        local_time.tm_mon + 1,
	        local_time.tm_mday,
	        local_time.tm_hour,
	        local_time.tm_min,
	        local_time.tm_sec);
#	endif
	if(error == CUSTOM_ERROR) {
		// Combine populated prefix with given error format string into new format string
		int copied = snprintf(full_err_format_str, MAX_STR_CONST, "%s%s\n", err_prefix, va_arg(args, const char *));
		if (0 < copied) {
			vfprintf(stderr, full_err_format_str, args);
		}
	} else {
		// Combine populated prefix with given error format string into new format string
		int copied = snprintf(full_err_format_str, MAX_STR_CONST, "%s%s\n", err_prefix, err_format_str[error]);
		if (0 < copied) {
			vfprintf(stderr, full_err_format_str, args);
		}
	}
	va_end(args);
#	ifdef IS_ROCTO
	const char *error_message;
	char buffer[MAX_STR_CONST];
	int err_level;
	int message_type;
	ErrorResponse *err;
	if(!rocto_session.sending_message && rocto_session.connection_fd != 0) {
		rocto_session.sending_message = TRUE;
		va_start(args, error);
		if(error == CUSTOM_ERROR) {
			vsnprintf(buffer, MAX_STR_CONST, va_arg(args, const char *), args);
		} else {
			vsnprintf(buffer, MAX_STR_CONST, err_format_str[error], args);
		}
		va_end(args);
		switch(level) {
			case TRACE:
				err_level = PSQL_Error_INFO;
				break;
			case INFO:
				err_level = PSQL_Error_INFO;
				break;
			case DEBUG:
				err_level = PSQL_Error_DEBUG;
				break;
			case WARNING:
				err_level = PSQL_Error_WARNING;
				break;
			case ERROR:
				err_level = PSQL_Error_ERROR;
				break;
			case FATAL:
				err_level = PSQL_Error_FATAL;
				break;
		}
		err = make_error_response(err_level,
				err_code_map[error],
				buffer,
				0);
		int sent_ret = send_message(&rocto_session, (BaseMessage*)(&err->type));
		free_error_response(err);
		rocto_session.sending_message = FALSE;
	}
#	endif
	if(level == FATAL) {
#ifdef IS_ROCTO
		shutdown(rocto_session.connection_fd, SHUT_RDWR);
#endif
		ydb_fork_n_core();
		exit(error);
	}
	return;
}
