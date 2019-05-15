#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <stdlib.h>

#include <libyottadb.h>

#include "octo.h"
#include "errors.h"

#ifdef FEATURE_ROCTO
#include "rocto/rocto.h"

enum MessageType {
	NOTIFICATION_MESSAGE,
	ERROR_MESSAGE
};

#endif

const char *host_info = "[%s:%s] ";
const char *log_prefix = "[%5s] %s:%d %04d-%02d-%02d %02d:%02d:%02d : ";

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
#ifdef FEATURE_ROCTO
	fprintf(stderr, host_info, rocto_session.ip, rocto_session.port);
#endif
	fprintf(stderr, log_prefix, type,
		file,
		line,
	        local_time.tm_year + 1900,
	        local_time.tm_mon + 1,
	        local_time.tm_mday,
	        local_time.tm_hour,
	        local_time.tm_min,
	        local_time.tm_sec);
	if(error == CUSTOM_ERROR) {
		vfprintf(stderr, va_arg(args, const char *), args);
	} else {
		vfprintf(stderr, err_format_str[error], args);
	}
	va_end(args);
	fprintf(stderr, "\n");
#ifdef FEATURE_ROCTO
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
				err_level = PSQL_Error_INFO;
				break;
			case ERROR:
				err_level = PSQL_Error_ERROR;
				break;
			case FATAL:
				err_level = PSQL_Error_FATAL;
				break;
		}
		err = make_error_response(err_level,
				PSQL_Code_Unknown,
				buffer,
				0);
		send_message(&rocto_session, (BaseMessage*)(&err->type));
		free_error_response(err);
		rocto_session.sending_message = FALSE;
	}
#endif
	if(level == FATAL) {
		ydb_fork_n_core();
		exit(error);
	}
	return;
}
