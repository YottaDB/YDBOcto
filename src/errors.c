#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <stdlib.h>

#include <libyottadb.h>

#include "octo.h"
#include "errors.h"

const char *log_prefix = "[%5s] %s:%d %04d-%02d-%02d %02d:%02d:%02d : ";

/**
 * Logs error at level, formatting output and sending to the correct location.
 *
 * If level is FATAL, terminates the process and calls ydb_fork_n_core
 */
void octo_log(int line, char *file, enum ERROR_LEVEL level, enum ERROR error, ...) {
	va_list args;
	va_start(args, error);
	const char *type;
	time_t log_time;
	struct tm local_time;

	if(level < config->record_error_level)
		return;

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
	if(level == FATAL) {
		ydb_fork_n_core();
		exit(error);
	}
	return;
}
