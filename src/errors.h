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

#ifndef ERRORS_H
#define ERRORS_H

#include <stdarg.h>
#include "constants.h"

#define TRUE 1
#define FALSE 0

enum ERROR_LEVEL {
	TRACE,
	INFO,
	DEBUG,
	WARNING,
	ERROR,
	FATAL
};

#define ERROR_DEF(name, format_string, psql_error_code) name,
#define ERROR_END(name, format_string, psql_error_code) name
enum ERROR {
  #include "errors.hd"
};
#undef ERROR_DEF
#undef ERROR_END

// Define PSQL Error code names
#define ERROR_DEF(name, format_string) name,
#define ERROR_END(name, format_string) name
typedef enum {
  #include "error_codes.hd"
} PSQL_SQLSTATECode;
#undef ERROR_DEF
#undef ERROR_END



extern const char *err_format_str[];

typedef struct ErrorBuffer {
	char buffer[MAX_STR_CONST];
	int offset;
} ErrorBuffer;

void octo_log(int line, char *file, enum ERROR_LEVEL level, enum ERROR error, ...);

const char *format_error_string(struct ErrorBuffer *err_buff, enum ERROR error, ...);

#define YDB_ERROR_CHECK(status, z_status, msg) do {					\
	if(YDB_OK != status) {								\
		YDB_LITERAL_TO_BUFFER("$ZSTATUS", (z_status));				\
		YDB_MALLOC_BUFFER((msg), MAX_STR_CONST);				\
		ydb_get_s(z_status, 0, NULL, (msg));					\
		(msg)->buf_addr[(msg)->len_used] = '\0';				\
		/** TODO: not all ydb errs FATAL */					\
		octo_log(__LINE__, __FILE__, FATAL, ERR_YOTTADB, (msg)->buf_addr);	\
		YDB_FREE_BUFFER((msg));							\
	}										\
} while(0);

#define TRACE(err, ...) TRACE >= config->record_error_level  \
	? octo_log(__LINE__, __FILE__, TRACE, err, ## __VA_ARGS__) : (void)0;
#define INFO(err, ...) INFO >= config->record_error_level  \
	? octo_log(__LINE__, __FILE__, INFO, err, ## __VA_ARGS__) : (void)0;
#define DEBUG(err, ...) DEBUG >= config->record_error_level  \
	? octo_log(__LINE__, __FILE__, DEBUG, err, ## __VA_ARGS__) : (void)0;
#define WARNING(err, ...) WARNING >= config->record_error_level  \
	? octo_log(__LINE__, __FILE__, WARNING, err, ## __VA_ARGS__) : (void)0;
#define ERROR(err, ...) ERROR >= config->record_error_level  \
	? octo_log(__LINE__, __FILE__, ERROR, err, ## __VA_ARGS__) : (void)0;
#define FATAL(err, ...) FATAL >= config->record_error_level  \
	? octo_log(__LINE__, __FILE__, FATAL, err, ## __VA_ARGS__) : (void)0;

#endif
