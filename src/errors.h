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

#ifndef ERRORS_H
#define ERRORS_H

#include <stdarg.h>
#include "constants.h"

#define FALSE 0
#define TRUE  1

enum VERBOSITY_LEVEL { TRACE, DEBUG, INFO, ERROR };

enum SEVERITY_LEVEL { TRACE_Severity, DEBUG_Severity, INFO_Severity, WARNING_Severity, ERROR_Severity, FATAL_Severity };

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
	int  offset;
} ErrorBuffer;

void octo_log(int line, char *file, enum VERBOSITY_LEVEL level, enum SEVERITY_LEVEL severity, enum ERROR error, ...);
void ydb_error_check(int status, char *file, int line);

/* This macro is needed so we record the caller's __FILE__ and __LINE__ locations instead of that inside `ydb_error_check.c` */
#define YDB_ERROR_CHECK(STATUS) ydb_error_check(STATUS, __FILE__, __LINE__)

const char *format_error_string(struct ErrorBuffer *err_buff, enum ERROR error, ...);

#define TRACE(err, ...) \
	TRACE >= config->verbosity_level ? octo_log(__LINE__, __FILE__, TRACE, TRACE_Severity, err, ##__VA_ARGS__) : (void)0;
#define DEBUG(err, ...) \
	DEBUG >= config->verbosity_level ? octo_log(__LINE__, __FILE__, DEBUG, DEBUG_Severity, err, ##__VA_ARGS__) : (void)0;
#define INFO(err, ...) \
	INFO >= config->verbosity_level ? octo_log(__LINE__, __FILE__, INFO, INFO_Severity, err, ##__VA_ARGS__) : (void)0;
#define WARNING(err, ...) \
	INFO >= config->verbosity_level ? octo_log(__LINE__, __FILE__, INFO, WARNING_Severity, err, ##__VA_ARGS__) : (void)0;
// ERROR and FATAL severity errors should always be reported, so don't bother checking the verbosity level
#define ERROR(err, ...) octo_log(__LINE__, __FILE__, ERROR, ERROR_Severity, err, ##__VA_ARGS__);
#define FATAL(err, ...) octo_log(__LINE__, __FILE__, ERROR, FATAL_Severity, err, ##__VA_ARGS__);

#endif
