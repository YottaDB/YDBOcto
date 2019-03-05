/* Copyright (C) 2018 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef ERRORS_H
#define ERRORS_H

#include <stdarg.h>

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

#define ERROR_DEF(name, format_string) name,
#define ERROR_END(name, format_string) name
enum ERROR {
  #include "errors.hd"
};
#undef ERROR_DEF
#undef ERROR_END

#define ERROR_DEF(name, format_string) format_string,
#define ERROR_END(name, format_string) format_string
static const char *err_format_str[] = {
  #include "errors.hd"
};
#undef ERROR_DEF
#undef ERROR_END

void octo_log(int line, char *file, enum ERROR_LEVEL level, enum ERROR error, ...);

#define YDB_ERROR_CHECK(status, z_status, msg) do {   \
	if(YDB_OK != status) {                              \
		YDB_LITERAL_TO_BUFFER("$ZSTATUS", (z_status));    \
		INIT_YDB_BUFFER((msg), MAX_STR_CONST);            \
		ydb_get_s(z_status, 0, NULL, (msg));              \
		(msg)->buf_addr[(msg)->len_used] = '\0';          \
		/** TODO: not all ydb errs FATAL */               \
		octo_log(__LINE__, __FILE__, FATAL, ERR_YOTTADB, (msg)->buf_addr);    \
		free((msg)->buf_addr);                            \
		(msg)->len_used = 0;                              \
		(msg)->len_alloc = 0;                             \
	}                                                   \
} while(0);

#define TRACE(err, ...) TRACE >= config->record_error_level  \
	? octo_log(__LINE__, __FILE__, TRACE, err, ## __VA_ARGS__) : FALSE;
#define INFO(err, ...) INFO >= config->record_error_level  \
	? octo_log(__LINE__, __FILE__, INFO, err, ## __VA_ARGS__) : FALSE;
#define DEBUG(err, ...) DEBUG >= config->record_error_level  \
	? octo_log(__LINE__, __FILE__, DEBUG, err, ## __VA_ARGS__) : FALSE;
#define WARNING(err, ...) WARNING >= config->record_error_level  \
	? octo_log(__LINE__, __FILE__, WARNING, err, ## __VA_ARGS__) : FALSE;
#define ERROR(err, ...) ERROR >= config->record_error_level  \
	? octo_log(__LINE__, __FILE__, ERROR, err, ## __VA_ARGS__) : FALSE;
#define FATAL(err, ...) FATAL >= config->record_error_level  \
	? octo_log(__LINE__, __FILE__, FATAL, err, ## __VA_ARGS__) : FALSE;

#endif
