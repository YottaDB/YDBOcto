/* Copyright (C) 2018-2019 YottaDB, LLC
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
#include <string.h>

#include "octo.h"
#include "helpers.h"

// Makes an array of ydb_buffers (subscripts), beginning with global name and successively adding each name from args
ydb_buffer_t *make_buffers(char *global, size_t num_args, ...) {
	va_list args;
	ydb_buffer_t *ret;

	va_start(args, num_args);
	ret = vmake_buffers(global, num_args, args);
	va_end(args);

	return ret;
}

ydb_buffer_t *vmake_buffers(char *global, size_t num_args, va_list args) {
	ydb_buffer_t *buffers;
	char *arg;
	int i;

	buffers = (ydb_buffer_t*)malloc((num_args+1) * sizeof(ydb_buffer_t));

	buffers[0].buf_addr = global;
	buffers[0].len_used = buffers[0].len_alloc = strlen(global);

	for(i = 0; i < num_args; i++) {
		arg = va_arg(args, char*);
		buffers[i+1].buf_addr = arg;
		buffers[i+1].len_alloc = buffers[i+1].len_used = strlen(arg);
	}

	return buffers;
}
