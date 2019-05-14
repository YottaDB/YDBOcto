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
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

#include "octo.h"
#include "octo_types.h"

#include "logical_plan.h"
#include "physical_plan.h"

#include "mmrhash.h"

int generate_filename(hash128_state_t *state, const char *directory_path, char *full_path, FileType file_type) {
	const unsigned short max_filename_len = 31;
	char buffer[max_filename_len];
	char filename[max_filename_len + sizeof(char)];		// count null terminator
	char *xref_prefix = "_ydboctoX";
	char *output_plan_prefix = "_ydboctoP";
	char *c = NULL;
	unsigned int prefix_len = 0;
	unsigned int buf_len = 0;
	int full_path_len = 0;
	ydb_uint16 hash;

	if (state == NULL) {
		return -1;
	}
	if (directory_path == NULL) {
		return -1;
	}
	if (full_path == NULL) {
		return -1;
	}

	prefix_len = strlen(xref_prefix);	// All prefixes have the same size
	buf_len = max_filename_len - prefix_len;

	ydb_mmrhash_128_result(state, 0, &hash);
	ydb_hash_to_string(&hash, buffer, buf_len);

	switch (file_type) {
		case CrossReference:
			strncpy(filename, xref_prefix, prefix_len);
			break;
		case OutputPlan:
			strncpy(filename, output_plan_prefix, prefix_len);
			break;
		default:
			return -1;
	}

	// Copy hash string into filename
	c = filename;
	c += prefix_len;
	strncpy(c, buffer, buf_len);
	c += buf_len;
	*c = '\0';

	// Prepend directory path and append ".m" file extension
	full_path_len = snprintf(full_path, MAX_STR_CONST, "%s/%s.m", directory_path, filename);

	return full_path_len;
}
