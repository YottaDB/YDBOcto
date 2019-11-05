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
#include <assert.h>
#include <unistd.h>

#include "octo.h"
#include "octo_types.h"

#include "logical_plan.h"
#include "physical_plan.h"

#include "mmrhash.h"

int generate_routine_name(hash128_state_t *state, char *routine_name, int routine_len, FileType file_type) {
	char hash_str[MAX_ROUTINE_LEN];
	char *xref_prefix = "%ydboctoX";
	char *output_plan_prefix = "%ydboctoP";
	char *ydb_trigger_prefix = "%ydboctoT";
	char *c = NULL;
	unsigned int prefix_len = 0;
	unsigned int hash_len = 0;
	ydb_uint16 hash;

	assert(state);

	prefix_len = strlen(xref_prefix);	// All prefixes have the same size
	switch (file_type) {
		case CrossReference:
			strncpy(routine_name, xref_prefix, prefix_len);
			break;
		case OutputPlan:
			strncpy(routine_name, output_plan_prefix, prefix_len);
			break;
		case YDBTrigger:
			strncpy(routine_name, ydb_trigger_prefix, prefix_len);
			break;
		default:
			return 1;
			break;
	}

	hash_len = routine_len - prefix_len;

	ydb_mmrhash_128_result(state, 0, &hash);
	ydb_hash_to_string(&hash, hash_str, hash_len);

	// Copy hash string into filename
	c = routine_name;
	c += prefix_len;
	strncpy(c, hash_str, hash_len);
	c += hash_len;
	*c = '\0';

	return 0;
}
