/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
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

#define ROUTINE_PREFIX_LEN LIT_LEN(TABLE_GLOBAL_NAME_PREFIX)

void generate_routine_name(hash128_state_t *state, char *routine_name, int routine_len, FileType file_type) {
	char	     hash_str[MAX_ROUTINE_LEN];
	char *	     c = NULL;
	unsigned int hash_len = 0;
	ydb_uint16   hash;

	assert(state);

	switch (file_type) {
	case CrossReference:
		assert(ROUTINE_PREFIX_LEN == LIT_LEN(XREF_PLAN_NAME_PREFIX));
		MEMCPY_LIT(routine_name, XREF_PLAN_NAME_PREFIX);
		break;
	case OutputPlan:
		assert(ROUTINE_PREFIX_LEN == LIT_LEN(PHYSICAL_PLAN_NAME_PREFIX));
		MEMCPY_LIT(routine_name, PHYSICAL_PLAN_NAME_PREFIX);
		break;
	case YDBTrigger:
		assert(ROUTINE_PREFIX_LEN == LIT_LEN(TRIGGER_NAME_PREFIX));
		MEMCPY_LIT(routine_name, TRIGGER_NAME_PREFIX);
		break;
	case FunctionHash:
	default:
		assert(FunctionHash == file_type);
		assert(ROUTINE_PREFIX_LEN == LIT_LEN(FUNCTION_NAME_PREFIX));
		MEMCPY_LIT(routine_name, FUNCTION_NAME_PREFIX);
		break;
	}

	hash_len = routine_len - ROUTINE_PREFIX_LEN;

	ydb_mmrhash_128_result(state, 0, &hash);
	ydb_hash_to_string(&hash, hash_str, hash_len);

	// Copy hash string into filename
	c = routine_name;
	c += ROUTINE_PREFIX_LEN;
	strncpy(c, hash_str, hash_len);
	c += hash_len;
	*c = '\0';

	return;
}
