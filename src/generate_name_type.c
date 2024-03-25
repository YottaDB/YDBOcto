/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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

/* Given the input "name_type", "state" (which has already had one or more calls to "ydb_mmrhash_128_ingest()") and
 * "len" (used only by "TableGlobal" case) parameters, this function generates a name that is stored in the output
 * parameter "ret_name" followed by a 1-byte NULL terminator. The "ret_name_len" input parameter indicates the size
 * of the space allocated in "ret_name" (used to ensure we never exceed that space inside this function).
 */
void generate_name_type(NameType name_type, hash128_state_t *state, int len, char *ret_name, int ret_name_len) {
	char	   hash_str[MAX_ROUTINE_LEN];
	char	  *c = NULL;
	ydb_uint16 hash;

	/* Assert that "ret_name" has space for "MAX_ROUTINE_LEN" + 1-byte null terminator + 1-byte '^' where applicable */
	assert(NULL != state);
	c = ret_name;
	switch (name_type) {
	case CrossReference:
		assert((MAX_ROUTINE_LEN + 1) == ret_name_len);
		assert(ROUTINE_PREFIX_LEN == LIT_LEN(XREF_PLAN_NAME_PREFIX));
		MEMCPY_LIT(c, XREF_PLAN_NAME_PREFIX);
		break;
	case OutputPlan:
		assert((MAX_ROUTINE_LEN + 1) == ret_name_len);
		assert(ROUTINE_PREFIX_LEN == LIT_LEN(PHYSICAL_PLAN_NAME_PREFIX));
		MEMCPY_LIT(c, PHYSICAL_PLAN_NAME_PREFIX);
		break;
	case FunctionHash:
		assert((MAX_ROUTINE_LEN + 1) == ret_name_len);
		assert(ROUTINE_PREFIX_LEN == LIT_LEN(FUNCTION_NAME_PREFIX));
		MEMCPY_LIT(c, FUNCTION_NAME_PREFIX);
		break;
	case TableGlobal:
		assert((MAX_ROUTINE_LEN + 2) == ret_name_len);
		*c++ = '^';
		assert(ROUTINE_PREFIX_LEN == LIT_LEN(TABLE_GLOBAL_NAME_PREFIX));
		MEMCPY_LIT(c, TABLE_GLOBAL_NAME_PREFIX);
		break;
	case UniqueGlobal:
	default:
		assert(UniqueGlobal == name_type);
		assert((MAX_ROUTINE_LEN + 2) == ret_name_len);
		*c++ = '^';
		assert(ROUTINE_PREFIX_LEN == LIT_LEN(UNIQUE_GLOBAL_NAME_PREFIX));
		MEMCPY_LIT(c, UNIQUE_GLOBAL_NAME_PREFIX);
		break;
	}
	ydb_mmrhash_128_result(state, len, &hash);

	unsigned int hash_len;
	c += ROUTINE_PREFIX_LEN;
	hash_len = ret_name_len - (c - ret_name) - 1; /* - 1 to reserve space for 1-byte NULL terminator */
	ydb_hash_to_string(&hash, hash_str, hash_len);
	strncpy(c, hash_str, hash_len); // Copy hash string into name
	c += hash_len;
	*c = '\0';
	return;
}
