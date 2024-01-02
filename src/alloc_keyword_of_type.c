/****************************************************************
 *								*
 * Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

/* Helper function invoked by various rules in src/parser.y, src/parser/select.y and
 * src/optimization_transforms/generate_logical_plan.c.
 * Returns a pointer to an allocated "SqlStatement" structure of type "keyword_STATEMENT"
 * and holding one "keyword_type" keyword in the doubly linked list of keywords.
 */
SqlStatement *alloc_keyword_of_type(OptionalKeyword keyword_type) {
	SqlStatement *ret;

	MALLOC_KEYWORD_STMT(ret, keyword_type);
	ret->v.keyword->v = NULL;
	return ret;
}
