/****************************************************************
 *								*
 * Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	*
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

/* Helper function invoked by various rules in src/parser.y and src/parser/select.y
 * Returns a pointer to an allocated "SqlStatement" structure of type "keyword_STATEMENT"
 * and holding one "NO_KEYWORD" keyword in the doubly linked list of keywords.
 */
SqlStatement *alloc_no_keyword(void) {
	SqlStatement *ret;

	MALLOC_KEYWORD_STMT(ret, NO_KEYWORD);
	ret->v.keyword->v = NULL;
	return ret;
}
