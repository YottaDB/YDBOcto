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

#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

SqlOptionalKeyword *get_keyword_from_keywords(SqlOptionalKeyword *start_keyword, enum OptionalKeyword keyword) {
	SqlOptionalKeyword *cur_keyword;

	cur_keyword = start_keyword;
	do {
		if (cur_keyword->keyword == keyword)
			return cur_keyword;
		cur_keyword = cur_keyword->next;
	} while (cur_keyword != start_keyword);
	return NULL;
}
