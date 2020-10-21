/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
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

// A copy function to create a copy of the given list
SqlColumnListAlias *copy_column_list_alias_list(SqlColumnListAlias *cla) {
	SqlColumnListAlias *cla_copy, *cla_new, *cla_cur;

	cla_copy = NULL;
	cla_cur = cla;
	do {
		OCTO_CMALLOC_STRUCT(cla_new, SqlColumnListAlias);
		assert(NULL == cla_cur->duplicate_of_column);
		*cla_new = *cla_cur;
		dqinit(cla_new);
		if (NULL == cla_copy) {
			cla_copy = cla_new;
		} else {
			dqappend(cla_copy, cla_new);
		}
		cla_cur = cla_cur->next;
	} while (cla_cur != cla);
	return cla_copy;
}
