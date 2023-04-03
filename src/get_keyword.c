/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

SqlOptionalKeyword *get_keyword(SqlColumn *column, enum OptionalKeyword keyword) {
	SqlOptionalKeyword *start_keyword;
	if (NULL == column->keywords) {
		// This is possible in case of table_value_STATEMENT
		return NULL;
	}
	UNPACK_SQL_STATEMENT(start_keyword, column->keywords, keyword);
	return get_keyword_from_keywords(start_keyword, keyword);
}
