/****************************************************************
 *								*
 * Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	*
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

char *get_aggregate_func_name(SqlAggregateType type) {
	switch (type) {
	case AGGREGATE_COUNT_ASTERISK:
		return "count(*)";
		break;
	case AGGREGATE_COUNT:
	case AGGREGATE_COUNT_TABLE_ASTERISK:
	case AGGREGATE_COUNT_DISTINCT:
	case AGGREGATE_COUNT_DISTINCT_TABLE_ASTERISK:
		return "count";
		break;
	case AGGREGATE_AVG:
	case AGGREGATE_AVG_DISTINCT:
		return "avg";
		break;
	case AGGREGATE_MIN:
		return "min";
		break;
	case AGGREGATE_MAX:
		return "max";
		break;
	case AGGREGATE_SUM:
	case AGGREGATE_SUM_DISTINCT:
		return "sum";
		break;
	default:
		assert(FALSE);
		break;
	}
	return "";
}
