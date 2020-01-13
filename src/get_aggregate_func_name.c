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

char	*get_aggregate_func_name(SqlAggregateType type) {
	switch(type) {
	case COUNT_ASTERISK_AGGREGATE:
		return "COUNT(*)";
		break;
	case COUNT_AGGREGATE:
	case COUNT_AGGREGATE_DISTINCT:
		return "COUNT";
		break;
	case AVG_AGGREGATE:
	case AVG_AGGREGATE_DISTINCT:
		return "AVG";
		break;
	case MIN_AGGREGATE:
		return "MIN";
		break;
	case MAX_AGGREGATE:
		return "MAX";
		break;
	case SUM_AGGREGATE:
	case SUM_AGGREGATE_DISTINCT:
		return "SUM";
		break;
	default:
		assert(FALSE);
		break;
	}
	return "";
}
