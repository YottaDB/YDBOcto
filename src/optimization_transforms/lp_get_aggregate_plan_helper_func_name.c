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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

char *lp_get_aggregate_plan_helper_func_name(LPActionType type) {
	char *m_func;

	switch (type) {
	case LP_AGGREGATE_FUNCTION_COUNT_ASTERISK:
		m_func = "CountAsterisk";
		break;
	case LP_AGGREGATE_FUNCTION_COUNT:
		m_func = "Count";
		break;
	case LP_AGGREGATE_FUNCTION_AVG:
		m_func = "Avg";
		break;
	case LP_AGGREGATE_FUNCTION_MIN:
		m_func = "Min";
		break;
	case LP_AGGREGATE_FUNCTION_MAX:
		m_func = "Max";
		break;
	case LP_AGGREGATE_FUNCTION_SUM:
		m_func = "Sum";
		break;
	case LP_AGGREGATE_FUNCTION_COUNT_DISTINCT:
		m_func = "CountDistinct";
		break;
	case LP_AGGREGATE_FUNCTION_AVG_DISTINCT:
		m_func = "AvgDistinct";
		break;
	case LP_AGGREGATE_FUNCTION_SUM_DISTINCT:
		m_func = "SumDistinct";
		break;
	default:
		assert(FALSE);
		m_func = NULL;
		break;
	}
	return m_func;
}
