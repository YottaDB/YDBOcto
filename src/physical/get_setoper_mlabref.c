/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include "logical_plan.h"
#include "physical_plan.h"

/* Given a SET operation and a physical plan, this function returns the M entryref that needs to be invoked
 * in "src/aux/_ydboctoplanhelpers.m" that implements this SET operation.
 */
char *get_setoper_mlabref(SetOperType *set_oper, PhysicalPlan *pplan) {
	char *plan_helper_mlabref;

	switch (set_oper->set_oper_type) {
	case LP_SET_UNION:
		plan_helper_mlabref = (pplan->stash_columns_in_keys ? "columnkeyUNION" : "UNION");
		break;
	case LP_SET_UNION_ALL:
		plan_helper_mlabref = (pplan->stash_columns_in_keys ? "columnkeyUNIONALL" : "UNIONALL");
		break;
	case LP_SET_EXCEPT:
		plan_helper_mlabref = (pplan->stash_columns_in_keys ? "columnkeyEXCEPT" : "EXCEPT");
		break;
	case LP_SET_EXCEPT_ALL:
		plan_helper_mlabref = (pplan->stash_columns_in_keys ? "columnkeyEXCEPTALL" : "EXCEPTALL");
		break;
	case LP_SET_INTERSECT:
		plan_helper_mlabref = (pplan->stash_columns_in_keys ? "columnkeyINTERSECT" : "INTERSECT");
		break;
	case LP_SET_INTERSECT_ALL:
		plan_helper_mlabref = (pplan->stash_columns_in_keys ? "columnkeyINTERSECTALL" : "INTERSECTALL");
		break;
	default:
		plan_helper_mlabref = NULL;
		assert(FALSE);
		break;
	}
	return plan_helper_mlabref;
}
