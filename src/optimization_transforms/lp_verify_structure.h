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

#ifndef LP_VERIFY_STRUCTURE
#define LP_VERIFY_STRUCTURE

/* The below prototype requires both "logical_plan.h" and "physical_plan.h" hence it is in its own header file */

// Verifies that we have a good structure
//  Rules: root is INSERT, has TABLE and PROJECT as parameters
//   PROJECT has COLUMN_LIST and SELECT as parameters
//     SELECT has TABLE and CRITERIA as parameters
//       CRITERIA has KEYS and WHERE as criteria
//  OR: root is SET_OPERATION, and has SET_OPTIONS and PLANS as parameters
//    SET_OPTION has <set type>
//    PLANS has <INSERT|SET_OPERATION> as both operands
// As a side effect, this also fills in the linked list of aggregate functions in `options->aggregate` if options is non-NULL.
int lp_verify_structure(LogicalPlan *plan, PhysicalPlanOptions *options);

#endif
