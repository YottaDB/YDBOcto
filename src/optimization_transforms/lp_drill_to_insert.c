/* Copyright (C) 2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

LogicalPlan *lp_drill_to_insert(LogicalPlan *plan) {
	LogicalPlan *cur_plan = plan;

	if(plan->type == LP_INSERT)
		return plan;

	if(cur_plan->type == LP_SET_OPERATION) {
		// Fetch one of the output plans from this set
		cur_plan = cur_plan->v.operand[1];
		assert(cur_plan->type == LP_PLANS);
		cur_plan = cur_plan->v.operand[0];
	} else {
		assert(FALSE);
	}

	return lp_drill_to_insert(cur_plan);
}
