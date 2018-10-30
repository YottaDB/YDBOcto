/* Copyright (C) 2018 YottaDB, LLC
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
#include <stdlib.h>

#include "logical_plan.h"

LogicalPlan *lp_copy_plan(LogicalPlan *plan) {
	LogicalPlan *new_plan;
	if(plan == NULL)
		return NULL;
	new_plan = (LogicalPlan *)malloc(sizeof(LogicalPlan));
	*new_plan = *plan;
	/// TODO: should this also clone tables and what not?
	switch(plan->type) {
	case LP_VALUE:
	case LP_TABLE:
	case LP_KEY:
	case LP_COLUMN_ALIAS:
		break;
	default:
		new_plan->v.operand[0] = lp_copy_plan(plan->v.operand[0]);
		new_plan->v.operand[1] = lp_copy_plan(plan->v.operand[1]);
	}
	return new_plan;
}
