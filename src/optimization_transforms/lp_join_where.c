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

LogicalPlan *lp_join_where(LogicalPlan *where1, LogicalPlan *where2) {
	LogicalPlan *new_plan, *w;
	if(where1 == NULL)
		return where2;
	if(where2 == NULL)
		return where1;
	if(where1->v.operand[0] == NULL)
		return where2;
	if(where2->v.operand[0] == NULL)
		return where1;
	new_plan = (LogicalPlan *)octo_cmalloc(memory_chunks, sizeof(LogicalPlan));
	new_plan->type = LP_BOOLEAN_AND;
	assert(where1->type == LP_WHERE);
	w = where1->v.operand[0];
	assert(where2->type == LP_WHERE);
	new_plan->v.operand[0] = w;
	new_plan->v.operand[1] = where2->v.operand[0];
	where1->v.operand[0] = new_plan;
	return where1;
}
