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
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

void lp_insert_key(LogicalPlan *plan, LogicalPlan *key) {
	LogicalPlan *cur_keys, *t;

	cur_keys = lp_get_keys(plan);

	// Drill down to the last item
	if(cur_keys->v.operand[0] != NULL) {
		while(cur_keys->v.operand[1] != NULL) {
			cur_keys = cur_keys->v.operand[1];
		}
		t = MALLOC_LP(cur_keys->v.operand[1], LP_KEYS);
		t->v.operand[0] = key;
	} else {
		cur_keys->v.operand[0] = key;
	}
}
