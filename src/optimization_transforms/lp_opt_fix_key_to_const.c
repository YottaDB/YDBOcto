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

/**
 * Fixes the key to a specific value
 *
 * If the key is already fixed, changes it to a UNION
 *
 * @returns TRUE (1) if the key was fixed, or FALSE if it couldn't be done
 */
int lp_opt_fix_key_to_const(LogicalPlan *root, SqlKey *key, LogicalPlan *value) {
	// We can't fix keys that are already fixed, or keys that are part of an
	//  cross reference iteration
	UNUSED(root);
	if(key->type == LP_KEY_ADVANCE && key->cross_reference_output_key == NULL) {
		key->value = lp_copy_plan(value);
		key->type = LP_KEY_FIX;
	} else {
		return FALSE;
	}
	return TRUE;
}
