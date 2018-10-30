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
#include <unistd.h>

#include "octo.h"
#include "octo_types.h"

#include "logical_plan.h"
#include "physical_plan.h"

#include "template_helpers.h"

int get_unique_number();

int emit_physical_plan(PhysicalPlan *pplan) {
	int plan_id, len, fd;
	PhysicalPlan *cur_plan = pplan, *first_plan;
	char buffer[MAX_STR_CONST], plan_name_buffer[MAX_STR_CONST];
	char filename[MAX_STR_CONST];
	FILE *output_file;

	assert(cur_plan != NULL);

	// Walk the plans back to the first
	while(cur_plan->prev != NULL)
		cur_plan = cur_plan->prev;

	first_plan = cur_plan;
	do {
		plan_id++;
		len = snprintf(plan_name_buffer, MAX_STR_CONST, "myEvilPlan%d", plan_id);
		cur_plan->plan_name = malloc(len+1);
		memcpy(cur_plan->plan_name, plan_name_buffer, len);
		cur_plan->plan_name[len] = '\0';
		cur_plan = cur_plan->next;
	} while(cur_plan != NULL);

	// We should probably get a hash of the input SQL statement and use
	//  that as the plan_id
	plan_id = 0;
	snprintf(filename, MAX_STR_CONST, "%s/outputPlan1.m", config->tmp_dir);
	output_file = fopen(filename, "w");
	cur_plan = first_plan;
	do {
		//printf("\n%s\n\n", buffer);
		/// TODO: error handling here
		tmpl_physical_plan(buffer, MAX_STR_CONST, cur_plan);
		assert(output_file != NULL);
		fprintf(output_file, "%s\n", buffer);
		cur_plan = cur_plan->next;
	} while(cur_plan != NULL);
	fd = fileno(output_file);
	fsync(fd);
	fclose(output_file);
	return TRUE;
}

int get_unique_number() {
	// https://xkcd.com/221/
	return 4;
}
