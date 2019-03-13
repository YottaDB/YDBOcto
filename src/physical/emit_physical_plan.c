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

int emit_physical_plan(PhysicalPlan *pplan) {
	int plan_id, len, fd;
	PhysicalPlan *cur_plan = pplan, *first_plan;
	char buffer[MAX_STR_CONST], plan_name_buffer[MAX_STR_CONST];
	char filename[MAX_STR_CONST], *tableName, *columnName;
	SqlValue *value;
	SqlKey *key;
	FILE *output_file;

	assert(cur_plan != NULL);

	// Walk the plans back to the first
	while(cur_plan->prev != NULL)
		cur_plan = cur_plan->prev;
	first_plan = cur_plan;

	// Output the cross reference plans
	do {
		if(!(cur_plan->outputKey && cur_plan->outputKey->is_cross_reference_key)) {
			cur_plan = cur_plan->next;
			continue;
		}
		key = cur_plan->outputKey;
		UNPACK_SQL_STATEMENT(value, key->table->tableName, value);
		tableName = value->v.reference;
		UNPACK_SQL_STATEMENT (value, key->column->columnName, value);
		columnName = value->v.reference;
		len = snprintf(plan_name_buffer, MAX_STR_CONST, "myEvilPlan%d", plan_id);
		cur_plan->plan_name = malloc(len+1);
		memcpy(cur_plan->plan_name, plan_name_buffer, len);
		cur_plan->plan_name[len] = '\0';
		snprintf(filename, MAX_STR_CONST, "%s/genOctoXref%s%s.m", config->tmp_dir, tableName, columnName);
		output_file = fopen(filename, "w");
		filename[strlen(filename)-2] = '\0';
		cur_plan->filename = &filename[strlen(config->tmp_dir)+1];
		tmpl_physical_plan(buffer, MAX_STR_CONST, cur_plan);
		assert(output_file != NULL);
		fprintf(output_file, "%s\n", buffer);
		fd = fileno(output_file);
		fsync(fd);
		fclose(output_file);

		cur_plan = cur_plan->next;
	} while(cur_plan != NULL);

	cur_plan = first_plan;
	do {
		while(cur_plan && cur_plan->outputKey && cur_plan->outputKey->is_cross_reference_key) {
			cur_plan = cur_plan->next;
		}
		if(cur_plan == NULL)
			break;
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
		// Skip any plans that are cross references
		while(cur_plan && cur_plan->outputKey && cur_plan->outputKey->is_cross_reference_key) {
			cur_plan = cur_plan->next;
		}
		if(cur_plan == NULL)
			break;
		cur_plan->filename = filename;
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
