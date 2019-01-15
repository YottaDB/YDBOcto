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
#ifndef PHYSICAL_PLAN
#define PHYSICAL_PLAN

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

#include "logical_plan.h"

enum PPActionType {
	PP_PROJECT,
	PP_DELETE,
	PP_ADVANCE
} typedef PPActionType;

// Some SET operations require special post-condition stuff;
//  this allows us to track what set operation we are doing in
//  in the physical plan
enum PPSetOperation {
	PP_NOT_SET,
	PP_UNION_SET,
	PP_EXCEPT_SET,
	PP_INTERSECT_SET
} typedef PPSetOperation;

struct PhysicalPlan typedef PhysicalPlan;

struct PhysicalPlan {
	char *plan_name;
	PhysicalPlan *prev, *next;
	// These represent keys which we are iterating over; usually a single key
	SqlKey *sourceKeys[MAX_KEY_COUNT];
	// These represent the keys we used to do the iteration
	SqlKey *iterKeys[MAX_KEY_COUNT];
	SqlKey *outputKey;
	SqlTableAlias *outputTable;
	LogicalPlan *where;
	LogicalPlan *projection;
	LogicalPlan *order_by;
	SqlTableAlias **symbols;
	SqlOptionalKeyword *keywords;
	unsigned int total_symbols;
	unsigned int total_iter_keys;
	unsigned int total_source_keys;
	// If set to 1, this plan should emit the columns as subscripts of the key,
	//  rather than using a row id
	int stash_columns_in_keys;

	// If true, maintain a column-wise index of known types
	int maintain_columnwise_index;
	// If true, only add the value to the output key if it doesn't already exist in
	//  the columnwise index; requires the columnwise index
	int distinct_values;
	// The type of action to perform; project inserts value, delete removes them
	PPActionType action_type;
	PPSetOperation set_operation;
};

PhysicalPlan *generate_physical_plan(LogicalPlan *plan, PhysicalPlan *next);
// Outputs physical plans to temporary files located in config.tmp_dir
//  Names are like ppplanXXXX, where XXXX is a unique number
// Returns TRUE on success
int emit_physical_plan(PhysicalPlan *pplan);

// Returns true if the key is a version of this column
int key_equals_column(SqlKey *key, SqlColumn *column);

void print_temporary_table(PhysicalPlan *plan, int cursor_id);

PhysicalPlan *emit_select_statement(ydb_buffer_t *cursor_global,
  ydb_buffer_t *cursor_exe_global, struct SqlStatement *stmt,
  SqlTable *destination_table);
PhysicalPlan *emit_insert_statement(ydb_buffer_t *cursor_global,
    ydb_buffer_t *cursor_exe_global, struct SqlStatement *stmt);


#endif
