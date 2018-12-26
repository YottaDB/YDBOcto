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
#ifndef LOGICAL_PLAN
#define LOGICAL_PLAN

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

#define MALLOC_LP(dest, dest_type)					\
	(dest) = (LogicalPlan*)malloc(sizeof(LogicalPlan));		\
	memset((dest), 0, sizeof(LogicalPlan));				\
	(dest)->type = dest_type;

#define GET_LP(dest, source, side, dest_type)			\
	assert((side) < 2);					\
	assert((source)->v.operand[(side)]->type == dest_type);	\
	(dest) = (source)->v.operand[(side)];

// Forward declarations
struct LogicalPlan typedef LogicalPlan;
struct SqlKey typedef SqlKey;

#define LP_ACTION_TYPE(name) name,
#define LP_ACTION_END(name) name
typedef enum {
  #include "lp_action_type.hd"
} LPActionType;
#undef LP_ACTION_TYPE
#undef LP_ACTION_END


#define LP_ACTION_TYPE(name) #name,
#define LP_ACTION_END(name) #name
static const char *lp_action_type_str[] = {
  #include "lp_action_type.hd"
};
#undef LP_ACTION_TYPE
#undef LP_ACTION_END


// We use yet another triple type here so we can easily traverse the tree
//  to replace tables and wheres; specifically, the WHERE can have
//  complete trees under it, and it would be awkward to overload
//  void pointers
struct LogicalPlan {
	LPActionType type;
	int *counter;
	union {
		// Set for most types
		LogicalPlan *operand[2];
		// Set if type == LP_COLUMN_ALIAS
		SqlColumnAlias *column_alias;
		// Set if type == LP_VALUE
		SqlValue *value;
		// Set if type == LP_TABLE
		SqlTableAlias *table_alias;
		// Set if type == LP_KEY
		SqlKey *key;
		// Set if type == LP_KEYWORD
		SqlOptionalKeyword *keywords;
		// Set if type == LP_PIECE_NUMBER
		int piece_number;
	} v;
};

struct SqlKey {
	SqlTable *table;
	SqlColumn *column;
	int key_num;
	int random_id;
	// If this key is fixed, this is the value
	LogicalPlan *value;
	// Used to customize how insert works; default is to
	//  get the key and add an integer column
	SqlValue *insert;
	// The only relevant types are KEY_FIXED, KEY_ADVANCE
	LPActionType type;
	// Previous subscript in this table
	//  if not a real key, *value will be set
	SqlKey *prevSubscript, *nextSubscript;
	// Table that owns this key; used to extract key from plan
	//  when generating an extract for a given column
	LogicalPlan *owner;
};

// Helper functions

// Generates a base plan given a SELECT statement
LogicalPlan *generate_logical_plan(SqlStatement *stmt, int *plan_id);
LogicalPlan *optimize_logical_plan(LogicalPlan *plan);

// Provides a copy of the plan
LogicalPlan *lp_copy_plan(LogicalPlan *plan);
// Deletes a plan
void lp_free_plan(LogicalPlan *plan);

// Verifies that we have a good structure
//  Rules: root is INSERT, has TABLE and PROJECT as parameters
//   PROJECT has COLUMN_LIST and SELECT as parameters
//     SELECT has TABLE and CRITERIA as parameters
//       CRITERIA has KEYS and WHERE as criteria
int lp_verify_structure(LogicalPlan *plan);

// Returns the projection triple
LogicalPlan *lp_get_project(LogicalPlan *plan);
// Returns the select triple
LogicalPlan *lp_get_select(LogicalPlan *plan);
// Returns the key for the given LP
LogicalPlan *lp_get_select_key(LogicalPlan *plan, SqlKey *key);
// Returns the WHERE statement for the given LP
LogicalPlan *lp_get_select_where(LogicalPlan *plan);
// Returns the LP_KEYWORDS for the given LP
LogicalPlan *lp_get_select_keywords(LogicalPlan *plan);
// Returns the selected columns for this plan
LogicalPlan *lp_get_projection_columns(LogicalPlan *plan);
// Returns the LP_KEYS from the select criteria
LogicalPlan *lp_get_keys(LogicalPlan *plan);
// Returns the key corresponding to a column, or NULL
SqlKey *lp_get_key(LogicalPlan *plan, LogicalPlan *column_alias);
// Returns the index of the specified column in the key ordering, or -1
//  if the specified column is not a key
int lp_get_key_index(LogicalPlan *plan, LogicalPlan *column_alias);
// Returns the output key
LogicalPlan *lp_get_output_key(LogicalPlan *plan);
// Inserts a key at the end of the plans keys
void lp_insert_key(LogicalPlan *plan, LogicalPlan *key);
// Returns LP_WHERE with an AND of the two wheres
LogicalPlan *lp_join_where(LogicalPlan *where1, LogicalPlan *where2);
// Returns a new logical plan representing the boolean structure from stmt
LogicalPlan *lp_generate_where(SqlStatement *stmt, int *plan_id);
// Returns a logical plan representing the provided ColumnListAlias
LogicalPlan *lp_column_list_to_lp(SqlColumnListAlias *list);
// Converts a list of columns to a column list associated with the given table alias
SqlColumnListAlias *lp_columns_to_column_list(SqlColumn *column, SqlTableAlias *table_alias);
LogicalPlan *lp_table_join_to_column_list(LogicalPlan *table_join);
LogicalPlan *lp_replace_derived_table_references(LogicalPlan *root, LogicalPlan *new_plan, SqlTableAlias *table_alias);

// Given a plan, attempts to calculate a "cost" estimate of that plan
int lp_calculate_plan_cost(LogicalPlan *plan);

// Fills buffer with a representation of the plan
int lp_emit_plan(char *buffer, size_t buffer_len, LogicalPlan *plan);

// Attempts to replace as many statements in the WHERE as possible
//  with table KEYS
int lp_optimize_where(LogicalPlan *plan);
// Attempts to move keys from WHERE to SELECT_KEYS
int lp_remove_keys(LogicalPlan *plan);
// Reorders keys where possible using statistics
int lp_optimize_keys(LogicalPlan *plan);
// Replaces JOINs with WHERE criteria and a new table
int lp_replace_joins(LogicalPlan *plan);

// Specific optimizations we can perform
//  These return 1 if the optimization succeeded, 0 otherwise
/// Attempts to replace this EQUALS statement with a xref IN
int lp_optimize_where_replace_non_key_equal(LogicalPlan *plan, LogicalPlan *where);

// Returns a unique number within the context of this plan;
//  maybe not be unique in terms of global numbers
int get_plan_unique_number(LogicalPlan *plan);

// Optimization routines
int lp_opt_fix_key_to_const(LogicalPlan *root, SqlKey *key, LogicalPlan *value);

#endif
