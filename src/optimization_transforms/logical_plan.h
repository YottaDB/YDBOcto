/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#ifndef LOGICAL_PLAN
#define LOGICAL_PLAN

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"
#include "memory_chunk.h"

#define MALLOC_LP(RET, DEST, DEST_TYPE)				\
{								\
	OCTO_CMALLOC_STRUCT(RET, LogicalPlan);			\
	(RET)->type = DEST_TYPE;				\
	DEST = RET;						\
}

#define	MALLOC_LP_2ARGS(DEST, DEST_TYPE)			\
{								\
	LogicalPlan	*dummyPlan;				\
	MALLOC_LP(dummyPlan, DEST, DEST_TYPE);			\
}

#define GET_LP(DEST, SOURCE, SIDE, DEST_TYPE)			\
{								\
	assert((SIDE) < 2);					\
	assert((SOURCE)->v.operand[(SIDE)]->type == DEST_TYPE);	\
	(DEST) = (SOURCE)->v.operand[(SIDE)];			\
}

// Forward declarations
struct LogicalPlan;
struct SqlKey;

#define LP_ACTION_TYPE(name) name,
#define LP_ACTION_END(name) name
typedef enum {
  #include "lp_action_type.hd"
} LPActionType;
#undef LP_ACTION_TYPE
#undef LP_ACTION_END

extern const char *lp_action_type_str[];

// We use yet another triple type here so we can easily traverse the tree
//  to replace tables and wheres; specifically, the WHERE can have
//  complete trees under it, and it would be awkward to overload
//  void pointers
typedef struct LogicalPlan {
	LPActionType type;
	int *counter;
	union {
		// Set for most types
		struct LogicalPlan *operand[2];
		// Set if type == LP_COLUMN_ALIAS
		SqlColumnAlias *column_alias;
		// Set if type == LP_COLUMN_LIST_ALIAS
		SqlColumnListAlias *column_list_alias;
		// Set if type == LP_VALUE
		SqlValue *value;
		// Set if type == LP_TABLE
		SqlTableAlias *table_alias;
		// Set if type == LP_KEY
		struct SqlKey *key;
		// Set if type == LP_KEYWORD
		SqlOptionalKeyword *keywords;
		// Set if type == LP_PIECE_NUMBER
		int piece_number;
	} v;
} LogicalPlan;

typedef struct SqlKey {
	SqlTable *table;
	SqlColumn *column;
	int key_num;
	int unique_id;
	// If this key is fixed, this is the value
	LogicalPlan *value;
	// Used to customize how insert works; default is to
	//  get the key and add an integer column
	SqlValue *insert;
	// The only relevant types are KEY_FIXED, KEY_ADVANCE
	LPActionType type;
	// Table that owns this key; used to extract key from plan
	//  when generating an extract for a given column
	// If this key is part of a UNION, this is the LP_INSERT
	//  plan which outputs this key
	LogicalPlan *owner;
	// If true, this is an output key for a cross reference
	int is_cross_reference_key;
	// If this is a cross reference key which is not an output key, this will point to the
	// output key, which we can snag the column name from
	struct SqlKey *cross_reference_output_key;
	// The source of the cross reference
	SqlColumnAlias *cross_reference_column_alias;
	// If this is a cross refence key, this value will point to the filename used to store the
	// code to provide the cross reference
	char *cross_reference_filename;
} SqlKey;

// Helper functions

// Generates a base plan given a SELECT statement
LogicalPlan *generate_logical_plan(SqlStatement *stmt, int *plan_id);
LogicalPlan *optimize_logical_plan(LogicalPlan *plan);

// Generate a logical plan for a SET operation
LogicalPlan *lp_generate_set_logical_plan(SqlStatement *stmt, int *plan_id);

// Provides a copy of the plan
LogicalPlan *lp_copy_plan(LogicalPlan *plan);
// Copies the SqlKey into a new key
SqlKey *lp_copy_key(SqlKey *key);

// Verifies that we have a good structure
//  Rules: root is INSERT, has TABLE and PROJECT as parameters
//   PROJECT has COLUMN_LIST and SELECT as parameters
//     SELECT has TABLE and CRITERIA as parameters
//       CRITERIA has KEYS and WHERE as criteria
//  OR: root is SET_OPERATION, and has SET_OPTIONS and PLANS as parameters
//    SET_OPTION has <set type>
//    PLANS has <INSERT|SET_OPERATION> as both operands
int lp_verify_structure(LogicalPlan *plan);

// Returns the projection triple
LogicalPlan *lp_get_project(LogicalPlan *plan);
// Returns the select triple
LogicalPlan *lp_get_select(LogicalPlan *plan);
// Returns the key for the given LP
LogicalPlan *lp_get_select_key(LogicalPlan *plan, SqlKey *key);
// Returns the TABLE_JOIN statement for the given LP
LogicalPlan *lp_get_table_join(LogicalPlan *plan);
// Returns the WHERE statement for the given LP
LogicalPlan *lp_get_select_where(LogicalPlan *plan);
// Returns the LP_KEYWORDS for the given LP
LogicalPlan *lp_get_select_keywords(LogicalPlan *plan);
// Returns the selected columns for this plan
LogicalPlan *lp_get_projection_columns(LogicalPlan *plan);
// Returns the LP_KEYS from the select criteria
LogicalPlan *lp_get_keys(LogicalPlan *plan);
// Returns the LP_CRITERIA
LogicalPlan *lp_get_criteria(LogicalPlan *plan);
// Returns the key corresponding to a column, or NULL
SqlKey *lp_get_key(LogicalPlan *plan, LogicalPlan *column_alias);
// Returns the index of the specified column in the key ordering, or -1
//  if the specified column is not a key
int lp_get_key_index(LogicalPlan *plan, LogicalPlan *column_alias);
// Returns the output key
LogicalPlan *lp_get_output_key(LogicalPlan *plan);
// Returns the
// Inserts a key at the end of the plans keys
void lp_insert_key(LogicalPlan *plan, LogicalPlan *key);
// Returns LP_WHERE with an AND of the two wheres
LogicalPlan *lp_join_where(LogicalPlan *where1, LogicalPlan *where2);
// Returns a new logical plan representing the boolean structure from stmt
LogicalPlan *lp_generate_where(SqlStatement *stmt, int *plan_id);
// Given a column and a table, generates a cross reference plan and returns it
LogicalPlan *lp_generate_xref_plan(LogicalPlan *plan, SqlTable *table, SqlColumn *column, int unique_id);
/**
 * Returns the keys corresponding to the cross reference for column in table, and updates
 * the LP_TABLE_JOIN of plan to include the plan which needs to be execute to generate the cross
 * reference
 */
LogicalPlan *lp_generate_xref_keys(LogicalPlan *plan, SqlTable *table, SqlColumnAlias *column_alias, SqlTableAlias *table_alias);
// Returns a logical plan representing the provided ColumnListAlias
LogicalPlan *lp_column_list_to_lp(SqlColumnListAlias *list, int *plan_id);
// Converts a list of columns to a column list associated with the given table alias
SqlColumnListAlias *lp_columns_to_column_list(SqlColumn *column, SqlTableAlias *table_alias);
LogicalPlan *lp_table_join_to_column_list(LogicalPlan *table_join, int *plan_id);
LogicalPlan *lp_replace_derived_table_references(LogicalPlan *root, LogicalPlan *new_plan, SqlTableAlias *table_alias);
// Given a SET operation, drills down until it encounters the first LP_INSERT statement
LogicalPlan *lp_drill_to_insert(LogicalPlan *plan);

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
// Converts the provided boolean expression to normal disjunctive form
LogicalPlan *lp_make_normal_disjunctive_form(LogicalPlan *root);
// Generates a new plan which is a LP_SET_OPERATION of the two plans
// If a or b is NULL, returns the other
LogicalPlan *lp_join_plans(LogicalPlan *a, LogicalPlan *b, LPActionType type);

// Inserts a new key into the plan for the given column alias (column, table, unique_id
// This key *must* be fixed to a value later, is it may not be resolvable before then
// Returned value will be a LP_KEY
LogicalPlan *lp_make_key(SqlColumnAlias *column_alias);

// Specific optimizations we can perform
//  These return 1 if the optimization succeeded, 0 otherwise
/// Attempts to replace this EQUALS statement with a xref IN
int lp_optimize_where_replace_non_key_equal(LogicalPlan *plan, LogicalPlan *where);
/**
 * Attempts to optimize there WHERE statement which contains nothing but items like
 *   "X = Y AND Y = Z AND Z = A"
 */
int lp_optimize_where_multi_equal_ands(LogicalPlan *plan, LogicalPlan *where);

// Returns a unique number within the context of this plan;
//  maybe not be unique in terms of global numbers
int get_plan_unique_number(LogicalPlan *plan);


// Optimization routines
int lp_opt_fix_key_to_const(LogicalPlan *root, SqlKey *key, LogicalPlan *value);

#endif
