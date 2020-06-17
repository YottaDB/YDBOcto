/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

#define GET_LP(DEST, SOURCE, SIDE, DEST_TYPE)					\
{										\
	assert((SIDE) < 2);							\
	assert((SOURCE)->v.lp_default.operand[(SIDE)]->type == DEST_TYPE);	\
	(DEST) = (SOURCE)->v.lp_default.operand[(SIDE)];			\
}

/* The ERROR_ENCOUNTERED parameter is set to TRUE in case we notice errors inside "lp-generate_where()" (it could recurse
 * and go deep). This is so outermost caller knows to issue an error at logical plan stage and not proceed with physical plan
 * even if one error is seen anywhere in a recursive function call.
 */
#define	LP_GENERATE_WHERE(STMT, PLAN_ID, PARENT, RET, ERROR_ENCOUNTERED)	{	\
	if (NULL != STMT) {								\
		RET = lp_generate_where(STMT, PLAN_ID, PARENT);				\
		if (NULL == RET)							\
			ERROR_ENCOUNTERED = TRUE;					\
	} else {									\
		RET = NULL;								\
	}										\
}

// Forward declarations
struct LogicalPlan;
struct SqlKey;

#define LP_ACTION_TYPE(name) name,
typedef enum {
  #include "lp_action_type.hd"
} LPActionType;
#undef LP_ACTION_TYPE

#define	IS_TYPE_LP_AGGREGATE(TYPE)	((LP_AGGREGATE_FUNCTION_COUNT_ASTERISK <= TYPE) && (TYPE < LP_AGGREGATE_LAST))

extern const char *lp_action_type_str[];

/* Fields needed by various LP_* types */
typedef struct LpColumnAlias {
	SqlColumnAlias		*column_alias;
} LpColumnAlias;

typedef struct LpColumnListAlias {
	SqlColumnListAlias	*column_list_alias;
} LpColumnListAlias;

typedef struct LpValue {
	SqlValue 		*value;
} LpValue;

typedef struct LpTable {
	SqlTableAlias		*table_alias;
} LpTable;

typedef struct LpKey {
	struct SqlKey		*key;
} LpKey;

typedef struct LpKeywords {
	SqlOptionalKeyword	*keywords;
} LpKeywords;

typedef struct LpPieceNumber {
	int			piece_number;
} LpPieceNumber;

typedef struct LpDefault {
	struct LogicalPlan	*operand[2];
} LpDefault;

/* Extra fields needed by LP_TABLE_JOIN */
typedef struct LpExtraTableJoin {
	enum SqlJoinType	cur_join_type;
	struct LogicalPlan	*join_on_condition;
} LpExtraTableJoin;

/* Extra fields needed by LP_ORDER_BY */
typedef struct LpExtraOrderBy {
	enum OptionalKeyword	direction;		/* OPTIONAL_ASC or OPTIONAL_DESC */
	boolean_t		order_by_column_num;	/* this is an ORDER BY COLUMN NUM usage */
} LpExtraOrderBy;

/* Extra fields needed by LP_ORDER_BY */
typedef struct LpExtraWhere {
	int			num_outer_joins;
} LpExtraWhere;

/* Extra fields needed by LP_INSERT */
typedef struct LpExtraInsert {
	SqlTableAlias		*root_table_alias;	/* The outer most SqlTableAlias structure corresponding to this
							 * logical plan. Needed to forward this to the PhysicalPlan structure.
							 */
	struct LogicalPlan	*first_aggregate;
	struct PhysicalPlan	*physical_plan;		/* Pointer to corresponding physical plan. Note that there could be
							 * multiple physical plans corresponding to the same logical plan
							 * (i.e. many physical plans to 1 logical plan relationship).
							 */
} LpExtraInsert;

/* Extra fields needed by LP_COLUMN_ALIAS */
typedef struct LpExtraColumnAlias {
	struct LogicalPlan	*derived_column;	/* The LP_DERIVED_COLUMN plan corresponding to this LP_COLUMN_ALIAS
							 * if one exists. Otherwise this is NULL.
							 */
} LpExtraColumnAlias;

/* Extra fields needed by LP_DERIVED_COLUMN */
typedef struct LpExtraDerivedColumn {
	SqlColumnAlias		*subquery_column_alias;	/* The original SqlColumnAlias structure (in the LP_COLUMN_ALIAS plan)
							 * that was later replaced by a LP_DERIVED_COLUMN. Needed to know
							 * the type of the original (and in turn the LP_DERIVED_COLUMN) column.
							 * Also needed to determine the GROUP BY column number.
							 */
} LpExtraDerivedColumn;

typedef struct LpExtraAggregateFunction {
	struct LogicalPlan	*next_aggregate;
	int			aggregate_cnt;
	SqlValueType		param_type;	/* Data type (STRING_LITERAL, NUMERIC_LITERAL etc.) of function parameter.
						 * Initialized/Needed only for LP_AGGREGATE_FUNCTION_MIN/LP_AGGREGATE_FUNCTION_MAX
						 * Inherited from corresponding `SqlAggregateFunction->type`.
						 */
} LpExtraAggregateFunction;

typedef struct LpExtraCoerceType {
	SqlValueType		coerce_type;		/* The resulting type of the typecast/coercion operator */
	SqlValueType		pre_coerce_type;	/* The type of the operand before the typecast/coercion operator */
} LpExtraCoerceType;

/* We use yet another triple type here so we can easily traverse the tree to replace tables and WHEREs.
 * Specifically, the WHERE can have complete trees under it, and it would be awkward to overload void pointers.
 */
typedef struct LogicalPlan {
	LPActionType	type;
	int		*counter;
	union {
		/* If any changes to the below union layout happen (new members are added etc.), then code in
		 * `lp_is_bool_operand_type_string.c` will need to be adjusted (search for LOGICAL_PLAN_KEEP_IN_SYNC).
		 */
		LpColumnAlias		lp_column_alias;	// To be used if type == LP_COLUMN_ALIAS
		LpColumnListAlias	lp_column_list_alias;	// To be used if type == LP_COLUMN_LIST_ALIAS
		LpValue			lp_value;		// To be used if type == LP_VALUE
		LpTable			lp_table;		// To be used if type == LP_TABLE
		LpKey			lp_key;			// To be used if type == LP_KEY
		LpKeywords		lp_keywords;		// To be used if type == LP_KEYWORDS
		LpPieceNumber		lp_piece_number;	// To be used if type == LP_PIECE_NUMBER
		LpDefault		lp_default;		// To be used for all other LP_* types
	} v;
	union {
		LpExtraTableJoin		lp_table_join;		// To be used if type == LP_TABLE_JOIN
		LpExtraOrderBy			lp_order_by;		// To be used if type == LP_ORDER_BY
		LpExtraWhere			lp_where;		// To be used if type == LP_WHERE
		LpExtraInsert			lp_insert;		// To be used if type == LP_INSERT
		LpExtraColumnAlias		lp_column_alias;	// To be used if type == LP_COLUMN_ALIAS
		LpExtraDerivedColumn		lp_derived_column;	// To be used if type == LP_DERIVED_COLUMN
		LpExtraAggregateFunction	lp_aggregate_function;	// To be used if type == LP_AGGREGATE_*
		LpExtraCoerceType		lp_coerce_type;		// To be used if type == LP_COERCE_TYPE
	} extra_detail;
} LogicalPlan;

typedef struct SqlKey {
	SqlTable		*table;
	SqlColumn		*column;
	int			key_num;
	int			unique_id;
	// If this key is fixed, this is the value
	LogicalPlan		*fixed_to_value;
	// Table that owns this key; used to extract key from plan
	//  when generating an extract for a given column
	// If this key is part of a UNION, this is the LP_INSERT
	//  plan which outputs this key
	LogicalPlan		*owner;
	// The only relevant types are KEY_FIXED, KEY_ADVANCE
	LPActionType		type;
	// If true, this is an output key for a cross reference
	int			is_cross_reference_key;
	// If this is a cross reference key which is not an output key, this will point to the
	// output key, which we can snag the column name from
	struct SqlKey		*cross_reference_output_key;
	// The source of the cross reference
	SqlColumnAlias		*cross_reference_column_alias;
	// If this is a cross refence key, this value will point to the filename used to store the
	// code to provide the cross reference
	char			*cross_reference_filename;
	boolean_t		skip_column_reference_optimization;	/* TRUE => an optimization in "tmpl_column_reference"
									 * will be skipped. Used by the second half of RIGHT JOIN.
									 */
	int			in_value_list_id;	/* id used during physical plan phase to store value list of IN operator */
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
// As a side effect, this also fills in the linked list of aggregate function (`aggregate` parameter).
int lp_verify_structure(LogicalPlan *plan, LogicalPlan **aggregate);

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
// Returns the output key
LogicalPlan *lp_get_output_key(LogicalPlan *plan);
// Returns the number of columns in the SELECT column list for a given plan
int lp_get_num_cols_in_select_column_list(LogicalPlan *plan);
// Returns the 'n'th LP_COLUMN_LIST in the SELECT column list for a given plan */
LogicalPlan *lp_get_col_num_n_in_select_column_list(LogicalPlan *column_list, int n);
int lp_get_num_cols_in_select_column_list(LogicalPlan *plan);
// Returns the M function name in _ydboctoplanhelpers.m corresponding to the aggregate function type (input parameter `type`)
char	*lp_get_aggregate_plan_helper_func_name(LPActionType type);
// Inserts a key at the end of the plans keys
void lp_insert_key(LogicalPlan *plan, LogicalPlan *key);
// Returns if either operand of a boolean operation logical plan (e.g. LP_BOOLEAN_LESS_THAN) is of type STRING
boolean_t lp_is_bool_operand_type_string(LogicalPlan *plan);
// Returns LP_WHERE with an AND of the two wheres
LogicalPlan *lp_join_where(LogicalPlan *where1, LogicalPlan *where2);
// Returns a new logical plan representing the boolean structure from stmt
LogicalPlan *lp_generate_where(SqlStatement *stmt, int *plan_id, SqlStatement *parent);
// Given a column and a table, generates a cross reference plan and returns it
LogicalPlan *lp_generate_xref_plan(LogicalPlan *plan, SqlTable *table, SqlColumn *column, int unique_id);
/**
 * Returns the keys corresponding to the cross reference for column in table, and updates
 * the LP_TABLE_JOIN of plan to include the plan which needs to be execute to generate the cross
 * reference
 */
LogicalPlan *lp_generate_xref_keys(LogicalPlan *plan, SqlTable *table, SqlColumnAlias *column_alias, SqlTableAlias *table_alias);
// Returns a logical plan representing the provided ColumnListAlias
LogicalPlan *lp_column_list_to_lp(SqlColumnListAlias *list, int *plan_id, boolean_t *caller_error_encountered);
LogicalPlan *lp_replace_derived_table_references(LogicalPlan *root, SqlTableAlias *table_alias, SqlKey *key);
// Given a SET operation, drills down until it encounters the first LP_INSERT statement
LogicalPlan *lp_drill_to_insert(LogicalPlan *plan);

// Given a plan, attempts to calculate a "cost" estimate of that plan
int lp_calculate_plan_cost(LogicalPlan *plan);

// Fills buffer with a representation of the plan and emits it to stderr; Is a no-op if octo/rocto not started with -vv at least
void lp_emit_plan(LogicalPlan *plan, char *stage);

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
void lp_optimize_where_multi_equals_ands(LogicalPlan *plan, LogicalPlan *where,
					SqlTableAlias *right_table_alias, boolean_t num_outer_joins);

// Returns a unique number within the context of this plan;
//  maybe not be unique in terms of global numbers
int get_plan_unique_number(LogicalPlan *plan);

#endif
