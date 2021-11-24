/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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

#define MALLOC_LP(RET, DEST, DEST_TYPE)                \
	{                                              \
		OCTO_CMALLOC_STRUCT(RET, LogicalPlan); \
		(RET)->type = DEST_TYPE;               \
		DEST = RET;                            \
	}

#define MALLOC_LP_2ARGS(DEST, DEST_TYPE)               \
	{                                              \
		LogicalPlan *dummyPlan;                \
		MALLOC_LP(dummyPlan, DEST, DEST_TYPE); \
	}

#define GET_LP(DEST, SOURCE, SIDE, DEST_TYPE)                                      \
	{                                                                          \
		assert((SIDE) < 2);                                                \
		assert((SOURCE)->v.lp_default.operand[(SIDE)]->type == DEST_TYPE); \
		(DEST) = (SOURCE)->v.lp_default.operand[(SIDE)];                   \
	}

/* The ERROR_ENCOUNTERED parameter is set to TRUE in case we notice errors inside "lp_generate_where()" (it could recurse
 * and go deep). This is so outermost caller knows to issue an error at logical plan stage and not proceed with physical plan
 * even if one error is seen anywhere in a recursive function call.
 */
#define LP_GENERATE_WHERE(STMT, PARENT_STMT, RET, ERROR_ENCOUNTERED) \
	{                                                            \
		if (NULL != STMT) {                                  \
			RET = lp_generate_where(STMT, PARENT_STMT);  \
			if (NULL == RET)                             \
				ERROR_ENCOUNTERED = TRUE;            \
		} else {                                             \
			RET = NULL;                                  \
		}                                                    \
	}

// Forward declarations
struct LogicalPlan;
struct SqlKey;

#define LP_ACTION_TYPE(name) name,
typedef enum {
#include "lp_action_type.hd"
} LPActionType;
#undef LP_ACTION_TYPE

#define IS_TYPE_LP_AGGREGATE(TYPE)		      ((LP_AGGREGATE_FUNCTION_COUNT_ASTERISK <= TYPE) && (TYPE < LP_AGGREGATE_LAST))
#define IS_LP_COLUMN_ALIAS_OR_LP_DERIVED_COLUMN(PLAN) ((LP_COLUMN_ALIAS == PLAN->type) || (LP_DERIVED_COLUMN == PLAN->type))
#define GET_COLUMN_ALIAS_FROM_LP_COLUMN_ALIAS_OR_LP_DERIVED_COLUMN(PLAN, COLUMN_ALIAS)                                         \
	{                                                                                                                      \
		assert(IS_LP_COLUMN_ALIAS_OR_LP_DERIVED_COLUMN(PLAN));                                                         \
		COLUMN_ALIAS = ((LP_COLUMN_ALIAS == PLAN->type) ? PLAN->v.lp_column_alias.column_alias                         \
								: PLAN->extra_detail.lp_derived_column.subquery_column_alias); \
	}

extern const char *lp_action_type_str[];

/* Fields needed by various LP_* types */
typedef struct LpColumn {
	SqlColumn *column;
} LpColumn;

typedef struct LpColumnAlias {
	SqlColumnAlias *column_alias;
} LpColumnAlias;

typedef struct LpColumnListAlias {
	SqlColumnListAlias *column_list_alias;
} LpColumnListAlias;

typedef struct LpValue {
	SqlValue *value;
} LpValue;

typedef struct LpTable {
	SqlTableAlias *table_alias;
} LpTable;

typedef struct LpKey {
	struct SqlKey *key;
} LpKey;

typedef struct LpKeywords {
	SqlOptionalKeyword *keywords;
} LpKeywords;

typedef struct LpPieceNumber {
	int piece_number;
} LpPieceNumber;

typedef struct LpDefault {
	struct LogicalPlan *operand[2];
} LpDefault;

/* Extra fields needed by LP_TABLE_JOIN */
typedef struct LpExtraTableJoin {
	enum SqlJoinType    cur_join_type;
	struct LogicalPlan *join_on_condition;
} LpExtraTableJoin;

/* Extra fields needed by LP_ORDER_BY */
typedef struct LpExtraOrderBy {
	enum OptionalKeyword direction;		  /* OPTIONAL_ASC or OPTIONAL_DESC */
	int		     order_by_column_num; /* If non-zero, this is an ORDER BY COLUMN NUM usage and the value
						   *   points to the column number in the SELECT column list that this
						   *   ORDER BY corresponds to.
						   * If not an ORDER BY COLUMN NUM usage, it is 0.
						   */
} LpExtraOrderBy;

/* Extra fields needed by LP_ORDER_BY */
typedef struct LpExtraWhere {
	int num_outer_joins;
} LpExtraWhere;

/* Extra fields needed by LP_SELECT_QUERY or LP_TABLE_VALUE */
typedef struct LpExtraInsert {
	SqlTableAlias *root_table_alias;      /* If LP_SELECT_QUERY, this field points to the outer most SqlTableAlias structure
					       * corresponding to this logical plan. Needed to forward this to the
					       * PhysicalPlan structure.
					       * If LP_TABLE_VALUE, this field points to the SqlTableAlias structure
					       * at this level (not outer level) corresponding to this logical plan.
					       */
	struct LogicalPlan * first_aggregate; /* Used only in case of LP_SELECT_QUERY. Not used if LP_TABLE_VALUE */
	struct PhysicalPlan *physical_plan;   /* Pointer to corresponding physical plan. Note that there is only ONE physical
					       * plan corresponding to this logical plan i.e. there is a 1 to 1 mapping.
					       */
	boolean_t to_array;		      // Indicates the result of this LP_SELECT_QUERY should be converted to a SQL array
} LpExtraInsert;

/* Extra fields needed by LP_COLUMN_ALIAS */
typedef struct LpExtraColumnAlias {
	struct LogicalPlan *derived_column; /* The LP_DERIVED_COLUMN plan corresponding to this LP_COLUMN_ALIAS
					     * if one exists. Otherwise this is NULL.
					     */
} LpExtraColumnAlias;

/* Extra fields needed by LP_DERIVED_COLUMN */
typedef struct LpExtraDerivedColumn {
	SqlColumnAlias *subquery_column_alias; /* The original SqlColumnAlias structure (in the LP_COLUMN_ALIAS plan)
						* that was later replaced by a LP_DERIVED_COLUMN. Needed to know
						* the type of the original (and in turn the LP_DERIVED_COLUMN) column.
						* Also needed to determine the GROUP BY column number.
						*/
} LpExtraDerivedColumn;

typedef struct LpExtraAggregateFunction {
	struct LogicalPlan *next_aggregate;
	int		    aggregate_cnt;
	SqlValueType	    param_type; /* Data type (STRING_LITERAL, NUMERIC_LITERAL etc.) of function parameter.
					 * Initialized/Needed only for LP_AGGREGATE_FUNCTION_MIN/LP_AGGREGATE_FUNCTION_MAX
					 * Inherited from corresponding `SqlAggregateFunction->type`.
					 */
} LpExtraAggregateFunction;

typedef struct LpExtraCoerceType {
	SqlDataTypeStruct coerce_type;	   /* The resulting type of the typecast/coercion operator */
	SqlValueType	  pre_coerce_type; /* The type of the operand before the typecast/coercion operator */
} LpExtraCoerceType;

/* When adding a LP_FUNCTION_CALL we need to know if a LP_FUNCTION_CALL plan is already part of the linked list.
 * Since we maintain a singly linked list of these plans, an easy way to know this is if a plan has a non-NULL "next_function" link.
 * But if a plan is the last member of the linked list, it would have a "next_function" link of NULL but is still part
 * of the linked list so we need a special value (that is not NULL and is also not a valid pointer) to denote the end of the
 * linked list. The same issue exists with the LP_TABLE linked list too. Hence the special choice of -1 in the below macro.
 */
#define LP_LIST_END ((LogicalPlan *)-1)

typedef struct LpExtraFunctionCall {
	struct LogicalPlan *next_function; /* maintains linked list of LP_FUNCTION_CALL plans in entire query */
} LpExtraFunctionCall;

typedef struct LpExtraTable {
	struct LogicalPlan *next_table; /* maintains linked list of LP_TABLE plans in entire query */
} LpExtraTable;

typedef struct LpExtraSetOperation {
	boolean_t is_deferred_plan;	    /* TRUE if at least one of the SET operation operands point to deferred plans.
					     * A LP_SET_OPERATION plan can have its 2 operands as LP_SET_OPERATION or
					     * LP_SELECT_QUERY or LP_TABLE_VALUE etc. Except for LP_SET_OPERATION type,
					     * all other types of logical plans correspond to an actual physical plan.
					     * In that case we check the "is_deferred_plan" field in that physical plan.
					     * For the LP_SET_OPERATION type operand, we check the "is_deferred_plan" field
					     * in that logical plan's LpExtraSetOperation type member.
					     */
	boolean_t is_deferred_plan_valid;   /* TRUE if "is_deferred_plan" field has been already initialized.
					     * Need a separate field than "is_deferred_plan" to track this as the
					     * latter can take on FALSE (0) and TRUE (1) as valid values.
					     */
	struct SetOperType *set_oper;	    /* Back pointer to "SetOperType" structure whose "lp_set_operation" field points
					     * to this LP_SET_OPERATION plan.
					     */
	struct PhysicalPlan *physical_plan; /* The physical plan whose "set_oper_list" contains the above "set_oper" field */
} LpExtraSetOperation;

/* We use yet another triple type here so we can easily traverse the tree to replace tables and WHEREs.
 * Specifically, the WHERE can have complete trees under it, and it would be awkward to overload void pointers.
 */
typedef struct LogicalPlan {
	LPActionType type;
	union {
		/* If any changes to the below union layout happen (new members are added etc.), then code in
		 * `lp_is_bool_operand_type_string.c` will need to be adjusted (search for LOGICAL_PLAN_KEEP_IN_SYNC).
		 */
		LpColumn	  lp_column;		// To be used if type == LP_COLUMN
		LpColumnAlias	  lp_column_alias;	// To be used if type == LP_COLUMN_ALIAS
		LpColumnListAlias lp_column_list_alias; // To be used if type == LP_COLUMN_LIST_ALIAS
		LpValue		  lp_value;		// To be used if type == LP_VALUE
		LpTable		  lp_table;		// To be used if type == LP_TABLE
		LpKey		  lp_key;		// To be used if type == LP_KEY
		LpKeywords	  lp_keywords;		// To be used if type == LP_KEYWORDS
		LpPieceNumber	  lp_piece_number;	// To be used if type == LP_PIECE_NUMBER
		LpDefault	  lp_default;		// To be used for all other LP_* types
	} v;
	union {
		LpExtraTableJoin lp_table_join;			// To be used if type == LP_TABLE_JOIN
		LpExtraOrderBy	 lp_order_by;			// To be used if type == LP_ORDER_BY
		LpExtraWhere	 lp_where;			// To be used if type == LP_WHERE
		LpExtraInsert	 lp_select_query;		// To be used if type == LP_SELECT_QUERY or LP_TABLE_VALUE
								//	or LP_INSERT_INTO or LP_DELETE_FROM or LP_UPDATE
		LpExtraColumnAlias	 lp_column_alias;	// To be used if type == LP_COLUMN_ALIAS
		LpExtraDerivedColumn	 lp_derived_column;	// To be used if type == LP_DERIVED_COLUMN
		LpExtraAggregateFunction lp_aggregate_function; // To be used if type == LP_AGGREGATE_*
		LpExtraCoerceType	 lp_coerce_type;	// To be used if type == LP_COERCE_TYPE
		LpExtraFunctionCall	 lp_function_call;	// To be used if type == LP_FUNCTION_CALL
		LpExtraTable		 lp_table;		// To be used if type == LP_TABLE
		LpExtraSetOperation	 lp_set_operation;	// To be used if type == LP_SET_OPERATION
	} extra_detail;
} LogicalPlan;

typedef struct SqlKey {
	SqlTable * table;
	SqlColumn *column;
	int	   key_num;
	int	   unique_id;
	// If this key is fixed, this is the value
	LogicalPlan *fixed_to_value;
	// Table that owns this key; used to extract key from plan
	//  when generating an extract for a given column
	// If this key is part of a UNION, this is the LP_SELECT_QUERY
	//  plan which outputs this key
	LogicalPlan *owner;
	// The only relevant types are KEY_FIXED, KEY_ADVANCE
	LPActionType type;
	// If true, this is an output key for a cross reference
	int is_cross_reference_key;
	/* Indicates whether the table is stored in a YDB global variable (set to "^"). Otherwise, the variable is stored in a YDB
	 * local variable (set to ""). In either case, the node used to store the cross-reference should reflect the type of node
	 * used to store the underlying table. In other words, tables stored in LVNs should have xrefs stored in LVNs as well, to
	 * prevent cross-process data leaks (and incorrect/inconsistent results) that would arise from using GVNs to store such
	 * xrefs.
	 *
	 * Note that this is only relevant when is_cross_reference_key is TRUE, and should be ignored otherwise.
	 */
	char *xref_prefix;
	// If this is a cross reference key which is not an output key, this will point to the
	// output key, which we can snag the column name from
	struct SqlKey *cross_reference_output_key;
	// The source of the cross reference
	SqlColumnAlias *cross_reference_column_alias;
	// If this is a cross refence key, this value will point to the filename used to store the
	// code to provide the cross reference
	char *	  cross_reference_filename;
	boolean_t use_tmpl_emit_source_for_xref_key; /* TRUE => "tmpl_column_reference" will use "tmpl_emit_source" instead of
						      * "tmpl_key" to generate the column value for this non-key column.
						      * Used by the second half of RIGHT JOIN.
						      */
	int in_value_list_id;			     /* id used during physical plan phase to store value list of IN operator */
} SqlKey;

// Helper functions

// Generates a base plan given a SELECT statement
LogicalPlan *generate_logical_plan(SqlStatement *stmt);
LogicalPlan *optimize_logical_plan(LogicalPlan *plan);

// Generate a logical plan for a SET operation
LogicalPlan *lp_generate_set_logical_plan(SqlStatement *stmt);

// Provides a copy of the plan
LogicalPlan *lp_copy_plan(LogicalPlan *plan);
// Copies the SqlKey into a new key
SqlKey *lp_copy_key(SqlKey *key);

// Returns the projection triple
LogicalPlan *lp_get_project(LogicalPlan *plan);
// Returns the select triple
LogicalPlan *lp_get_select(LogicalPlan *plan);
// Returns the key for the given LP
LogicalPlan *lp_get_select_key(LogicalPlan *plan, SqlKey *key);
// Returns the TABLE_JOIN statement for the given LP
LogicalPlan *lp_get_table_join(LogicalPlan *plan);

/* Returns the unique_id corresponding to the LP_TABLE/LP_TABLE_VALUE/LP_SELECT_QUERY/LP_SET_OPERATION plan inside a LP_TABLE_JOIN
 */
int lp_get_tablejoin_unique_id(LogicalPlan *plan);

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
// Returns the 'n'th LP_COLUMN_LIST in the SELECT column list for a given plan
LogicalPlan *lp_get_col_num_n_in_select_column_list(LogicalPlan *column_list, int n);
int	     lp_get_num_cols_in_select_column_list(LogicalPlan *plan);
// Returns the M function name in _ydboctoplanhelpers.m corresponding to the aggregate function type (input parameter `type`)
char *lp_get_aggregate_plan_helper_func_name(LPActionType type);

// Returns "unique_id" given a LP_COLUMN_ALIAS or LP_DERIVED_COLUMN logical plan
int lp_get_unique_id_from_lp_column_alias_or_lp_derived_column(LogicalPlan *plan);

// Inserts a key at the end of the plans keys
void lp_insert_key(LogicalPlan *plan, LogicalPlan *key);
// Returns if either operand of a boolean operation logical plan (e.g. LP_BOOLEAN_LESS_THAN) is of type STRING
boolean_t lp_is_operand_type_string(LogicalPlan *plan);
// Returns LP_WHERE with an AND of the two wheres
LogicalPlan *lp_join_where(LogicalPlan *where1, LogicalPlan *where2);
// Returns a new logical plan representing the boolean structure from stmt
LogicalPlan *lp_generate_where(SqlStatement *stmt, SqlStatement *parent_stmt);
// Given a column and a table, generates a cross reference plan and returns it
LogicalPlan *lp_generate_xref_plan(SqlTable *table, SqlColumn *column, int unique_id);
/**
 * Returns the keys corresponding to the cross reference for column in table, and updates
 * the LP_TABLE_JOIN of plan to include the plan which needs to be execute to generate the cross
 * reference
 */
LogicalPlan *lp_generate_xref_keys(LogicalPlan *plan, SqlTable *table, SqlColumnAlias *column_alias, SqlTableAlias *table_alias);
// Returns a logical plan representing the provided ColumnListAlias
LogicalPlan *lp_column_list_to_lp(SqlColumnListAlias *list, boolean_t *caller_error_encountered);
LogicalPlan *lp_replace_derived_table_references(LogicalPlan *root, SqlTableAlias *table_alias, SqlKey *key);
// Given a SET operation, drills down until it encounters the first LP_SELECT_QUERY statement
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

/* Attempts to optimize any CROSS JOINs in the query */
void lp_optimize_cross_join(LogicalPlan *plan, LogicalPlan *table_join, LogicalPlan *where);

/**
 * Attempts to optimize there WHERE statement which contains nothing but items like
 *   "X = Y AND Y = Z AND Z = A"
 */
void lp_optimize_where_multi_equals_ands(LogicalPlan *plan, LogicalPlan *where, SqlTableAlias *right_table_alias,
					 boolean_t num_outer_joins);

LogicalPlan *lp_optimize_where_multi_equals_ands_helper(LogicalPlan *plan, LogicalPlan *where, int *key_unique_id_array, void *ptr,
							boolean_t num_outer_joins);

/**
 * Walk through the column list, converting each right side value as appropriate.
 * "ret" is an output parameter. This function sets `*ret` to the first (in linked list) LP_COLUMN_LIST on return.
 * "parent_stmt" points to parent SqlStatement structure and is used only in rare cases (EXISTS operator etc.).
 * "start_columns" points to the start of the column list.
 * Returns whether an error was encountered. 0 if no error was encountered. 1 if an error was encountered.
 * This function can only be used for column lists with at least one column.
 */
boolean_t lp_generate_column_list(LogicalPlan **ret, SqlStatement *parent_stmt, SqlColumnList *start_columns);

/* Generates a LP_TABLE_VALUE table plan corresponding to a VALUES() clause specification and returns it */
LogicalPlan *lp_generate_table_value(SqlStatement *stmt, boolean_t *caller_error_encountered);

// Creates and returns a new/unique plan id
int get_new_plan_unique_id();

#endif
