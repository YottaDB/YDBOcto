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

#ifndef PHYSICAL_PLAN
#define PHYSICAL_PLAN

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

#include "logical_plan.h"

typedef enum PPActionType {
	PP_PROJECT,
	PP_DELETE,
	PP_ADVANCE
} PPActionType;

// Some SET operations require special post-condition stuff;
//  this allows us to track what set operation we are doing in
//  in the physical plan
typedef enum PPSetOperation {
	PP_NOT_SET,
	PP_UNION_SET,
	PP_EXCEPT_SET,
	PP_INTERSECT_SET
} PPSetOperation;

typedef struct SetOperType	{
	LPActionType		set_oper_type;
	int			input_id1;
	int			input_id2;
	int			output_id;
	struct SetOperType	*prev;
	struct SetOperType	*next;
} SetOperType;

typedef struct PhysicalPlan {
	char			*plan_name, *filename, *trigger_name;
	struct PhysicalPlan	*prev, *next;
	// These represent the keys we used to do the iteration
	SqlKey			*iterKeys[MAX_KEY_COUNT];
	SqlKey			*outputKey;
	LogicalPlan		*where;			/* WHERE clause */
	LogicalPlan		*tablejoin;		/* FROM clause */
	LogicalPlan		*aggregate_options;	/* GROUP BY and HAVING */
	SqlOptionalKeyword	*keywords;		/* DISTINCT etc. */
	LogicalPlan		*order_by;		/* ORDER BY clause */
	LogicalPlan		*projection;
	unsigned int		total_iter_keys;
	// If set to 1, this plan should emit the columns as subscripts of the key,
	//  rather than using a row id
	boolean_t		stash_columns_in_keys;
	// If true, this plan outputs a cross reference key, and should be treated thusly
	boolean_t		is_cross_reference_key;

	// If true, maintain a column-wise index of known types
	boolean_t		maintain_columnwise_index;
	// If true, only add the value to the output key if it doesn't already exist in
	//  the columnwise index; requires the columnwise index
	boolean_t		distinct_values;

	// If true, this plan should not be emitted in order but waited until after all required
	//   plans have been emitted first. This is important when the plan will not run in-order,
	//   but will be called as a subquery which can't be extracted due to a parent reference
	boolean_t		deferred_plan;
	// Points to the parent plan of this plan; we need this so we can resolve
	//   references to parent queries and mark intermediate plans as deferred
	struct PhysicalPlan	*parent_plan;
	// If true, we should emit code to ensure only one value gets inserted to the output key
	// for a given set of keys
	boolean_t		emit_duplication_check;
	boolean_t		*treat_key_as_null;	/* Set to TRUE for a short period when generating M code for
							 * the case where a left table row did not match any right
							 * table row in an OUTER JOIN (LEFT/RIGHT/FULL). If TRUE, any
							 * references to columns from this table are automatically
							 * treated as NULL (replaced with "") in the generated M code.
							 */
	boolean_t		tablejoin_body_group_by_done;	/* FALSE initially. Set to TRUE in "tmpl_physical_plan"
								 * once "tmpl_tablejoin_body_group_by" call is done
								 * but "tmpl_group_by" call has not happened.
								 */
	boolean_t		aggregate_function_or_group_by_specified;
	SetOperType		*set_oper_list;		/* Linked list of SET OPERATIONS to do on this plan at the end */
	struct PhysicalPlan	*dnf_prev, *dnf_next;	/* Linked list of plans that are at the same LP_SET_DNF level */
	LogicalPlan		*lp_insert;		/* The owning LP_INSERT logical plan corresponding to this physical plan */
} PhysicalPlan;

/* Below macro returns TRUE if GROUP BY or HAVING have been specified and/or Aggregate functions have been used in plan */
#define	IS_GROUP_BY_PLAN(PLAN) (PLAN->aggregate_function_or_group_by_specified)

// This provides a convenient way to pass options to subplans
// which need to be aware of a request from a higher level
typedef struct {
	struct PhysicalPlan	*parent;
	struct PhysicalPlan	**last_plan;
	struct PhysicalPlan	*dnf_plan_next;
	boolean_t		stash_columns_in_keys;
} PhysicalPlanOptions;

PhysicalPlan *generate_physical_plan(LogicalPlan *plan, PhysicalPlanOptions *options);
// Outputs physical plans to temporary files located in config.tmp_dir
//  Names are like ppplanXXXX, where XXXX is a unique number
// Returns TRUE on success
int emit_physical_plan(PhysicalPlan *pplan, char *plan_filename);

// Returns true if the key is a version of this column
int key_equals_column(SqlKey *key, SqlColumn *column);

int print_temporary_table(SqlStatement *, int cursor_id, void *parms, char *plan_name);

/**
 * Parses query, and calls the callback if it is a select statement. Otherwise, the query is a data altering
 *  query and gets executed
 *
 * @returns TRUE on success, FALSE on failure
 */
int run_query(int (*callback)(SqlStatement *, int, void *, char*), void *parms);

PhysicalPlan	*emit_select_statement(SqlStatement *stmt, char *plan_filename);

#endif
