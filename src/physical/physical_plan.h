/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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

typedef enum PPActionType { PP_PROJECT, PP_DELETE, PP_ADVANCE } PPActionType;

// Some SET operations require special post-condition stuff;
//  this allows us to track what set operation we are doing in
//  in the physical plan
typedef enum PPSetOperation { PP_NOT_SET, PP_UNION_SET, PP_EXCEPT_SET, PP_INTERSECT_SET } PPSetOperation;

typedef struct SetOperType {
	LPActionType	    set_oper_type;
	LogicalPlan *	    lp_set_operation; /* used for deferred plan determination and corresponding M code generation */
	int		    input_id1;
	int		    input_id2;
	int		    output_id;
	struct SetOperType *prev;
	struct SetOperType *next;
} SetOperType;

typedef struct PhysicalPlan {
	char *		     plan_name, *filename;
	struct PhysicalPlan *prev, *next;
	SqlKey *	     iterKeys[MAX_KEY_COUNT]; /* These represent the keys we used to do the iteration */
	SqlKey *	     outputKey;
	LogicalPlan *	     where;		/* WHERE clause */
	LogicalPlan *	     tablejoin;		/* FROM clause */
	LogicalPlan *	     aggregate_options; /* GROUP BY and HAVING */
	SqlOptionalKeyword * keywords;		/* DISTINCT etc. */
	LogicalPlan *	     order_by;		/* ORDER BY clause */
	LogicalPlan *	     projection;
	unsigned int	     total_iter_keys;
	boolean_t	     stash_columns_in_keys; /* If set to 1, this plan should emit the columns
						     * as subscripts of the key, rather than using a row id
						     */
	boolean_t is_cross_reference_key;	/* If true, this plan outputs a cross reference key, and should be treated thusly */
	boolean_t maintain_columnwise_index;	/* If true, maintain a column-wise index of known types */
	boolean_t distinct_values;		/* If true, only add the value to the output key if it doesn't already exist in
						 *  the columnwise index; requires the columnwise index.
						 */
	struct PhysicalPlan *parent_plan;	/* Points to the parent plan of this plan; we need this so we can resolve
						 * references to parent queries and mark intermediate plans as deferred.
						 */
	boolean_t is_deferred_plan;		/* TRUE if this physical plan is a deferred plan */
	boolean_t emit_duplication_check;	/* If true, we should emit code to ensure only one value gets inserted to
						 * the output key for a given set of keys.
						 */
	boolean_t *treat_key_as_null;		/* Set to TRUE for a short period when generating M code for
						 * the case where a left table row did not match any right
						 * table row in an OUTER JOIN (LEFT/RIGHT/FULL). If TRUE, any
						 * references to columns from this table are automatically
						 * treated as NULL (replaced with "") in the generated M code.
						 */
	boolean_t tablejoin_body_group_by_done; /* FALSE initially. Set to TRUE in "tmpl_physical_plan"
						 * once "tmpl_tablejoin_body_group_by" call is done
						 * but "tmpl_group_by" call has not happened.
						 */
	int		     aggregate_function_or_group_by_or_having_specified; /* copy of same field from table_alias */
	SetOperType *	     set_oper_list; /* Linked list of SET OPERATIONS to do on this plan at the end */
	unsigned int	     view_total_iter_keys;
	SqlKey *	     viewKeys[MAX_KEY_COUNT]; /* These represent the keys that map to this pplan */
	struct PhysicalPlan *dnf_prev, *dnf_next;     /* Linked list of plans that are at the same LP_SET_DNF level */
	LogicalPlan *	     lp_select_query;	      /* The owning LP_SELECT_QUERY or LP_TABLE_VALUE or LP_INSERT_INTO
						       * or LP_DELETE_FROM or LP_UPDATE logical plan corresponding to this
						       * physical plan.
						       */
	struct PhysicalPlan *dependent_plans_end;     /* Points to the last physical plan that was added to the linked list of
						       * physical plans as part of the "generate_physical_plan()" that first
						       * generated this "PhysicalPlan" structure. The linked list starting from
						       * the current "PhysicalPlan" structure going back the "prev" links until
						       * "dependent_plans_end" form a list of physical plans that need to be moved
						       * ahead in case we encounter the need for this physical plan again during
						       * "generate_physical_plan()". Moving these plans avoids the need for us to
						       * generate multiple physical plans for the same logical plan i.e. allowing us
						       * to have a 1-1 mapping between physical and logical plans.
						       */
	boolean_t in_where_clause;		      /* Used by generate_physical_plan() to convey to
						       * sub_query_check_and_generate_physical_plan() that this particular
						       * physical plan is currently processing a WHERE clause. Its set in
						       * generate_physical_plan() before processing WHERE clause and
						       * reset after processing it. We cannot use PhysicalPlanOptions to convey
						       * this information because we need to specifically identify if a
						       * particular physical plan is executing a WHERE clause or not.
						       */
} PhysicalPlan;

/* Below macro returns TRUE if GROUP BY or HAVING have been specified and/or Aggregate functions have been used in plan */
#define IS_GROUP_BY_PLAN(PLAN) (PLAN->aggregate_function_or_group_by_or_having_specified)

#define IS_INSERT_INTO_PHYSICAL_PLAN(PPLAN) ((NULL != PPLAN->lp_select_query) && (LP_INSERT_INTO == PPLAN->lp_select_query->type))
#define IS_DELETE_FROM_PHYSICAL_PLAN(PPLAN) ((NULL != PPLAN->lp_select_query) && (LP_DELETE_FROM == PPLAN->lp_select_query->type))
#define IS_UPDATE_PHYSICAL_PLAN(PPLAN)	    ((NULL != PPLAN->lp_select_query) && (LP_UPDATE == PPLAN->lp_select_query->type))
#define HYPHEN_LINE			    "---------------------------------------------------------"

// This provides a convenient way to pass options to subplans
// which need to be aware of a request from a higher level
typedef struct PhysicalPlanOptions {
	// Parent will always represnt the parent of a plan
	struct PhysicalPlan *parent;
	// last_plan will always represent the end of linked list of plans
	struct PhysicalPlan **last_plan;
	struct PhysicalPlan * dnf_plan_next;
	boolean_t	      stash_columns_in_keys;
	LogicalPlan **	      aggregate; /* Helps maintain a linked list of LP_AGGREGATE_FUNC* plans in each query.
					  * Subqueries inside the query maintain their own linked list.
					  */
	LogicalPlan **function;		 /* Helps maintain a linked list of LP_FUNCTION_CALL plans across entire query */
	LogicalPlan **table;		 /* Helps maintain a linked list of LP_TABLE plans across entire query */
	LogicalPlan **view;		 /* Helps maintain a linked list of LP_VIEW plans across entire query */
	LogicalPlan * lp_select_query;	 /* Used by lp_verify_structure() call from generate_physical_plan() to know the
					  * LogicalPlan of the select query which invoked the function. The intention here
					  * is to access the address of pplan associated with this logical plan such that
					  * after generate_physical_plan() the pplan is initialized and it can be used by
					  * tmpl_print_expression.ctemplate to directly use it to fetch data associated with
					  * its unique_id.
					  */
} PhysicalPlanOptions;

PhysicalPlan *generate_physical_plan(LogicalPlan *plan, PhysicalPlanOptions *options);

/* Allocate and initialize (a few fields) a physical plan. Returns the allocated physical plan. */
PhysicalPlan *allocate_physical_plan(LogicalPlan *plan, PhysicalPlanOptions *plan_options, PhysicalPlanOptions *orig_plan_options);

// Outputs physical plans to temporary files located in config.plan_src_dir
//  Names are like ppplanXXXX, where XXXX is a unique number
// Returns TRUE on success
int emit_physical_plan(PhysicalPlan *pplan, char *plan_filename);

// Returns true if the key is a version of this column
int key_equals_column(SqlKey *key, SqlColumn *column);

// Returns true if the unique_id is found in pplan
boolean_t is_unique_id_a_key_of_pplan(PhysicalPlan *pplan, int unique_id);

int	      is_update_keycol_or_xref(PhysicalPlan *pplan);
int	      get_num_key_cols_in_set_clause(PhysicalPlan *pplan);
char *	      get_setoper_mlabref(SetOperType *set_oper, PhysicalPlan *pplan);
PhysicalPlan *get_physical_plan_from_unique_id(PhysicalPlan *pplan, int unique_id);
PhysicalPlan *get_physical_plan_and_key_for_unique_id(PhysicalPlan *pplan, int unique_id, SqlKey **matching_key);
PhysicalPlan *emit_sql_statement(SqlStatement *stmt, char *plan_filename);
int emit_physical_or_xref_plan(char *plan_filename, SqlStatement *stmt, char *tableName, char *columnName, PhysicalPlan *xref_plan);
int emit_xref_plan(char *plan_filename, char *tableName, char *columnName, PhysicalPlan *xref_plan);

#endif
