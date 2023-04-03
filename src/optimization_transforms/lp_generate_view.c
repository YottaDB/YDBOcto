/****************************************************************
 *								*
 * Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

/*
 * This function creates a logical plan for a view and its structure is as shown below.
 *
 * 		 LP_VIEW
 * 		 /     \
 *  LP_SELECT_QUERY  LP_OUTPUT
 *
 * View definition is created only once for a view. All other references to this view will have the same view definition.
 *
 * For example:
 * LP_VIEW structure for each view reference in the following query is different but view definition will be the same.
 * Ex: select (select id from v limit 1), (select v.firstname from v,v as n1 limit 1);
 */

LogicalPlan *lp_generate_view(SqlStatement *stmt, boolean_t *caller_error_encountered) {
	assert(table_alias_STATEMENT == stmt->type);
	SqlTableAlias *table_alias;
	UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);

	SqlView *view;
	UNPACK_SQL_STATEMENT(view, table_alias->table, create_view);
	assert((table_alias_STATEMENT == view->src_table_alias_stmt->type)
	       || (set_operation_STATEMENT == view->src_table_alias_stmt->type));

	LogicalPlan *lp_view_src;
	// Check if the view definition was already created for this view, if not create one
	ydb_buffer_t view_name[2];
	YDB_STRING_TO_BUFFER(view->viewName->v.value->v.string_literal, &view_name[0]);

	ydb_buffer_t ydboctoViewLp;
	YDB_LITERAL_TO_BUFFER("%ydboctoViewLp", &ydboctoViewLp);

	ydb_buffer_t ret;
	char	     retbuff[sizeof(void *)];
	OCTO_SET_BUFFER(ret, retbuff);

	int status;
	status = ydb_get_s(&ydboctoViewLp, 1, &view_name[0], &ret);
	switch (status) {
	case YDB_OK:
		// The definition already exists
		lp_view_src = *((LogicalPlan **)ret.buf_addr);
		break;
	case YDB_ERR_LVUNDEF:;
		// The definition doesn't exist, create a new one
		lp_view_src = generate_logical_plan(view->src_table_alias_stmt);
		/* Optimizing the definition here is necessary. This helps all view references
		 * in queries like the following to refer to the correct view definition. When the
		 * view definition is not optimized here v1 n1 and v1 n2 will refer to different parts
		 * of the DNF LP_SET_OPERATION.
		 * 	CREATE VIEW v1 as SELECT * FROM names n1 WHERE EXISTS (SELECT * FROM names n2) AND (n1.id < 3 OR n1.id > 3);
		 * 	SELECT * FROM v1 n1 WHERE EXISTS (SELECT * FROM v1 n2) AND (n1.id < 3 OR n1.id > 3);
		 */
		lp_view_src = optimize_logical_plan(lp_view_src);
		ydb_buffer_t save_value;
		save_value.buf_addr = (char *)&lp_view_src;
		save_value.len_used = save_value.len_alloc = sizeof(void *);
		status = ydb_set_s(&ydboctoViewLp, 1, &view_name[0], &save_value);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			*caller_error_encountered = TRUE;
			return NULL;
		}
		break;
	default:
		YDB_ERROR_CHECK(status);
		*caller_error_encountered = TRUE;
		return NULL;
		break;
	}

	// Create the LP_VIEW structure with the view definition fetched by above code
	LogicalPlan *lp_view;
	MALLOC_LP_2ARGS(lp_view, LP_VIEW);
	lp_view->extra_detail.lp_view.table_alias = table_alias;
	lp_view->v.lp_default.operand[0] = lp_view_src;
	LogicalPlan *dst;
	MALLOC_LP(dst, lp_view->v.lp_default.operand[1], LP_OUTPUT);
	/* The table_alias is guaranteed to be different for each lp_generate_view() call that uses the same underlying view
	 * definition. For example, in a query select * from v v1, v v2;, the lp_generate_view() call will happen twice for view v.
	 * One call will use the table_alias for v1 and another call will use the table_alias for v2 both of which are guaranteed
	 * to be unique. So it makes sense to use those ids instead of generating a new unique id for each instance of the
	 * same view. Hence the `table_alias->unique_id` argument below.
	 */
	dst->v.lp_default.operand[0]
	    = lp_alloc_key(NULL, NULL, table_alias->unique_id, LP_KEY_ADVANCE, NULL, FALSE, lp_get_output_key(lp_view_src));
	return lp_view;
}
