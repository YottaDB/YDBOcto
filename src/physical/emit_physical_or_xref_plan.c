/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <unistd.h>
#include <assert.h>

#include "octo.h"
#include "physical_plan.h"

#define GET_PLAN_METADATA_DB_NODE(PLAN_FILENAME, DB_NODE_FOUND, STATUS)       \
	{                                                                     \
		ydb_buffer_t varname, subs_array[3];                          \
                                                                              \
		YDB_STRING_TO_BUFFER(config->global_names.octo, &varname);    \
		YDB_LITERAL_TO_BUFFER(OCTOLIT_PLAN_METADATA, &subs_array[0]); \
		subs_array[1] = PLAN_FILENAME;                                \
		YDB_LITERAL_TO_BUFFER(OCTOLIT_OUTPUT_KEY, &subs_array[2]);    \
		STATUS = ydb_data_s(&varname, 3, subs_array, &DB_NODE_FOUND); \
	}

/* Emits M code onto a file (either _ydboctoP*.m or _ydboctoX*.m).
 * Returns
 *	0 for success
 *	non-zero value for failure
 */
int emit_physical_or_xref_plan(char *plan_filename, SqlStatement *stmt, char *tableName, char *columnName,
			       PhysicalPlan *xref_plan) {
	int	      i, output_key_id, status;
	ydb_buffer_t  filename_lock[3];
	PhysicalPlan *pplan;
	ydb_buffer_t  plan_meta[4], value_buffer;

	status = YDB_OK;
	for (i = 0;; i++) {
		/* i = 0 is the iteration BEFORE we get the M lock (to generate the plan).
		 * i = 1 is the iteration AFTER  we get the M lock (to generate the plan).
		 *
		 * The code to do checks is mostly common for both iterations hence this for loop to avoid code duplication.
		 */
		boolean_t    generate_plan;
		SetOperType *set_oper;

		generate_plan = (-1 == access(plan_filename, F_OK));
		if (!generate_plan) {
			/* The plan exists (i.e. has already been generated). But check if the corresponding nodes
			 * in the database are in sync as well. If not, regenerate the plan. This way we will avoid
			 * an ERR_DATABASE_FILES_OOS error later.
			 */
			unsigned int db_node_found;
			ydb_buffer_t filename_buffer;

			YDB_STRING_TO_BUFFER(plan_filename, &filename_buffer);
			GET_PLAN_METADATA_DB_NODE(filename_buffer, db_node_found, status); /* sets "db_node_found" and "status" */
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				break;
			}
			if (0 == db_node_found) {
				/* Plan exists but no corresponding db node was found. Regenerate plan. */
				generate_plan = TRUE;
			}
		}
		if (!generate_plan) {
			/* Plan was found to already exist. So reuse it. */
			INFO(INFO_REUSE_M_PLAN, plan_filename);
			break;
		}
		if (0 == i) {
			/* Get the M lock and redo the check of whether the plan is still not generated */
			YDB_STRING_TO_BUFFER(config->global_names.octo, &filename_lock[0]);
			YDB_STRING_TO_BUFFER(OCTOLIT_FILES, &filename_lock[1]);
			YDB_STRING_TO_BUFFER(plan_filename, &filename_lock[2]);
			/* Wait for 5 seconds in case another process is writing to same filename.
			 * Below does a LOCK +^%ydboctoocto("files","_ydbocto*.m"):5
			 */
			status = ydb_lock_incr_s(TIMEOUT_5_SEC, &filename_lock[0], 2, &filename_lock[1]);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				break;
			}
			continue; /* So we redo the check of whether plan exists or not after getting lock */
		}
		/* We got the lock and the plan still does not exist. Generate the plan this time around. */
		if (NULL != stmt) {
			/* Generate _ydboctoP*.m */

			assert(NULL == tableName);
			assert(NULL == columnName);
			assert(NULL == xref_plan);
			INFO(INFO_M_PLAN, plan_filename);
			pplan = emit_sql_statement(stmt, plan_filename);
			if (NULL == pplan) {
				status = 1; /* non-zero value to signify error */
				break;
			}
		} else {
			/* Generate _ydboctoX*.m */
			assert(NULL != tableName);
			assert(NULL != columnName);
			assert(NULL != xref_plan);
			INFO(INFO_GENERATING_XREF, plan_filename, tableName, columnName);
			emit_xref_plan(plan_filename, tableName, columnName, xref_plan);
			pplan = xref_plan;
		}
		/* Store output key for the given plan. Note that for a LP_INSERT_INTO, there is no output key but we still
		 * note down an output key id of the source query (e.g. SELECT) that way a pre-existing plan gets reused
		 * instead of creating it afresh every time (i.e. "GET_PLAN_METADATA_DB_NODE" check in
		 * "emit_physical_or_xref_plan.c" succeeds and "generate_plan" variable in that function does not get set to TRUE).
		 * Note that we need to do this store as the last step in this function as this global node is checked in
		 * "emit_physical_or_xref_plan.c" as part of the GET_PLAN_METADATA_DB_NODE and if it exists, it is assumed that all
		 * other setup of global nodes related to the plan is done.
		 */
		set_oper = pplan->set_oper_list;
		if (NULL != set_oper) {
			DEBUG_ONLY(LPActionType set_oper_type);

			DEBUG_ONLY(set_oper_type = set_oper->set_oper_type);
			DEBUG_ONLY(assert((LP_SET_UNION == set_oper_type) || (LP_SET_UNION_ALL == set_oper_type)
					  || (LP_SET_DNF == set_oper_type) || (LP_SET_EXCEPT == set_oper_type)
					  || (LP_SET_EXCEPT_ALL == set_oper_type) || (LP_SET_INTERSECT == set_oper_type)
					  || (LP_SET_INTERSECT_ALL == set_oper_type)));
			output_key_id = set_oper->output_id;
		} else if (NULL == pplan->outputKey) {
			LogicalPlan *output_key;

			if (NULL != xref_plan) {
				/* For an xref plan, there is no output key so use key of 0 */
				output_key_id = 0;
			} else {
				assert(IS_INSERT_INTO_PHYSICAL_PLAN(pplan));
				output_key = lp_get_output_key(pplan->lp_select_query);
				output_key_id = output_key->v.lp_key.key->unique_id;
			}
		} else {
			output_key_id = pplan->outputKey->unique_id;
		}
		OCTO_MALLOC_NULL_TERMINATED_BUFFER(&value_buffer, INT32_TO_STRING_MAX);
		OCTO_INT32_TO_BUFFER(output_key_id, &value_buffer);
		// Prepare metadata buffers
		YDB_STRING_TO_BUFFER(config->global_names.octo, &plan_meta[0]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_PLAN_METADATA, &plan_meta[1]);
		YDB_STRING_TO_BUFFER(plan_filename, &plan_meta[2]);
		YDB_LITERAL_TO_BUFFER(OCTOLIT_OUTPUT_KEY, &plan_meta[3]);
		/* Set gvn ^%ydboctoocto("plan_metadata","_ydbocto*.m","output_key") */
		status = ydb_set_s(&plan_meta[0], 3, &plan_meta[1], &value_buffer);
		YDB_FREE_BUFFER(&value_buffer);
		YDB_ERROR_CHECK(status);
		break;
	}
	/* If this is the second iteration, release the lock obtained in first iteration. */
	if (0 != i) {
		int status2;

		status2 = ydb_lock_decr_s(&filename_lock[0], 2, &filename_lock[1]);
		YDB_ERROR_CHECK(status2);
		/* If no primary error, then pass any secondary error as the primary error */
		if (0 == status) {
			status = status2;
		}
	}
	return status;
}
