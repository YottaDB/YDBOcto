/****************************************************************
 *								*
 * Copyright (c) 2019-2025 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>

#include "octo.h"
#include "octo_types.h"

#include "logical_plan.h"
#include "physical_plan.h"
#include "template_helpers.h"

#include "mmrhash.h"

int emit_physical_plan(PhysicalPlan *pplan, char *plan_filename) {
	int		plan_id, len, fd, status;
	uint64_t	buffer_index, buffer_len;
	PhysicalPlan   *cur_plan = pplan, *first_plan, xrefplan, nondeferredplan, deferredplan, *tmp_plan;
	PhysicalPlan   *prev_plan, *next_plan;
	char	       *buffer, plan_name_buffer[MAX_PLAN_NAME_LEN];
	char		filename[OCTO_PATH_MAX];
	char	       *tableName, *columnName;
	char	       *tmp_plan_filename = NULL;
	unsigned int	plan_filename_len;
	SqlValue       *value;
	SqlKey	       *key;
	FILE	       *output_file;
	char	       *linestart, *lineend;
	hash128_state_t state;
	FILE	       *memstream;
	char	       *outbuf;
	size_t		outsize;

	memstream = open_memstream(&outbuf, &outsize);
	if (NULL == memstream) {
		ERROR(ERR_SYSCALL_WITH_ARG, "open_memstream()", errno, strerror(errno), "memstream");
		return 1;
	}

	assert(NULL != cur_plan);
	buffer_len = INIT_M_ROUTINE_LENGTH;
	buffer = calloc(buffer_len, sizeof(char));

	// Walk the plans back to the first
	while (NULL != cur_plan->prev) {
		cur_plan = cur_plan->prev;
	}

	// Reorder the plans in the order (1) Cross reference plans (2) Non-deferred plans and (3) Deferred plans
	// as this is the order in which they are eventually emitted. Preserve the ordering otherwise amongst multiple
	// plans within the same class though.
	xrefplan.prev = NULL;
	xrefplan.next = NULL;
	nondeferredplan.prev = NULL;
	nondeferredplan.next = NULL;
	deferredplan.prev = NULL;
	deferredplan.next = NULL;
	for (; NULL != cur_plan;) {
		if (cur_plan->outputKey && cur_plan->outputKey->is_cross_reference_key)
			tmp_plan = &xrefplan; // Use cross-reference plan linked list
		else if (cur_plan->is_deferred_plan)
			tmp_plan = &deferredplan; // Use deferred plan linked list
		else
			tmp_plan = &nondeferredplan; // Use non-deferred plan linked list
		// Remove cur_plan from current linked list
		if (NULL != cur_plan->prev)
			cur_plan->prev->next = cur_plan->next;
		if (NULL != cur_plan->next)
			cur_plan->next->prev = cur_plan->prev;
		// Insert cur_plan into tmp_plan linked list
		if (NULL == tmp_plan->next)
			tmp_plan->next = cur_plan;
		cur_plan->prev = tmp_plan->prev;
		if (NULL != tmp_plan->prev)
			tmp_plan->prev->next = cur_plan;
		tmp_plan->prev = cur_plan;
		cur_plan = cur_plan->next;
		tmp_plan->prev->next = tmp_plan;
	}
	assert(((NULL == xrefplan.next) && (NULL == xrefplan.prev)) || ((NULL != xrefplan.next) && (NULL != xrefplan.prev)));
	assert((NULL != nondeferredplan.next) && (NULL != nondeferredplan.prev));
	assert(((NULL == deferredplan.next) && (NULL == deferredplan.prev))
	       || ((NULL != deferredplan.next) && (NULL != deferredplan.prev)));
	// We keep the (1) Cross reference plans in a separate linked list AND
	// combine (2) Non-deferred plans and (3) Deferred plans into one linked list.
	first_plan = nondeferredplan.next;
	first_plan->prev = NULL;
	prev_plan = nondeferredplan.prev;
	next_plan = deferredplan.next;
	prev_plan->next = next_plan;
	if (NULL != next_plan) {
		next_plan->prev = prev_plan;
		deferredplan.prev->next = NULL;
	}
	// Output the cross reference plans
	if (NULL != xrefplan.prev)
		xrefplan.prev->next = NULL;
	for (cur_plan = xrefplan.next; NULL != cur_plan; cur_plan = cur_plan->next) {
		char	    *routine_name;
		ydb_string_t ci_routine_name;

		/* Assert that the logical plan corresponding to the xref physical plan points back to this physical plan.
		 * This is because duplicate xref plans are avoided in "generate_physical_plan.c".
		 */
		assert(cur_plan->lp_select_query->extra_detail.lp_select_query.physical_plan == cur_plan);
		assert(cur_plan->outputKey && cur_plan->outputKey->is_cross_reference_key);
		key = cur_plan->outputKey;
		UNPACK_SQL_STATEMENT(value, key->table->tableName, value);
		tableName = value->v.reference;
		UNPACK_SQL_STATEMENT(value, key->column->columnName, value);
		columnName = value->v.reference;
		len = snprintf(plan_name_buffer, MAX_PLAN_NAME_LEN, "%s", XREFPLAN_LIT);
		cur_plan->plan_name = octo_cmalloc(memory_chunks, len + 1);
		memcpy(cur_plan->plan_name, plan_name_buffer, len);
		cur_plan->plan_name[len] = '\0';

		HASH128_STATE_INIT(state, 0);
		ydb_mmrhash_128_ingest(&state, (void *)tableName, strlen(tableName));
		ydb_mmrhash_128_ingest(&state, (void *)columnName, strlen(columnName));
		routine_name = octo_cmalloc(memory_chunks, MAX_ROUTINE_LEN + 1); // + 1 needed for null terminator
		generate_name_type(CrossReference, &state, 0, routine_name, MAX_ROUTINE_LEN + 1);
		// Copy routine name (starts with %)
		cur_plan->filename = routine_name;
		/* The below call updates "filename" to be the full path including "routine_name" at the end */
		status = get_full_path_of_generated_m_file(filename, sizeof(filename), &routine_name[1]);
		if (status) {
			free(buffer);
			return 1;
		}
		status = emit_physical_or_xref_plan(filename, NULL, tableName, columnName, cur_plan);
		if (status) {
			free(buffer);
			return 1;
		}

		/* This calls 'do xrefMetadata@routine_name' to populate AIM xref metadata.
		 * Sets global variable used in src/m_templates/tmpl_key_source_aim.ctemplate.
		 * Only called for global tables, as local tables don't use AIM.
		 */
		if ('^' == key->xref_prefix[0]) {
			ci_routine_name.address = routine_name;
			ci_routine_name.length = strlen(routine_name);
			status = ydb_ci("_ydboctoxrefMetadata", &ci_routine_name);

			// Error will be printed out from YDB_ERROR_CHECK, no need to print another error message.
			if (YDB_OK != status) {
				YDB_ERROR_CHECK(status);
				free(buffer);
				return 1;
			}
		}
	}

	boolean_t regexmatch_invoked = FALSE;
	// Generate plan names for Non-deferred and Deferred plans
	for (plan_id = 1, cur_plan = first_plan; NULL != cur_plan; cur_plan = cur_plan->next) {
		assert(!(cur_plan->outputKey && cur_plan->outputKey->is_cross_reference_key));
		len = snprintf(plan_name_buffer, MAX_PLAN_NAME_LEN, "%s%d", OCTOPLAN_LIT, plan_id);
		cur_plan->plan_name = octo_cmalloc(memory_chunks, len + 1);
		memcpy(cur_plan->plan_name, plan_name_buffer, len);
		cur_plan->plan_name[len] = '\0';
		plan_id++;
		if (cur_plan->regexmatch_invoked)
			regexmatch_invoked = TRUE;
	}

	plan_filename_len = strlen(plan_filename);
	tmp_plan_filename = (char *)octo_cmalloc(memory_chunks, plan_filename_len + sizeof(char));
	strncpy(tmp_plan_filename, plan_filename, plan_filename_len + sizeof(char));
	tmp_plan_filename[plan_filename_len - 1] = 't';

	fprintf(memstream, ";; This is a generated file; do not modify. Generated M code corresponds to below SQL query\n;; %s\n",
		HYPHEN_LINE);
	// input_buffer_combined would contain '\n'; Ensure after every newline, an M comment is printed for the next line of the
	// SQL query
	for (linestart = input_buffer_combined + old_input_index;;) {
		int linelen;

		lineend = strchr(linestart, '\n');
		if ((NULL == lineend) && config->is_rocto) {
			// Clients, notably the JDBC driver, may omit newlines, so look for a null terminator instead
			lineend = strchr(linestart, '\0');
		}
		/* cur_input_index marks the start of the next query do not print past it
		 * if it is null then there is no \n in the rest of the string so also set lineend to cur_input_index
		 */
		if ((NULL == lineend) || (lineend > (input_buffer_combined + cur_input_index)))
			lineend = input_buffer_combined + cur_input_index;
		assert(NULL != lineend);
		linelen = lineend - linestart;
		if (MAX_M_LINE_LEN < (linelen + 4)) { // 4 is the length of ";  \n"
			/* Truncate the query string if it exceeds the maximum M line length and insert ellipsis to indicate
			 * truncation has occurred. Include room for comment syntax, spaces, and ellipsis, so subtract 7 from the
			 * length of the format argument to be printed:
			 *	";  " (3) + "..." (3) + "\n" (1) = 7 bytes
			 */
			fprintf(memstream, ";  %.*s...\n", (int)(MAX_M_LINE_LEN - 7), linestart);
		} else {
			fprintf(memstream, ";  %.*s\n", (int)(linelen), linestart);
		}
		linestart = lineend + 1; /* + 1 to skip past matching '\n' to go to next line to print */
		/* if we hit cur_input_index stop looping */
		if (lineend == (input_buffer_combined + cur_input_index))
			break;
	}
	fprintf(memstream, ";; %s\n", HYPHEN_LINE);
	// Emit meta plan first that invokes all the Non-Deferred plans in sequence
	fprintf(memstream, "\n%s0(cursorId,wrapInTp)\n", OCTOPLAN_LIT);
	/* Emit M code to invoke xref plans first (if needed). This lets us wrap the rest of the query inside TP without TRANS2BIG
	 * errors (which are very likely if xref plans also happen while inside TP). To do that, go through the xref plans
	 * and invoke those cross reference plans.
	 */
	SqlKey *prev_t_key, *t_key;
	prev_t_key = NULL;
	for (cur_plan = xrefplan.next; NULL != cur_plan; cur_plan = cur_plan->next) {
		t_key = cur_plan->outputKey;
		/* If cross-reference-output-key is the same as the previously encountered key or the corresponding
		 * table/column is the same, an xref has already been generated in this physical plan so skip doing
		 * the check again for whether it has been generated or not.
		 */
		if ((NULL == t_key) || (prev_t_key == t_key)
		    || ((NULL != prev_t_key) && (prev_t_key->table == t_key->table) && (prev_t_key->column == t_key->column))) {
			continue;
		}
		prev_t_key = t_key;
		UNPACK_SQL_STATEMENT(value, t_key->table->tableName, value);
		tableName = value->v.reference;
		UNPACK_SQL_STATEMENT(value, t_key->column->columnName, value);
		columnName = value->v.reference;

		/* Global tables (most Octo tables) xref differs from local tables (e.g. pg_settings) */
		if ('^' == t_key->xref_prefix[0]) {
			fprintf(memstream, "    DO:'$GET(%s(\"%s\",\"%s\",\"%s\")) %s^%s(cursorId)\n", OCTOLIT_AIM_OCTO_CACHE,
				tableName, columnName, OCTOLIT_AIM_SUB_COMPLETED, XREFPLAN_LIT, cur_plan->filename);
		} else {
			fprintf(memstream, "    DO:'$DATA(%s%s(\"%s\",\"%s\",\"%s\")) %s^%s(cursorId)\n", t_key->xref_prefix,
				config->global_names.raw_octo, OCTOLIT_XREF_STATUS, tableName, columnName, XREFPLAN_LIT,
				cur_plan->filename);
		}
	}
	/* NEW variables that are used across all plans. Do it only once at the start of plan instead of inside each plan
	 * where the variable is used. Saves on multiple NEWs of the same variable particularly if the NEW happens to be
	 * inside a FOR loop. Note that PP_YDB_OCTO_Z needs to be NEWed inside each plan separately (as opposed to once
	 * across all plans). See YDBOcto#706 for details.
	 */
	fprintf(memstream, "    NEW %s,%s\n", PP_YDB_OCTO_P, PP_YDB_OCTO_EXPR);
	fprintf(memstream, "    TSTART:wrapInTp ():(serial)\n"); /* Wrap post-xref part of query in TP if requested */
	for (cur_plan = first_plan; NULL != cur_plan; cur_plan = cur_plan->next) {
		if (cur_plan->is_deferred_plan)
			break; // if we see a Deferred plan, it means we are done with the Non-Deferred plans
		assert(NULL != cur_plan->plan_name);
		fprintf(memstream, "    DO %s(cursorId)\n", cur_plan->plan_name);

		/* Check if this physical plan corresponds to a SET operation. If so, generate code that does the SET
		 * operation based on the results obtained from the SET operands (which would be queries whose physical
		 * plans are guaranteed to already been generated by the time we come here).
		 */
		SetOperType *set_oper;
		set_oper = cur_plan->set_oper_list;

		SetOperType *prev_oper;
		prev_oper = set_oper;
		assert((NULL == set_oper) || (NULL == set_oper->prev));
		for (; NULL != set_oper;) {
			prev_oper = set_oper;
			set_oper = set_oper->next;
		}
		for (; NULL != prev_oper;) {
			/* Check if at least one of the operands of this SET operation correspond to deferred plans.
			 * If so skip emitting code for the SET operation here. It will be done later just before
			 * the results of this plan are needed (in "tmpl_invoke_deferred_plan()").
			 */
			assert(TRUE == prev_oper->lp_set_operation->extra_detail.lp_set_operation.is_deferred_plan_valid);
			if (prev_oper->lp_set_operation->extra_detail.lp_set_operation.is_deferred_plan) {
#ifndef NDEBUG
				while (TRUE) {
					prev_oper = prev_oper->prev;
					if (NULL == prev_oper) {
						break;
					}
					assert(
					    TRUE
					    == prev_oper->lp_set_operation->extra_detail.lp_set_operation.is_deferred_plan_valid);
					assert(prev_oper->lp_set_operation->extra_detail.lp_set_operation.is_deferred_plan);
				}
#endif
				break;
			}

			char *plan_helper_mlabref;
			plan_helper_mlabref = get_setoper_mlabref(prev_oper, cur_plan);
			if (NULL != plan_helper_mlabref) {
				fprintf(memstream, "    DO %s^%%ydboctoplanhelpers(%d,%d,%d)\n", plan_helper_mlabref,
					prev_oper->input_id1, prev_oper->input_id2, prev_oper->output_id);
			}
			prev_oper = prev_oper->prev;
		}
	}
	fprintf(memstream, "    TCOMMIT:wrapInTp\n"); /* Commit TP (if wrapped) */
	/* Now that a SELECT/INSERT/DELETE etc. query has finished execution, check if there are any regex expression
	 * which were compiled through "$$regexmatch^%ydboctoplanhelpers calls". If so, free them up here to avoid a
	 * memory leak. We do not do this free inside "$$regexmatch^%ydboctoplanhelpers" for performance reasons. See
	 * https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/873#note_1034216975 for more details.
	 */
	if (regexmatch_invoked) { /* There is at least one physical plan that had M code which invoked
				   * $$regexmatch^%ydboctoplanhelpers. Emit M code to free up the memory allocated for regex
				   * compilations. To avoid a memory leak (YDBOcto#873).
				   */
		fprintf(memstream, "    DO regfreeIfAny^%%ydboctoplanhelpers\n");
	}
	fprintf(memstream, "    QUIT\n");
	// Emit Non-Deferred and Deferred plans in that order
	for (cur_plan = first_plan; NULL != cur_plan; cur_plan = cur_plan->next) {
		assert(cur_plan == cur_plan->lp_select_query->extra_detail.lp_select_query.physical_plan);
		cur_plan->filename = NULL; // filename needed only for cross reference plans
		buffer_index = 0;
		tmpl_physical_plan(&buffer, &buffer_len, &buffer_index, cur_plan);
		fprintf(memstream, "%s\n", buffer);
	}
	// Emit M code coresponding to LEFT JOIN body (in case any exists)
	for (cur_plan = first_plan; NULL != cur_plan; cur_plan = cur_plan->next) {
		if (!cur_plan->key_lvn_can_be_zysqlnull) {
			continue;
		}
		LogicalPlan *tablejoin;
		tablejoin = cur_plan->tablejoin;
		for (; NULL != tablejoin; tablejoin = tablejoin->v.lp_default.operand[1]) {
			if (NULL == tablejoin->extra_detail.lp_table_join.left_join_buffer) {
				continue;
			}
			int unique_id;
			unique_id = lp_get_tablejoin_unique_id(tablejoin);
			buffer_index = 0;

			char dnf_num_str[INT32_TO_STRING_MAX];
			if (0 != cur_plan->dnf_num) {
				snprintf(dnf_num_str, sizeof(dnf_num_str), "D%d", cur_plan->dnf_num);
			} else {
				dnf_num_str[0] = '\0';
			}
			fprintf(memstream, "\n%s%d%s;\n", OCTO_LEFT_JOIN_LIT, unique_id, dnf_num_str);
			fprintf(memstream, "%.*s\n    QUIT\n",
				(int)tablejoin->extra_detail.lp_table_join.left_join_save_buffer_index,
				tablejoin->extra_detail.lp_table_join.left_join_buffer);
		}
	}

	fclose(memstream);

	output_file = fopen(tmp_plan_filename, "w");
	if (output_file == NULL) {
		ERROR(ERR_SYSCALL_WITH_ARG, "fopen()", errno, strerror(errno), tmp_plan_filename);
		free(buffer);
		free(outbuf);
		return 1;
	}

	SAFE_PRINTF(fprintf, output_file, FALSE, FALSE, "%s", outbuf);
	// Close out the file
	fd = fileno(output_file);
	fsync(fd);
	fclose(output_file);
	free(outbuf);
	free(buffer);

	rename(tmp_plan_filename, plan_filename);
	status = store_plandirs_gvn(plan_filename); /* Track this plan in Octo internal gvns */
	return status;
}
