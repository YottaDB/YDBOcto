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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <errno.h>

#include "octo.h"
#include "octo_types.h"

#include "logical_plan.h"
#include "physical_plan.h"

#include "template_helpers.h"

#include "mmrhash.h"

int emit_physical_plan(PhysicalPlan *pplan, char *plan_filename) {
	int		plan_id, len, fd, buffer_len, buffer_index, status;
	PhysicalPlan *	cur_plan = pplan, *first_plan, xrefplan, nondeferredplan, deferredplan, *tmp_plan;
	PhysicalPlan *	prev_plan, *next_plan;
	char *		buffer, plan_name_buffer[MAX_STR_CONST];
	char		filename[OCTO_PATH_MAX], objfilename[OCTO_PATH_MAX];
	char		rtnname[MAX_ROUTINE_LEN + 1];
	char *		trigger_name, *tableName, *columnName;
	char *		tmp_plan_filename = NULL;
	unsigned int	plan_filename_len;
	SqlValue *	value;
	SqlKey *	key;
	FILE *		output_file;
	char *		hyphenline = "---------------------------------------------------------", *linestart, *lineend;
	ydb_buffer_t	plandirs_buff[4];
	hash128_state_t state;

	assert(NULL != cur_plan);
	buffer_len = INIT_M_ROUTINE_LENGTH;
	buffer_index = 0;
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
		else if (NULL != cur_plan->deferred_parent_plan)
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
	plan_id = 0;
	for (cur_plan = xrefplan.next; NULL != cur_plan; cur_plan = cur_plan->next, plan_id++) {
		char *routine_name;

		/* Assert that the logical plan corresponding to the xref physical plan points back to this physical plan.
		 * This is because duplicate xref plans are avoided in "generate_physical_plan.c".
		 */
		assert(cur_plan->lp_insert->extra_detail.lp_insert.physical_plan == cur_plan);
		assert(cur_plan->outputKey && cur_plan->outputKey->is_cross_reference_key);
		key = cur_plan->outputKey;
		UNPACK_SQL_STATEMENT(value, key->table->tableName, value);
		tableName = value->v.reference;
		UNPACK_SQL_STATEMENT(value, key->column->columnName, value);
		columnName = value->v.reference;
		len = snprintf(plan_name_buffer, MAX_STR_CONST, "xrefPlan%d", plan_id);
		cur_plan->plan_name = octo_cmalloc(memory_chunks, len + 1);
		memcpy(cur_plan->plan_name, plan_name_buffer, len);
		cur_plan->plan_name[len] = '\0';

		HASH128_STATE_INIT(state, 0);
		ydb_mmrhash_128_ingest(&state, (void *)tableName, strlen(tableName));
		ydb_mmrhash_128_ingest(&state, (void *)columnName, strlen(columnName));
		routine_name = octo_cmalloc(memory_chunks, MAX_ROUTINE_LEN + 1); // + 1 needed for null terminator
		status = generate_routine_name(&state, routine_name, MAX_ROUTINE_LEN, CrossReference);
		// copy routine name (starts with %)
		if (1 == status) {
			ERROR(ERR_PLAN_HASH_FAILED, "");
			free(buffer);
			return 1;
		}
		trigger_name = octo_cmalloc(memory_chunks, MAX_TRIGGER_LEN + 1); // + 1 needed for null terminator
		status = generate_routine_name(&state, trigger_name, MAX_TRIGGER_LEN, YDBTrigger);
		if (1 == status) {
			ERROR(ERR_PLAN_HASH_FAILED, "");
			free(buffer);
			return 1;
		}
		// Convert '%' to '_'
		key->cross_reference_filename = routine_name;
		/* The below call updates "filename" to be the full path including "routine_name" at the end */
		status = get_full_path_of_generated_m_file(filename, sizeof(filename), &routine_name[1]);
		if (status) {
			free(buffer);
			return 1;
		}
		if (access(filename, F_OK) == -1) { // file doesn't exist
			ydb_buffer_t table_buff[4];

			INFO(INFO_GENERATING_XREF, filename, tableName, columnName);
			output_file = fopen(filename, "w");
			if (output_file == NULL) {
				ERROR(ERR_SYSCALL_WITH_ARG, "fopen()", errno, strerror(errno), filename);
				free(buffer);
				return 1;
			}
			cur_plan->filename = key->cross_reference_filename;
			cur_plan->trigger_name = trigger_name;
			fprintf(output_file,
				";; This is a generated file; do not modify.\n"
				";; %s\n;; Generated M code maintains cross reference for %s column in %s table\n;; %s\n",
				hyphenline, columnName, tableName, hyphenline);
			buffer_index = 0;
			tmpl_physical_plan(&buffer, &buffer_len, &buffer_index, cur_plan);
			assert(output_file != NULL);
			fprintf(output_file, "%s\n", buffer);
			fd = fileno(output_file);
			fsync(fd);
			fclose(output_file);
			/* Record the fact that an xref plan got generated for this TABLE so we will know to delete this plan
			 * when a DROP TABLE or CREATE TABLE or DISCARD ALL is done.
			 */
			YDB_STRING_TO_BUFFER(config->global_names.octo, &table_buff[0]);
			YDB_LITERAL_TO_BUFFER(OCTOLIT_TABLEPLANS, &table_buff[1]);
			YDB_STRING_TO_BUFFER(tableName, &table_buff[2]);
			YDB_STRING_TO_BUFFER(filename, &table_buff[3]);
			/* Store gvn that links plan and this table */
			status = ydb_set_s(&table_buff[0], 3, &table_buff[1], NULL);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				free(buffer);
				return 1;
			}
			/* Record the full path of the plan srcdir and plan objdir so a later DROP TABLE or CREATE TABLE
			 * or DISCARD ALL can delete the .o file too when it deletes the .m file.
			 */
			YDB_STRING_TO_BUFFER(config->global_names.octo, &plandirs_buff[0]);
			YDB_LITERAL_TO_BUFFER(OCTOLIT_PLANDIRS, &plandirs_buff[1]);
			YDB_STRING_TO_BUFFER(filename, &plandirs_buff[2]);
			/* The below call updates "objfilename" to be the full path including "routine_name" at the end */
			status = get_full_path_of_generated_o_file(objfilename, sizeof(objfilename), &routine_name[1]);
			if (status) {
				free(buffer);
				return 1;
			}
			YDB_STRING_TO_BUFFER(objfilename, &plandirs_buff[3]);
			/* Store gvn that links srcdir and objdir of the generated plan */
			status = ydb_set_s(&plandirs_buff[0], 3, &plandirs_buff[1], NULL);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				free(buffer);
				return 1;
			}
		}
		// else : File already exists. i.e. cross reference for this tablename and columnname was already generated
		//        as part of this query in a previous plan. No need to regenerate it.
	}

	// Generate plan names for Non-deferred and Deferred plans
	for (plan_id = 1, cur_plan = first_plan; NULL != cur_plan; cur_plan = cur_plan->next) {
		assert(!(cur_plan->outputKey && cur_plan->outputKey->is_cross_reference_key));
		/* Although there can be multiple physical plans corresponding to the same logical plan,
		 * we will emit only one physical plan. So generate plan name only for the first of the duplicates.
		 */
		if (PRIMARY_PHYSICAL_PLAN(cur_plan) == cur_plan) {
			len = snprintf(plan_name_buffer, MAX_STR_CONST, "octoPlan%d", plan_id);
			cur_plan->plan_name = octo_cmalloc(memory_chunks, len + 1);
			memcpy(cur_plan->plan_name, plan_name_buffer, len);
			cur_plan->plan_name[len] = '\0';
			plan_id++;
		}
	}

	plan_filename_len = strlen(plan_filename);
	tmp_plan_filename = (char *)octo_cmalloc(memory_chunks, plan_filename_len + sizeof(char));
	strncpy(tmp_plan_filename, plan_filename, plan_filename_len + sizeof(char));
	tmp_plan_filename[plan_filename_len - 1] = 't';
	output_file = fopen(tmp_plan_filename, "w");
	if (output_file == NULL) {
		ERROR(ERR_SYSCALL_WITH_ARG, "fopen()", errno, strerror(errno), tmp_plan_filename);
		free(buffer);
		return 1;
	}

	fprintf(output_file, ";; This is a generated file; do not modify. Generated M code corresponds to below SQL query\n;; %s\n",
		hyphenline);
	// input_buffer_combined would contain '\n'; Ensure after every newline, an M comment is printed for the next line of the
	// SQL query
	for (linestart = input_buffer_combined + old_input_index;;) {
		lineend = strchr(linestart, '\n');
		/* cur_input_index marks the start of the next query do not print past it
		 * if it is null then there is no \n in the rest of the string so also set lineend to cur_input_index
		 */
		if ((NULL == lineend) || (lineend > (input_buffer_combined + cur_input_index)))
			lineend = input_buffer_combined + cur_input_index;
		assert(NULL != lineend);
		fprintf(output_file, ";  %.*s\n", (int)(lineend - linestart), linestart);
		linestart = lineend + 1; /* + 1 to skip past matching '\n' to go to next line to print */
		/* if we hit cur_input_index stop looping */
		if (lineend == (input_buffer_combined + cur_input_index))
			break;
	}
	fprintf(output_file, ";; %s\n", hyphenline);
	// Emit meta plan first that invokes all the Non-Deferred plans in sequence
	fprintf(output_file, "\noctoPlan0(cursorId)\n");
	for (cur_plan = first_plan; NULL != cur_plan; cur_plan = cur_plan->next) {
		if (NULL != cur_plan->deferred_parent_plan)
			break; // if we see a Deferred plan, it means we are done with the Non-Deferred plans
		/* Note that it is possible we encounter multiple physical plans that map to the same logical plan.
		 * In that case, only the first of those physical plans would have had a name generated. So use that for
		 * all the physical plans we go through.
		 */
		assert(NULL != PHYSICAL_PLAN_NAME(cur_plan));
		fprintf(output_file, "    DO %s(cursorId)\n", PHYSICAL_PLAN_NAME(cur_plan));
	}
	fprintf(output_file, "    QUIT\n");
	// Emit Non-Deferred and Deferred plans in that order
	for (cur_plan = first_plan; NULL != cur_plan; cur_plan = cur_plan->next) {
		if (cur_plan == cur_plan->lp_insert->extra_detail.lp_insert.physical_plan) {
			cur_plan->filename = NULL; // filename needed only for cross reference plans
			buffer_index = 0;
			tmpl_physical_plan(&buffer, &buffer_len, &buffer_index, cur_plan);
			fprintf(output_file, "%s\n", buffer);
		}
		/* else: This physical plan maps to the same logical plan that a prior physical plan points to.
		 *	 Skip emitting this plan as the prior physical plan is good enough.
		 */
	}

	free(buffer);
	// Close out the file
	fd = fileno(output_file);
	fsync(fd);
	fclose(output_file);
	rename(tmp_plan_filename, plan_filename);
	/* Record the full path of the plan srcdir and plan objdir so a later DISCARD ALL can delete the .o file
	 * too when it deletes the .m file.
	 */
	YDB_STRING_TO_BUFFER(config->global_names.octo, &plandirs_buff[0]);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_PLANDIRS, &plandirs_buff[1]);
	YDB_STRING_TO_BUFFER(plan_filename, &plandirs_buff[2]);
	/* Derive the routine name (for the .o file name) from the tail of the .m file name (i.e. "plan_filename") */
	assert((MAX_ROUTINE_LEN + 1) == sizeof(rtnname));
	memcpy(rtnname, plan_filename + plandirs_buff[2].len_used - MAX_ROUTINE_LEN - 2,
	       MAX_ROUTINE_LEN);	 /* - 2 to skip ".m" extension at end */
	rtnname[MAX_ROUTINE_LEN] = '\0'; /* NULL terminate as below call relies on that */
	/* The below call updates "objfilename" to be the full path including "rtnname" at the end */
	status = get_full_path_of_generated_o_file(objfilename, sizeof(objfilename), &rtnname[1]);
	if (status) {
		return 1;
	}
	YDB_STRING_TO_BUFFER(objfilename, &plandirs_buff[3]);
	/* Store gvn that links srcdir and objdir of generated plan */
	status = ydb_set_s(&plandirs_buff[0], 3, &plandirs_buff[1], NULL);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	return YDB_OK;
}
