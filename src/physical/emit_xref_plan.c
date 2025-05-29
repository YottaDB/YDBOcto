/****************************************************************
 *								*
 * Copyright (c) 2021-2025 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <assert.h>

#include "octo.h"
#include "physical_plan.h"
#include "template_helpers.h"

/* Emits an xref plan (_ydboctoX*.m).
 * Returns
 *	0 on sucess.
 *	1 on failure.
 */
int emit_xref_plan(char *plan_filename, char *tableName, char *columnName, PhysicalPlan *xref_plan) {
	ydb_buffer_t table_buff[4];
	FILE	    *memstream, *output_file;
	uint64_t     buffer_index, buffer_len;
	int	     fd, status;
	char	    *buffer;
	char	    *outbuf;
	size_t	     outsize;

	memstream = open_memstream(&outbuf, &outsize);
	if (NULL == memstream) {
		ERROR(ERR_SYSCALL_WITH_ARG, "open_memstream()", errno, strerror(errno), "memstream");
		return 1;
	}

	fprintf(memstream,
		";; This is a generated file; do not modify.\n"
		";; %s\n;; Generated M code maintains cross reference for %s column in %s table\n;; %s\n",
		HYPHEN_LINE, columnName, tableName, HYPHEN_LINE);

	buffer_len = INIT_M_ROUTINE_LENGTH;
	buffer = calloc(buffer_len, sizeof(char));
	buffer_index = 0;

	tmpl_physical_plan(&buffer, &buffer_len, &buffer_index, xref_plan);

	fprintf(memstream, "%s\n", buffer);
	free(buffer);
	fclose(memstream);

	output_file = fopen(plan_filename, "w");
	if (NULL == output_file) {
		ERROR(ERR_SYSCALL_WITH_ARG, "fopen()", errno, strerror(errno), plan_filename);
		free(outbuf);
		return 1;
	}

	SAFE_PRINTF(fprintf, output_file, NULL, NULL, "%s", outbuf);
	free(outbuf);

	fd = fileno(output_file);
	fsync(fd);
	fclose(output_file);

	/* Record the fact that an xref plan got generated for this TABLE so we will know to delete this plan
	 * when a DROP TABLE or CREATE TABLE or DISCARD ALL is done.
	 */
	YDB_STRING_TO_BUFFER(config->global_names.octo, &table_buff[0]);
	YDB_LITERAL_TO_BUFFER(OCTOLIT_TABLEPLANS, &table_buff[1]);
	YDB_STRING_TO_BUFFER(tableName, &table_buff[2]);
	YDB_STRING_TO_BUFFER(plan_filename, &table_buff[3]);
	/* Store gvn that links plan and this table.
	 * Same as SET ^%ydboctoocto("tableplans",TABLENAME,"_ydbocto*.m")=""
	 */
	status = ydb_set_s(&table_buff[0], 3, &table_buff[1], NULL);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	status = store_plandirs_gvn(plan_filename); /* Track this plan in Octo internal gvns */
	return status;
}
