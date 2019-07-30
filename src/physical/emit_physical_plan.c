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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

#include <openssl/conf.h>
#include <openssl/evp.h>
#include <openssl/err.h>

#include "octo.h"
#include "octo_types.h"

#include "logical_plan.h"
#include "physical_plan.h"

#include "template_helpers.h"

#include "mmrhash.h"

void generateHash(EVP_MD_CTX *mdctx, const char *message, size_t message_len, unsigned char **digest, unsigned int *digest_len) {
	if(1 != EVP_DigestInit_ex(mdctx, EVP_md5(), NULL)) {
		FATAL(ERR_LIBSSL_ERROR, "");
	}

	if(1 != EVP_DigestUpdate(mdctx, (const unsigned char*)message, message_len)) {
		FATAL(ERR_LIBSSL_ERROR, "");
	}

	if((*digest = OPENSSL_malloc(EVP_MD_size(EVP_md5()))) == NULL) {
		FATAL(ERR_LIBSSL_ERROR, "");
	}

	if(1 != EVP_DigestFinal_ex(mdctx, *digest, digest_len)) {
		FATAL(ERR_LIBSSL_ERROR, "");
	}
}

int emit_physical_plan(char *sql_query, PhysicalPlan *pplan, char *plan_filename) {
	int plan_id, len, fd;
	PhysicalPlan *cur_plan = pplan, *first_plan;
	char *buffer, plan_name_buffer[MAX_STR_CONST];
	char filename[OCTO_PATH_MAX], *routine_name, *tableName, *columnName;
	char *tmp_plan_filename = NULL;
	unsigned int routine_name_len, plan_filename_len;
	SqlValue *value;
	SqlKey *key;
	FILE *output_file;
	EVP_MD_CTX *mdctx = NULL;
	hash128_state_t state;

	assert(cur_plan != NULL);
	plan_id = 0;
	buffer = malloc(MAX_ROUTINE_LENGTH);
	memset(buffer, 0, MAX_ROUTINE_LENGTH);

	// Walk the plans back to the first
	while(cur_plan->prev != NULL)
		cur_plan = cur_plan->prev;
	first_plan = cur_plan;

	// Output the cross reference plans
	do {
		if(!(cur_plan->outputKey && cur_plan->outputKey->is_cross_reference_key)) {
			cur_plan = cur_plan->next;
			continue;
		}
		if(mdctx == NULL && ((mdctx = EVP_MD_CTX_create()) == NULL)) {
			FATAL(ERR_LIBSSL_ERROR, "");
		}
		key = cur_plan->outputKey;
		UNPACK_SQL_STATEMENT(value, key->table->tableName, value);
		tableName = value->v.reference;
		UNPACK_SQL_STATEMENT (value, key->column->columnName, value);
		columnName = value->v.reference;
		len = snprintf(plan_name_buffer, MAX_STR_CONST, "plan%d", plan_id);
		cur_plan->plan_name = octo_cmalloc(memory_chunks, len+1);
		memcpy(cur_plan->plan_name, plan_name_buffer, len);
		cur_plan->plan_name[len] = '\0';

		HASH128_STATE_INIT(state, 0);
		ydb_mmrhash_128_ingest(&state, (void*)tableName, strlen(tableName));
		ydb_mmrhash_128_ingest(&state, (void*)columnName, strlen(tableName));
		routine_name = octo_cmalloc(memory_chunks, MAX_ROUTINE_LEN);
		routine_name_len = generate_routine_name(&state, routine_name, MAX_ROUTINE_LEN, CrossReference);
		// copy routine name (starts with %)
		if (0 == routine_name_len) {
			FATAL(ERR_PLAN_HASH_FAILED, "");
		}
		// Convert '%' to '_'
		key->cross_reference_filename = routine_name;
		GET_FULL_PATH_OF_GENERATED_M_FILE(filename, &routine_name[1]);	/* updates "filename" to be full path */
		INFO(CUSTOM_ERROR, "Generating helper cross reference M file [%s]", filename);
		output_file = fopen(filename, "w");
		if(output_file == NULL) {
			FATAL(ERR_SYSCALL, "fopen", errno, strerror(errno));
			return FALSE;
		}
		cur_plan->filename = key->cross_reference_filename;
		tmpl_physical_plan(buffer, MAX_ROUTINE_LENGTH, cur_plan);
		assert(output_file != NULL);
		fprintf(output_file, "%s\n", buffer);
		fd = fileno(output_file);
		fsync(fd);
		fclose(output_file);

		cur_plan = cur_plan->next;
	} while(cur_plan != NULL);

	cur_plan = first_plan;
	do {
		while(cur_plan && cur_plan->outputKey && cur_plan->outputKey->is_cross_reference_key) {
			cur_plan = cur_plan->next;
		}
		if(cur_plan == NULL)
			break;
		plan_id++;
		len = snprintf(plan_name_buffer, MAX_STR_CONST, "octoPlan%d", plan_id);
		cur_plan->plan_name = octo_cmalloc(memory_chunks, len+1);
		memcpy(cur_plan->plan_name, plan_name_buffer, len);
		cur_plan->plan_name[len] = '\0';
		cur_plan = cur_plan->next;
	} while(cur_plan != NULL);

	// We should probably get a hash of the input SQL statement and use
	//  that as the plan_id
	plan_id = 0;
	plan_filename_len = strlen(plan_filename);
	tmp_plan_filename = (char*)octo_cmalloc(memory_chunks, plan_filename_len + sizeof(char));
	strncpy(tmp_plan_filename, plan_filename, plan_filename_len + sizeof(char));
	tmp_plan_filename[plan_filename_len-1] = 't';
	output_file = fopen(tmp_plan_filename, "w");
	if(output_file == NULL) {
		FATAL(ERR_SYSCALL, "fopen", errno, strerror(errno));
		return FALSE;
	}

	char *hyphenline = "---------------------------------------------------------", *linestart, *lineend;
	fprintf(output_file,
		";; This is a generated file; do not modify. Generated M code corresponds to below SQL query\n;; %s\n",
		hyphenline);
	// sql_query would contain '\n'; Ensure after every newline, an M comment is printed for the next line of the SQL query
	for (linestart = sql_query; ; )
	{
		lineend = strchr(linestart, '\n');
		if (NULL != lineend)
		{
			fprintf(output_file, ";  %.*s\n", (int)(lineend - linestart), linestart);
			linestart = lineend + 1;	/* + 1 to skip past matching '\n' to go to next line to print */
		} else
		{
			if ('\0' != *linestart)
				fprintf(output_file, ";; %s\n", linestart);
			break;
		}
	}
	fprintf(output_file, ";; %s\n", hyphenline);
	// Emit most of the plans, except for deferred and xref plans
	cur_plan = first_plan;
	do {
		// Skip any plans that are cross references
		while(cur_plan && cur_plan->outputKey && cur_plan->outputKey->is_cross_reference_key) {
			cur_plan = cur_plan->next;
		}
		// Skip any plans that are deferred
		while(cur_plan && cur_plan->deferred_plan) {
			cur_plan = cur_plan->next;
		}
		if(cur_plan == NULL)
			break;
		cur_plan->filename = filename;
		tmpl_physical_plan(buffer, MAX_ROUTINE_LENGTH, cur_plan);
		fprintf(output_file, "%s\n", buffer);
		cur_plan = cur_plan->next;
	} while(cur_plan != NULL);

	// Emit deferred plans
	cur_plan = first_plan;
	do {
		// Skip any plans that are cross references
		while(cur_plan && cur_plan->outputKey && cur_plan->outputKey->is_cross_reference_key) {
			cur_plan = cur_plan->next;
		}
		// Skip any plans that are not deferred and have already been emitted
		while(cur_plan && !cur_plan->deferred_plan) {
			cur_plan = cur_plan->next;
		}
		if(cur_plan == NULL)
			break;
		cur_plan->filename = filename;
		tmpl_physical_plan(buffer, MAX_ROUTINE_LENGTH, cur_plan);
		fprintf(output_file, "%s\n", buffer);
		cur_plan = cur_plan->next;
	} while(cur_plan != NULL);

	// Close out the file
	fd = fileno(output_file);
	fsync(fd);
	fclose(output_file);
	rename(tmp_plan_filename, plan_filename);
	if(mdctx != NULL) {
		EVP_MD_CTX_destroy(mdctx);
	}
	return TRUE;
}
