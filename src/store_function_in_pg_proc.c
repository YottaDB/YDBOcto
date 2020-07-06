/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
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
#include <ctype.h>

#include "octo.h"
#include "octo_types.h"
#include "helpers.h"

// Only store type information for parameters up to the max number supported for M calls.
#define ARGUMENT_TYPE_LIST_MAX_LEN (YDB_MAX_PARMS * INT32_TO_STRING_MAX)

#define CLEANUP_AND_RETURN(PG_PROC, OID_BUFFER) \
	{                                       \
		YDB_FREE_BUFFER(&PG_PROC[4]);   \
		return 1;                       \
	}

#define CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, PG_PROC, OID_BUFFER) \
	{                                                             \
		YDB_ERROR_CHECK(STATUS);                              \
		if (YDB_OK != STATUS) {                               \
			CLEANUP_AND_RETURN(PG_PROC, OID_BUFFER);      \
		}                                                     \
	}

PSQL_TypeOid get_psql_type_from_sqldatatype(SqlDataType type) {
	switch (type) {
	case BOOLEAN_TYPE:
		return PSQL_TypeOid_bool;
		break;
	case INTEGER_TYPE:
		return PSQL_TypeOid_int4;
		break;
	case NUMERIC_TYPE:
		return PSQL_TypeOid_numeric;
		break;
	case STRING_TYPE:
		return PSQL_TypeOid_varchar;
		break;
	case UNKNOWN_SqlDataType:
		assert(FALSE);
		break;
	}
	return PSQL_TypeOid_unknown;
}

/* Attempt to store a row in pg_catalog.pg_proc for this function.
 * Note that this function is similar to store_table_in_pg_class.
 */
int store_function_in_pg_proc(SqlFunction *function) {
	SqlParameterTypeList *start_parameter_type;
	SqlParameterTypeList *cur_parameter_type;
	SqlValue *	      value;
	ydb_buffer_t	      oid_buffer[2];
	ydb_buffer_t	      pg_proc[5];
	ydb_buffer_t	      octo_functions[4];
	ydb_buffer_t	      row_buffer;
	long long	      proc_oid;
	int		      status, result;
	int32_t		      arg_type_list_len;
	char *		      function_name;
	char		      row_str[MAX_STR_CONST];
	char		      arg_type_list[ARGUMENT_TYPE_LIST_MAX_LEN];
	char		      proc_oid_str[INT32_TO_STRING_MAX]; /* OIDs are stored as 4-byte unsigned integers:
								  * https://www.postgresql.org/docs/current/datatype-oid.html
								  */

	// Setup pg_proc table node buffers
	YDB_STRING_TO_BUFFER(config->global_names.octo, &pg_proc[0]);
	YDB_STRING_TO_BUFFER(OCTOLIT_TABLES, &pg_proc[1]);
	YDB_STRING_TO_BUFFER(OCTOLIT_PG_CATALOG, &pg_proc[2]);
	YDB_STRING_TO_BUFFER("pg_proc", &pg_proc[3]);
	pg_proc[4].buf_addr = proc_oid_str;
	pg_proc[4].len_alloc = sizeof(proc_oid_str);
	// Setup global OID node buffers
	YDB_STRING_TO_BUFFER(config->global_names.octo, &oid_buffer[0]);
	YDB_STRING_TO_BUFFER(OCTOLIT_OID, &oid_buffer[1]);

	/* Get a unique OID for the passed in function.
	 * 	i.e. $INCREMENT(^%ydboctoocto(OCTOLIT_OID))
	 */
	status = ydb_incr_s(&oid_buffer[0], 1, &oid_buffer[1], NULL, &pg_proc[4]);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}
	pg_proc[4].buf_addr[pg_proc[4].len_used] = '\0';

	// Extract the function name
	UNPACK_SQL_STATEMENT(value, function->function_name, value);
	function_name = value->v.string_literal;

	/* Create "array" (space-delimited list) of function argument types. This format is derived from the results of this query:
	 *	`select proargtypes from pg_catalog.pg_proc;`
	 * Also, count the number of arguments for storage in `pg_proc`.
	 */
	function->num_args = 0;
	arg_type_list_len = 0;
	if (NULL == function->parameter_type_list) { // The parameter type list was empty, so just use the empty string
		arg_type_list[0] = '\0';
	} else {
		UNPACK_SQL_STATEMENT(start_parameter_type, function->parameter_type_list, parameter_type_list);
		cur_parameter_type = start_parameter_type;
		do {
			function->num_args++;
			if (YDB_MAX_PARMS < function->num_args) {
				ERROR(ERR_TOO_MANY_FUNCTION_ARGUMENTS, function_name, YDB_MAX_PARMS);
				return 1;
			}
			/* Note that size/precision modifiers are discarded for CREATE FUNCTION statements,
			 * per https://www.postgresql.org/docs/current/sql-createfunction.html
			 */
			result = snprintf(&arg_type_list[arg_type_list_len], ARGUMENT_TYPE_LIST_MAX_LEN - arg_type_list_len, "%d%s",
					  get_psql_type_from_sqldatatype(cur_parameter_type->data_type->v.data_type),
					  ((start_parameter_type == cur_parameter_type->next) ? "" : " "));
			assert(result < (ARGUMENT_TYPE_LIST_MAX_LEN - arg_type_list_len));
			arg_type_list_len += result;
			cur_parameter_type = cur_parameter_type->next;
		} while (start_parameter_type != cur_parameter_type);
	}
	/* These are hard-coded magic values related to the Postgres catalog.
	 * Many of these simply aren't relevant for Octo as they pertain to features
	 * that aren't implemented.
	 * The columns that are populated by this module are those that are clearly necessary for
	 * the specified function's definition and use:
	 *	proname (function name)
	 *	pronargs (number of arguments)
	 *	prorettype (return type)
	 *	proargtypes (argument data types as a space-delimited list or "array")
	 *	prosrc (source code or reference for the function, in Octo's case, we use the M extrinsic function label)
	 * Columns of `pg_catalog.pg_proc` table in `tests/fixtures/octo-seed.sql`.
	 * Any changes to that table definition will require changes here too.
	 */
	snprintf(row_str, sizeof(row_str), "%s|11|10|12|1|0|0|-|f|f|f|f|f|i|s|%d|0|%d|%s||||||%s|||", function_name,
		 function->num_args, get_psql_type_from_sqldatatype(function->return_type->v.data_type), arg_type_list,
		 function->extrinsic_function->v.value->v.string_literal);
	row_buffer.len_alloc = row_buffer.len_used = strlen(row_str);
	row_buffer.buf_addr = row_str;
	/* Set the function name passed in as having an oid FUNCTIONOID in the pg_catalog.
	 * 	i.e. SET ^%ydboctoocto(OCTOLIT_TABLES,OCTOLIT_PG_CATALOG,"pg_proc",FUNCTIONOID)=...
	 */
	status = ydb_set_s(&pg_proc[0], 4, &pg_proc[1], &row_buffer);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}

	/* Store a cross reference of the FUNCTIONOID in ^%ydboctoocto(OCTOLIT_FUNCTIONS).
	 *	i.e. SET ^%ydboctoocto(OCTOLIT_FUNCTIONS,function_name,OCTOLIT_OID)=FUNCTIONOID
	 * That way a later DROP FUNCTION or CREATE FUNCTION `function_name` can clean all ^%ydboctoocto
	 * nodes created during the previous CREATE FUNCTION `function_name`
	 */
	YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_functions[0]);
	YDB_STRING_TO_BUFFER(OCTOLIT_FUNCTIONS, &octo_functions[1]);
	YDB_STRING_TO_BUFFER(function_name, &octo_functions[2]);
	YDB_STRING_TO_BUFFER(OCTOLIT_OID, &octo_functions[3]);
	status = ydb_set_s(&octo_functions[0], 3, &octo_functions[1], &pg_proc[4]);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	}

	proc_oid = strtoll(pg_proc[4].buf_addr, NULL, 10); /* copy over class OID before we start changing it for column OID */
	if ((LLONG_MIN == proc_oid) || (LLONG_MAX == proc_oid)) {
		ERROR(ERR_LIBCALL, "strtoll");
		return 1;
	}
	function->oid = proc_oid; /* Initialize OID in SqlFunction. Caller later invokes "compress_statement()" that stores
				   * this as part of the binary function definition in the database.
				   */
	return 0;
}
