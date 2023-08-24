/****************************************************************
 *                                                              *
 * Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.      *
 * All rights reserved.                                         *
 *                                                              *
 *      This source code contains the intellectual property     *
 *      of its copyright holder(s), and is made available       *
 *      under a license.  If you do not know the terms of       *
 *      the license, please stop and do not read further.       *
 *                                                              *
 ****************************************************************/

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

/* Function invoked by the rule named "function_definition" in src/parser.y (parses the CREATE FUNCTION command).
 * Returns
 *	non-NULL pointer to SqlStatement structure on success
 *	NULL on failure
 */
SqlStatement *function_definition(SqlStatement *identifier, SqlStatement *function_parameter_type_list, SqlStatement *data_type,
				  SqlStatement *m_function, boolean_t if_not_exists_specified) {
	SqlStatement *ret;
	SQL_STATEMENT(ret, create_function_STATEMENT);
	MALLOC_STATEMENT(ret, create_function, SqlFunction);

	SqlFunction *function;
	UNPACK_SQL_STATEMENT(function, ret, create_function);
	assert(value_STATEMENT == identifier->type);
	function->function_name = identifier;
	function->parameter_type_list = function_parameter_type_list;
	function->if_not_exists_specified = if_not_exists_specified;
	/* For FUNCTION return type, ignore any size specifications (i.e. if VARCHAR(30) is specified, ignore the 30).
	 * Hence only the "data_type" member is copied over below. "size" member is not copied over.
	 */
	function->return_type = data_type;
	function->extrinsic_function = m_function;
	/* Note:
	 *   "function->function_hash" is initialized later in "run_query.c" (under "case create_function_STATEMENT")
	 *   "function->oid" is initialized later in "store_function_in_pg_proc.c" (at end just before a "compress_statement" call)
	 */

	SqlValue *function_name_value;
	UNPACK_SQL_STATEMENT(function_name_value, function->function_name, value);
	function_name_value->type = FUNCTION_NAME;

	SqlValue *extrinsic_function_value;
	UNPACK_SQL_STATEMENT(extrinsic_function_value, function->extrinsic_function, value);
	extrinsic_function_value->type = FUNCTION_NAME;

	/* See comment in "src/parser/table_definition.c" for why "for" loop that runs 2 iterations is needed */
	int i;
	for (i = 0; i < 2; i++) {
		boolean_t octo929_drop_function = FALSE;
		boolean_t need_iteration_two;

		need_iteration_two = FALSE;
		if (config->is_auto_upgrade_octo929) {
			SqlValue *value;
			char *	  function_name, *orig_name, upper_or_lower_case_name[OCTO_MAX_IDENT + 1];
			UNPACK_SQL_STATEMENT(value, identifier, value);
			orig_name = value->v.string_literal;
			if (!value->is_double_quoted) {
				char *start, *end, *dst, *dst_end;

				start = orig_name;
				end = start + strlen(start);
				dst = upper_or_lower_case_name;
				dst_end = dst + sizeof(upper_or_lower_case_name);
				if (0 == i) {
					TOUPPER(dst, dst_end, start, end);
					need_iteration_two = TRUE; /* for lower case DROP FUNCTION command */
				} else {
					TOLOWER(dst, dst_end, start, end);
				}
				function_name = upper_or_lower_case_name;
			} else {
				function_name = orig_name;
				need_iteration_two = FALSE;
			}

			hash128_state_t state;
			char		function_hash[MAX_ROUTINE_LEN + 1];
			int		status;
			value->v.string_literal = function_name;	 /* temporarily tamper function name for hash computation */
			INVOKE_HASH_CANONICAL_QUERY(state, ret, status); /* "state" holds final hash */
			if (0 != status) {
				return NULL;
			}
			generate_name_type(FunctionHash, &state, 0, function_hash, sizeof(function_hash));
			value->v.string_literal = orig_name; /* Restore original function name */

			ydb_buffer_t ydbocto929, func_subs[2];
			char	     subs0_buff[INT32_TO_STRING_MAX];
			unsigned int data_ret;

			YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTO929, &ydbocto929);
			func_subs[0].buf_addr = subs0_buff;
			func_subs[0].len_alloc = sizeof(subs0_buff);
			func_subs[0].len_used
			    = snprintf(func_subs[0].buf_addr, func_subs[0].len_alloc, "%d", drop_function_STATEMENT);
			YDB_STRING_TO_BUFFER(function_hash, &func_subs[1]);
			status = ydb_data_s(&ydbocto929, 2, &func_subs[0], &data_ret);
			if (YDB_OK != status) {
				YDB_ERROR_CHECK(status);
				return NULL;
			}
			if (0 == data_ret) {
				status = ydb_set_s(&ydbocto929, 2, &func_subs[0], NULL);
				if (YDB_OK != status) {
					YDB_ERROR_CHECK(status);
					return NULL;
				}
				/* If we are loading "octo-seed.sql" and are encountering "CREATE FUNCTION" commands there,
				 * we don't need to worry about deleting the upper case version function as such
				 * commands are included in "octo-seed.sql" itself at the start so skip generating the
				 * "DROP FUNCTION" command in that case.
				 */
				if (!config->in_auto_load_octo_seed) {
					fprintf(config->octo929_sqlfile_stream, "DROP FUNCTION IF EXISTS \"%s\"(", function_name);
					octo929_drop_function = TRUE;
				}
			} else {
				/* We already did a DROP FUNCTION of the upper case name. In that case, don't do a DROP FUNCTION
				 * of the lower case name as it can incorrectly lead to permanently deleting a valid lower
				 * case table name at the end of the auto upgrade.
				 */
				assert(0 == i);
				need_iteration_two = FALSE;
			}
		}

		if (NULL != function->parameter_type_list) {
			SqlParameterTypeList *start_parameter_type, *cur_parameter_type;
			UNPACK_SQL_STATEMENT(start_parameter_type, function->parameter_type_list, parameter_type_list);
			cur_parameter_type = start_parameter_type;

			// Count the number of arguments for later storage in `pg_proc`, issue error if it exceeds YDB_MAX_PARMS
			function->num_args = 0;
			do {
				function->num_args++;
				if (YDB_MAX_PARMS < function->num_args) {
					ERROR(ERR_TOO_MANY_FUNCTION_ARGUMENTS, function_name_value->v.string_literal,
					      YDB_MAX_PARMS);
					return NULL;
				}
				if (octo929_drop_function) {
					char typestr[64]; /* this should be more than enough to hold the type name */

					get_user_visible_data_type_string(&cur_parameter_type->data_type_struct->v.data_type_struct,
									  typestr, sizeof(typestr));
					fprintf(config->octo929_sqlfile_stream, "%s", typestr);
					if (start_parameter_type != cur_parameter_type->next) {
						fprintf(config->octo929_sqlfile_stream, ",");
					}
				}
				cur_parameter_type = cur_parameter_type->next;
			} while (start_parameter_type != cur_parameter_type);
		}

		if (octo929_drop_function) {
			fprintf(config->octo929_sqlfile_stream, ");\n");
		}
		if (!need_iteration_two) {
			break;
		}
	}
	return ret;
}
