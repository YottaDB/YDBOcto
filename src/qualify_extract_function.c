/****************************************************************
 *								*
 * Copyright (c) 2022-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"
#include "octo_type_check.h"

#define COPY_SQLCOLUMNLIST(DST, SRC)                                                                                             \
	{                                                                                                                        \
		(DST)->value = (SRC)->value;                                                                                     \
		(DST)->references = (SRC)->references;                                                                           \
		/* Do NOT copy the linked list, as we want to create a separate list, not merge a new list into and existing one \
		 */                                                                                                              \
	}

/* Helper function that is very similar to "populate_data_type_column_list" in "populate_data_type.c".
 * Cannot use that since this one needs to invoke "qualify_extract_function()" instead of "populate_data_type()".
 */
int qualify_extract_function_column_list(SqlStatement *v, SqlTable *table, SqlValueType *type, boolean_t do_loop,
					 boolean_t is_first_pass, SqlTableAlias *table_alias, SqlStatement *extract_column,
					 SqlColumnList **dependencies) {
	SqlColumnList *column_list, *cur_column_list;
	SqlValueType   current_type;
	int	       result;

	result = 0;
	*type = UNKNOWN_SqlValueType;
	if (NULL != v) {
		SqlValue *extract_column_name;

		// SqlColumnList
		UNPACK_SQL_STATEMENT(column_list, v, column_list);
		cur_column_list = column_list;
		if (is_first_pass) {
			assert(NULL != extract_column);
			UNPACK_SQL_STATEMENT(extract_column_name, extract_column, value);
		} else {
			extract_column_name = NULL;
		}
		do {
			// SqlValue or SqlColumnAlias
			if (is_first_pass) {
				SqlOptionalKeyword *keyword;
				SqlColumn	   *column;
				SqlValue	   *col_name;

				if (value_STATEMENT == cur_column_list->value->type) {
					UNPACK_SQL_STATEMENT(col_name, cur_column_list->value, value);
					if (COLUMN_REFERENCE != col_name->type) {
						/* Circular dependencies are only possible for column references,
						 * not for string literals or other types of literals. In the latter
						 * cases, we don't need to issue an error, so signal that by setting
						 * `col_name` to `NULL` here and skipping the error in that case.
						 */
						col_name = NULL;
					}
				} else {
					assert(column_alias_STATEMENT == cur_column_list->value->type);
					UNPACK_SQL_STATEMENT(
					    col_name, cur_column_list->value->v.column_alias->column->v.column->columnName, value);
				}
				if (NULL != col_name) {
					column = find_column(col_name->v.string_literal, table);
					if (NULL != column) {
						keyword = get_keyword(column, OPTIONAL_EXTRACT);
						/* Only check this column for circular dependencies if it is an EXTRACT column,
						 * since such dependencies are only possible for EXTRACT columns.
						 */
						if (NULL != keyword) {
							if (cur_column_list->qualify_extract_function_cycle
							    < qualify_extract_function_cycle) {
								/* This is the first time this column list node is being accessed
								 * for the current EXTRACT column and so must be qualified. First
								 * note the access by updating `qualify_extract_function_cycle` for
								 * this node, then qualify it below.
								 */
								assert(0 == cur_column_list->qualify_extract_function_cycle);
								cur_column_list->qualify_extract_function_cycle
								    = qualify_extract_function_cycle;
								/* Qualify the arguments to this EXTRACT function in case it is
								 * itself an argument to another EXTRACT function. This is necessary
								 * for detection of circular dependencies across EXTRACT column
								 * definitions.
								 */
								if (0 < cur_column_list->qualify_extract_function_cycle) {
									result |= qualify_extract_function(
									    keyword->v, table, &current_type, is_first_pass,
									    table_alias, extract_column, dependencies);
								}
							} else {
								/* This node has already been visited. That means we have a
								 * dependency cycle in the argument list for the EXTRACT function
								 * currently being qualified. In that case, issue an error and
								 * terminate qualification here.
								 */
								assert(cur_column_list->qualify_extract_function_cycle
								       == qualify_extract_function_cycle);
								ERROR(ERR_CIRCULAR_EXTRACT_DEFINITION,
								      extract_column_name->v.string_literal,
								      col_name->v.string_literal);
								result = 1;
								break;
							}
						}
					}
				}
			}
			current_type = UNKNOWN_SqlValueType;
			result |= qualify_extract_function(cur_column_list->value, table, &current_type, is_first_pass, table_alias,
							   extract_column, dependencies);
			if (result) {
				break;
			}
			cur_column_list = cur_column_list->next;
			*type = current_type;
		} while (do_loop && (cur_column_list != column_list));
	}
	return result;
}

/* Function qualifies the "stmt" parameter (which corresponds to an EXTRACT specification referencing a SQL function call)
 * by verifying that the expression only uses columns from the table passed in the "table" parameter (raises errors
 * as appropriate). And also does type checking (raises errors as appropriate) and returns the type of "stmt" (if
 * appropriate) in the "type" parameter.
 *
 * This one function implements for an EXTRACT SQL function specification what 2 functions ("qualify_statement.c" and
 *"populate_data_type.c") implement for a SELECT query.
 *
 * Returns:
 *	0 if EXTRACT function qualification succeeded.
 *	1 if EXTRACT function had errors during qualification.
 *
 * Note: The below code is modeled on "qualify_statement.c" so it is possible changes here might need to be made there too.
 *       And vice versa (i.e. changes to "qualify_statement.c" might need to be made here too). An automated tool
 *       "tools/ci/check_code_base_assertions.csh" alerts us (through the pre-commit script and/or pipeline jobs)
 *       if these two get out of sync.
 * Note: This code is a lot simpler than "qualify_statement.c" because EXTRACT functions do not allow a lot of cases that
 *       SELECT queries do (e.g. sub queries, aggregate functions etc.).
 * Note: This code cannot invoke "populate_data_type()" for the type checking as that assumes column references have been
 *       already qualified which is not the case for EXTRACT FUNCTIONs as the table owning the column has not yet been created.
 *       Therefore, it duplicates a lot of the flow in "populate_data_type()" but avoids duplicating the type checking logic
 *       by invoking another function for the actual type check (e.g. "binary_operation_data_type_check()" etc.) that is also
 *       invoked by "populate_data_type.c".
 */
int qualify_extract_function(SqlStatement *stmt, SqlTable *table, SqlValueType *type, boolean_t is_first_pass,
			     SqlTableAlias *table_alias, SqlStatement *extract_column, SqlColumnList **dependencies) {
	int result;

	result = 0;
	if (NULL == stmt)
		return result;
	switch (stmt->type) {
	case value_STATEMENT:;
		SqlValue *value;

		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch (value->type) {
		case CALCULATED_VALUE:
			/* This is a function call */
			result |= qualify_extract_function(value->v.calculated, table, type, is_first_pass, table_alias,
							   extract_column, dependencies);
			break;
		case BOOLEAN_VALUE:
		case INTEGER_LITERAL:
		case NUMERIC_LITERAL:
		case STRING_LITERAL:
		case NUL_VALUE:
		case DATE_LITERAL:
		case TIME_LITERAL:
		case TIMESTAMP_LITERAL:
		case TIME_WITH_TIME_ZONE_LITERAL:
		case TIMESTAMP_WITH_TIME_ZONE_LITERAL:
			*type = value->type;
			/* This is a literal inside a EXTRACT FUNCTION. All literals inside the EXTRACT
			 * need to have their parameter index reset to 0 so "tmpl_print_expression.ctemplate"
			 * can generate appropriate M code for EXTRACTs (see comment there for more details).
			 * Note though that it is possible for the parameter index to be already reset if
			 * this EXTRACT had already been processed (due to moving table-level EXTRACTs to
			 * the end of the linked list, it is possible for "table_definition()" to call
			 * "qualify_extract_function()" on the same EXTRACT more than once. Therefore we
			 * cannot assert that "value->parameter_index" is non-zero at this point.
			 */
			value->parameter_index = 0;
			break;
		case FUNCTION_NAME:
			/* No need to do any qualification or type checking in this case */
			break;
		case COLUMN_REFERENCE:;
			/* The below code is similar to "qualify_column_name.c" but is a lot simpler since the only valid
			 * possible columns are those from the current table.
			 */
			char *c, *first_delim, *column_name;

			/* Find the first period; if it is missing, we need to match against all columns in the current table */
			for (c = value->v.string_literal; ('\0' != *c) && ('.' != *c); c++) {
				;
			}
			if ('.' == *c) {
				first_delim = c;
				for (c++; ('\0' != *c) && ('.' != *c); c++) {
					;
				}

				char *table_name;
				int   table_name_len;
				table_name = value->v.reference;
				if ('.' == *c) {
					table_name_len = c - table_name;
					column_name = c + 1;
				} else {
					table_name_len = first_delim - table_name;
					column_name = first_delim + 1;
				}

				SqlValue *tblName;
				int	  tblNameLen;

				UNPACK_SQL_STATEMENT(tblName, table->tableName, value);
				tblNameLen = strlen(tblName->v.reference);
				if ((tblNameLen != table_name_len) || memcmp(tblName->v.reference, table_name, table_name_len)) {
					/* Only the currently being created table name is allowed in column references inside
					 * EXTRACT FUNCTIONs. Issue error otherwise.
					 */
					ERROR(ERR_MISSING_FROM_ENTRY, table_name_len, table_name);
					yyerror(&stmt->loc, NULL, NULL, NULL, NULL, NULL);
					result = 1;
				}
			} else {
				column_name = value->v.reference;
			}
			if (result) {
				break;
			}
			/* Now that we validated the table name (if specified), validate/qualify the column name */
			SqlColumn *match_column;

			match_column = find_column(column_name, table);
			if (NULL == match_column) {
				ERROR(ERR_UNKNOWN_COLUMN_NAME, column_name);
				yyerror(&stmt->loc, NULL, NULL, NULL, NULL, NULL);
				result = 1;
			} else {
				/* Now that column qualification is successful, do data type population for caller */
				*type = get_sqlvaluetype_from_sqldatatype(match_column->data_type_struct.data_type, FALSE);
				if (!is_first_pass) {
					MALLOC_STATEMENT(stmt, column_alias, SqlColumnAlias);
					stmt->type = column_alias_STATEMENT;
					SQL_STATEMENT(stmt->v.column_alias->column, column_STATEMENT);
					stmt->v.column_alias->column->v.column = match_column;
					SQL_STATEMENT(stmt->v.column_alias->table_alias_stmt, table_alias_STATEMENT);
					stmt->v.column_alias->table_alias_stmt->v.table_alias = table_alias;
				}
			}
			break;
		case COERCE_TYPE:
			result |= qualify_extract_function(value->v.coerce_target, table, type, is_first_pass, table_alias,
							   extract_column, dependencies);
			if (result) {
				yyerror(NULL, NULL, &stmt, NULL, NULL, NULL);
			} else {
				/* This code is similar to that in "populate_data_type.c" */
				*type = get_sqlvaluetype_from_sqldatatype(value->u.coerce_type.coerced_type.data_type, FALSE);
			}
			break;
		case FUNCTION_HASH:
		case DELIM_VALUE:
		case IS_NULL_LITERAL:
		case INVALID_SqlValueType:
		case PARAMETER_VALUE:
		case TABLE_ASTERISK:
		case SELECT_ASTERISK:
		case BOOLEAN_OR_STRING_LITERAL:
			/* Even though a literal like 'f' or 't' is specified as a function parameter, it would be
			 * converted to a STRING_LITERAL as part of the 2nd call to "qualify_check_constraint()" in
			 * "function_call_data_type_check()" (see BOOLEAN_OR_STRING_LITERAL handling there). Therefore,
			 * we should never see a BOOLEAN_OR_STRING_LITERAL type of literal here.
			 */
		case UNKNOWN_SqlValueType:
			/* These usages should not be possible inside EXTRACT FUNCTIONs. Assert accordingly. */
			assert(FALSE);
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			result = 1;
			break;
			/* Do not add "default" case as we want to enumerate each explicit case here instead of having a
			 * general purpose bucket where all types not listed above fall into as that could hide subtle bugs.
			 */
		}
		break;
	case function_call_STATEMENT:;
		SqlFunctionCall *fc;

		UNPACK_SQL_STATEMENT(fc, stmt, function_call);
		result |= qualify_extract_function(fc->function_name, table, type, is_first_pass, table_alias, extract_column,
						   dependencies);
		if (result) {
			break;
		}
		result
		    = function_call_data_type_check(stmt, type, NULL, table); /* Note: "fc->parameters" also gets qualified here */
		if (result) {
			break;
		}
		result |= qualify_extract_function_column_list(fc->parameters, table, type, TRUE, is_first_pass, table_alias,
							       extract_column, dependencies);
		/* Note down the function name and hash as encountered in this EXTRACT FUNCTION.
		 * Needed later to store the list of functions that this CREATE TABLE command depends on.
		 * This way a DROP FUNCTION can check if there are any table EXTRACT FUNCTIONs relying on it and if so error out.
		 */
		if (is_first_pass) {
			SqlFunction *function;
			UNPACK_SQL_STATEMENT(function, fc->function_schema, create_function);

			ydb_buffer_t function_name;
			YDB_STRING_TO_BUFFER(function->function_name->v.value->v.string_literal, &function_name);

			ydb_buffer_t ydboctoTblExtract;
			YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOTBLEXTRACT, &ydboctoTblExtract);

			ydb_buffer_t subs[3];
			char	     subs1_buff[sizeof(void *)];
			YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS, &subs[0]);
			subs[1].buf_addr = subs1_buff;
			subs[1].len_alloc = sizeof(subs1_buff);

			int status;
			status = ydb_get_s(&ydboctoTblExtract, 1, &subs[0], &subs[1]);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				result = 1;
				break;
			}
			YDB_STRING_TO_BUFFER(function->function_hash->v.value->v.string_literal, &subs[2]);
			status = ydb_set_s(&ydboctoTblExtract, 3, &subs[0], &function_name);
			assert(YDB_OK == status);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				result = 1;
				break;
			}
		}
		break;
	case aggregate_function_STATEMENT:
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
	case binary_STATEMENT:
	case unary_STATEMENT:
	case array_STATEMENT:
	case coalesce_STATEMENT:
	case greatest_STATEMENT:
	case least_STATEMENT:;
	case null_if_STATEMENT:
	case cas_STATEMENT:
	case cas_branch_STATEMENT:
	case column_list_STATEMENT:
	case column_alias_STATEMENT:
	case column_list_alias_STATEMENT:
	case create_table_STATEMENT:
	case select_STATEMENT:
	case table_value_STATEMENT:
	case insert_STATEMENT:
	case delete_from_STATEMENT:
	case update_STATEMENT:
	case join_STATEMENT:
	case create_function_STATEMENT:
	case drop_table_STATEMENT:
	case drop_function_STATEMENT:
	case truncate_table_STATEMENT:
	case column_STATEMENT:
	case parameter_type_list_STATEMENT:
	case constraint_STATEMENT:
	case keyword_STATEMENT:
	case begin_STATEMENT:
	case commit_STATEMENT:
	case dynamic_sql_STATEMENT:
	case set_STATEMENT:
	case show_STATEMENT:
	case no_data_STATEMENT:
	case delim_char_list_STATEMENT:
	case index_STATEMENT:
	case data_type_struct_STATEMENT:
	case join_type_STATEMENT:
	case discard_all_STATEMENT:
	case row_value_STATEMENT:
	case history_STATEMENT:
	case display_relation_STATEMENT:
	case invalid_STATEMENT:
	case create_view_STATEMENT:
	case drop_view_STATEMENT:
		/* Do not add "default:" case as we want to enumerate each explicit case here instead of having a
		 * general purpose bucket where all types not listed above fall into as that could hide subtle bugs.
		 */
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		assert(FALSE);
		result = 1;
		break;
	}
	return result;
}
