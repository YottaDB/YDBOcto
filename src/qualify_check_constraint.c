/****************************************************************
 *								*
 * Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	*
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

/* Helper function that is very similar to "populate_data_type_column_list" in "populate_data_type.c".
 * Cannot use that since this one needs to invoke "qualify_check_constraint()" instead of "populate_data_type()".
 */
int qualify_check_constraint_column_list(SqlStatement *v, SqlTable *table, SqlValueType *type, SqlValueType *fix_type,
					 DataTypeCallback callback) {
	SqlColumnList *column_list, *cur_column_list;
	SqlValueType   current_type;
	int	       result;

	result = 0;
	*type = UNKNOWN_SqlValueType;
	if (NULL != v) {
		SqlStatement *first_value;
		boolean_t     saw_boolean_or_string_literal;

		// SqlColumnList
		UNPACK_SQL_STATEMENT(column_list, v, column_list);
		cur_column_list = column_list;
		first_value = NULL; /* needed to appease static code checkers from false warnings */
		saw_boolean_or_string_literal = FALSE;
		do {
			// SqlValue or SqlColumnAlias
			current_type = UNKNOWN_SqlValueType;
			result |= qualify_check_constraint(cur_column_list->value, table, &current_type, fix_type);
			if (result) {
				break;
			}
			if (BOOLEAN_OR_STRING_LITERAL == current_type) {
				saw_boolean_or_string_literal = TRUE;
			}
			if (UNKNOWN_SqlValueType != *type) {
				if (NULL != callback) {
					result |= callback(type, &current_type, first_value, cur_column_list->value, NULL);
					if (result) {
						break;
					}
				}
			} else {
				first_value = cur_column_list->value;
			}
			cur_column_list = cur_column_list->next;
			*type = current_type;
		} while (cur_column_list != column_list);
		if (!result && saw_boolean_or_string_literal && (BOOLEAN_OR_STRING_LITERAL != current_type)) {
			SqlValueType fix_type2;

			assert(current_type == *type);
			assert((BOOLEAN_VALUE == current_type) || (STRING_LITERAL == current_type));
			fix_type2 = current_type;
			cur_column_list = column_list;
			do {
				result = qualify_check_constraint(cur_column_list->value, table, &current_type, &fix_type2);
				assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it is 2nd
						    call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				cur_column_list = cur_column_list->next;
			} while (cur_column_list != column_list);
		}
	}
	return result;
}

/* Function qualifies the "stmt" parameter (which corresponds to a column level or table level CHECK constraint)
 * by verifying that the expression only uses columns from the table passed in the "table" parameter (raises errors
 * as appropriate). And also does type checking (raises errors as appropriate) and returns the type of "stmt" (if
 * appropriate) in the "type" parameter.
 *
 * This one function implements for the CHECK constraint what 2 functions ("qualify_statement.c" and "populate_data_type.c")
 * implement for a SELECT query.
 *
 * Returns:
 *	0 if CHECK constraint qualification succeeded.
 *	1 if CHECK constraint had errors during qualification.
 *
 * Note: The below code is modeled on "qualify_statement.c" so it is possible changes here might need to be made there too.
 *       And vice versa (i.e. changes to "qualify_statement.c" might need to be made here too). An automated tool
 *       "tools/ci/check_code_base_assertions.csh" alerts us (through the pre-commit script and/or pipeline jobs)
 *       if these two get out of sync.
 * Note: This code is a lot simpler than "qualify_statement.c" because CHECK constraints do not allow a lot of cases that
 *       SELECT queries do (e.g. sub queries, aggregate functions etc.).
 * Note: This code cannot invoke "populate_data_type()" for the type checking as that assumes column references have been
 *       already qualified which is not the case for CHECK constraints as the table owning the column has not yet been created.
 *       Therefore, it duplicates a lot of the flow in "populate_data_type()" but avoids duplicating the type checking logic
 *       by invoking another function for the actual type check (e.g. "binary_operation_data_type_check()" etc.) that is also
 *       invoked by "populate_data_type.c".
 */
int qualify_check_constraint(SqlStatement *stmt, SqlTable *table, SqlValueType *type, SqlValueType *fix_type) {
	int	     result;
	SqlValueType child_type[2];

	result = 0;
	if (NULL == stmt)
		return result;
	switch (stmt->type) {
	case aggregate_function_STATEMENT:
		/* Aggregate functions are not allowed in CHECK constraints. Issue error. */
		ERROR(ERR_AGGREGATE_FUNCTION_CHECK, NULL);

		SqlAggregateFunction *af;
		UNPACK_SQL_STATEMENT(af, stmt, aggregate_function);
		yyerror(&af->parameter->loc, NULL, NULL, NULL, NULL, NULL);
		result = 1;
		break;
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
		/* Sub queries are not allowed in CHECK constraints. Issue error. */
		ERROR(ERR_SUBQUERY_CHECK, NULL);
		yyerror(&stmt->loc, NULL, NULL, NULL, NULL, NULL);
		result = 1;
		break;
	case value_STATEMENT:;
		SqlValue *value;

		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch (value->type) {
		case CALCULATED_VALUE:
			/* This is a function call */
			result |= qualify_check_constraint(value->v.calculated, table, type, NULL);
			break;
		case BOOLEAN_OR_STRING_LITERAL:
			if (NULL != fix_type) {
				if (BOOLEAN_VALUE == *fix_type) {
					FIX_TYPE_TO_BOOLEAN_VALUE(value->type);
					value->v.string_literal = (value->u.bool_or_str.truth_value ? "1" : "0");
				} else {
					assert(STRING_LITERAL == *fix_type);
					FIX_TYPE_TO_STRING_LITERAL(value->type);
				}
			}
			/* Note: Below comment is needed to avoid gcc [-Wimplicit-fallthrough=] warning */
			/* fall through */
		case BOOLEAN_VALUE:
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case STRING_LITERAL:
		case NUL_VALUE:
		case DATE_LITERAL:
		case TIME_LITERAL:
		case TIME_WITH_TIME_ZONE_LITERAL:
		case TIMESTAMP_LITERAL:
		case TIMESTAMP_WITH_TIME_ZONE_LITERAL:
			*type = value->type;
			/* This is a literal inside a CHECK constraint. All literals inside the constraint
			 * need to have their parameter index reset to 0 so "tmpl_print_expression.ctemplate"
			 * can generate appropriate M code for constraints (see comment there for more details).
			 * Note though that it is possible for the parameter index to be already reset if
			 * this constraint had already been processed (due to moving table-level constraints to
			 * the end of the linked list, it is possible for "table_definition()" to call
			 * "qualify_check_constraint()" on the same constraint more than once. Therefore we
			 * cannot assert that "value->parameter_index" is non-zero at this point.
			 */
			value->parameter_index = 0;
			break;
		case FUNCTION_NAME:
			/* No need to do any qualification or type checking in this case */
			break;
		case TABLE_ASTERISK:
			ERROR(ERR_INVALID_CONSTRAINT_EXPRESSION, "TABLENAME.*")
			yyerror(&stmt->loc, NULL, NULL, NULL, NULL, NULL);
			result = 1;
			break;
		case PARAMETER_VALUE:
			/* These usages are not supported inside CHECK constraints. Issue syntax error. */
			ERROR(ERR_INVALID_CONSTRAINT_EXPRESSION, "Prepared statement")
			yyerror(&stmt->loc, NULL, NULL, NULL, NULL, NULL);
			result = 1;
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
					/* Only the name of the table currently being created is allowed in column references inside
					 * CHECK constraints. Issue error otherwise.
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
				/* Note down the column name as encountered in this CHECK constraint. */
				ydb_buffer_t ydboctoTblConstraint;
				ydb_buffer_t subs[2];
				int	     status;

				YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOTBLCONSTRAINT, &ydboctoTblConstraint);
				YDB_LITERAL_TO_BUFFER(OCTOLIT_COLUMNS, &subs[0]);
				YDB_STRING_TO_BUFFER(column_name, &subs[1]);
				status = ydb_set_s(&ydboctoTblConstraint, 2, &subs[0], NULL);
				assert(YDB_OK == status);
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					result = 1;
				} else {
					/* Now that column qualification is successful, do data type population for caller */
					*type = get_sqlvaluetype_from_sqldatatype(match_column->data_type_struct.data_type, FALSE);
				}
			}
			break;
		case COERCE_TYPE:
			result |= qualify_check_constraint(value->v.coerce_target, table, &value->u.coerce_type.pre_coerced_type,
							   NULL);
			if (BOOLEAN_OR_STRING_LITERAL == value->u.coerce_type.pre_coerced_type) {
				value->u.coerce_type.pre_coerced_type = STRING_LITERAL;
			}
			if (result) {
				// Any errors would have already been issued by the qualify_check_constraint() call immediately
				// above
			} else {
				/* This code is similar to that in "populate_data_type.c" */
				*type = get_sqlvaluetype_from_sqldatatype(value->u.coerce_type.coerced_type.data_type, FALSE);
			}
			break;
		case FUNCTION_HASH:
		case DELIM_VALUE:
		case IS_NULL_LITERAL:
		case SELECT_ASTERISK:
		case INVALID_SqlValueType:
		case UNKNOWN_SqlValueType:
			/* These usages should not be possible inside CHECK constraints. Assert accordingly. */
			assert(FALSE);
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			result = 1;
			break;
			/* Do not add "default" case as we want to enumerate each explicit case here instead of having a
			 * general purpose bucket where all types not listed above fall into as that could hide subtle bugs.
			 */
		}
		break;
	case binary_STATEMENT:;
		SqlBinaryOperation *binary;
		boolean_t	    is_in_column_list;

		/* Note: The below code is similar to that in populate_data_type.c. Any changes here might need to be done there. */
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		result |= qualify_check_constraint(binary->operands[0], table, &child_type[0], NULL);
		if (result) {
			break;
		}
		if (((BOOLEAN_IN == binary->operation) || (BOOLEAN_NOT_IN == binary->operation))
		    && (column_list_STATEMENT == binary->operands[1]->type)) {
			// SqlColumnList
			result |= qualify_check_constraint_column_list(binary->operands[1], table, &child_type[1], NULL,
								       ensure_same_type);
			is_in_column_list = TRUE;
		} else {
			// SqlStatement (?)
			result |= qualify_check_constraint(binary->operands[1], table, &child_type[1], NULL);
			is_in_column_list = FALSE;
		}
		if (result) {
			break;
		}
		if (BOOLEAN_OR_STRING_LITERAL == child_type[0]) {
			SqlValueType fix_type2;

			if (BOOLEAN_OR_STRING_LITERAL == child_type[1]) {
				/* Note: The intention was to model the below "if" as a "switch" just like it is in
				 * "populate_data_type.c" under the "case binary_STATEMENT" block.
				 * But doing so causes pre-commit hook failure in tools/ci/check_code_base_assertions.sh
				 * as this file needs to have similar layout of "case" blocks with qualify_statement.c
				 * Therefore we do an "if" check below (and not a "switch/case").
				 */
				if ((BOOLEAN_OR == binary->operation) || (BOOLEAN_AND == binary->operation)
				    || (BOOLEAN_IS == binary->operation) || (BOOLEAN_IS_NOT == binary->operation)) {
					fix_type2 = BOOLEAN_VALUE;
				} else {
					fix_type2 = STRING_LITERAL;
				}
				result |= qualify_check_constraint(binary->operands[0], table, &child_type[0], &fix_type2);
				assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it is 2nd
						    call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(fix_type2 == child_type[0]);
				if (is_in_column_list) {
					result |= qualify_check_constraint_column_list(binary->operands[1], table, &child_type[1],
										       &fix_type2, NULL);
				} else {
					result |= qualify_check_constraint(binary->operands[1], table, &child_type[1], &fix_type2);
				}
				assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it is 2nd
						    call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(fix_type2 == child_type[1]);
			} else {
				if (BOOLEAN_VALUE == child_type[1]) {
					fix_type2 = BOOLEAN_VALUE;
				} else {
					fix_type2 = STRING_LITERAL;
				}
				result |= qualify_check_constraint(binary->operands[0], table, &child_type[0], &fix_type2);
				assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it is 2nd
						    call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(fix_type2 == child_type[0]);
			}
		} else if (BOOLEAN_OR_STRING_LITERAL == child_type[1]) {
			SqlValueType fix_type2;

			if (BOOLEAN_VALUE == child_type[0]) {
				fix_type2 = BOOLEAN_VALUE;
			} else {
				fix_type2 = STRING_LITERAL;
			}
			if (is_in_column_list) {
				result |= qualify_check_constraint_column_list(binary->operands[1], table, &child_type[1],
									       &fix_type2, NULL);
			} else {
				result |= qualify_check_constraint(binary->operands[1], table, &child_type[1], &fix_type2);
			}
			assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it is 2nd call */
			UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
			/* Note that if binary->operands[1] points to a sub-query, then the above would have fixed the
			 * BOOLEAN_OR_STRING_LITERAL type to be STRING_LITERAL irrespective of what "fix_type2" was.
			 * Hence the below assert.
			 */
			assert((fix_type2 == child_type[1]) || (STRING_LITERAL == child_type[1]));
		}
		assert(BOOLEAN_OR_STRING_LITERAL != child_type[0]);
		assert(BOOLEAN_OR_STRING_LITERAL != child_type[1]);
		result = binary_operation_data_type_check(binary, child_type, type, NULL);
		break;
	case unary_STATEMENT:;
		SqlUnaryOperation *unary;
		boolean_t	   is_boolean_or_string_literal;

		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		result |= qualify_check_constraint(unary->operand, table, &child_type[0], NULL); /* Sets child_type[0] */
		if (result) {
			break;
		}
		is_boolean_or_string_literal = (BOOLEAN_OR_STRING_LITERAL == child_type[0]);
		result = unary_operation_data_type_check(unary, child_type, type); /* Sets "*type" */
		assert(BOOLEAN_OR_STRING_LITERAL != *type);
		if (is_boolean_or_string_literal) {
			/* The call to "unary_operation_data_type_check()" fixed BOOLEAN_OR_STRING_LITERAL type to BOOLEAN_VALUE
			 * for the BOOLEAN_NOT case. Reinvoke "qualify_check_constraint()" to fix the unary operand type.
			 */
			assert(BOOLEAN_VALUE == *type);
			result |= qualify_check_constraint(unary->operand, table, &child_type[0], type); /* Sets child_type[0] */
			assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it is 2nd call */
			UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
		}
		break;
	case array_STATEMENT:;
		SqlArray *array;

		UNPACK_SQL_STATEMENT(array, stmt, array);
		result |= qualify_check_constraint(array->argument, table, type, fix_type);
		/* Currently only ARRAY(single_column_subquery) is supported. In this case, "array->argument" points to a
		 * table_alias_STATEMENT type structure. And so any errors will show up in the above call. No additional
		 * errors possible in the "array" structure. This might change once more features of ARRAY() are supported
		 * at which point the code here might need more error checking.
		 */
		break;
	case function_call_STATEMENT:;
		SqlFunctionCall *fc;

		UNPACK_SQL_STATEMENT(fc, stmt, function_call);
		result |= qualify_check_constraint(fc->function_name, table, type, NULL);
		if (result) {
			break;
		}
		result
		    = function_call_data_type_check(stmt, type, NULL, table); /* Note: "fc->parameters" also gets qualified here */
		if (result) {
			break;
		}
		/* Note down the function name and hash as encountered in this CHECK constraint.
		 * Needed later to store the list of functions that this CREATE TABLE command depends on.
		 * This way a DROP FUNCTION can check if there are any table CHECK constraints relying on it and if so error out.
		 */
		SqlFunction *function;
		UNPACK_SQL_STATEMENT(function, fc->function_schema, create_function);

		ydb_buffer_t function_name;
		YDB_STRING_TO_BUFFER(function->function_name->v.value->v.string_literal, &function_name);

		ydb_buffer_t ydboctoTblConstraint;
		YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOTBLCONSTRAINT, &ydboctoTblConstraint);

		ydb_buffer_t subs[3];
		char	     subs1_buff[sizeof(void *)];
		YDB_LITERAL_TO_BUFFER(OCTOLIT_FUNCTIONS, &subs[0]);
		subs[1].buf_addr = subs1_buff;
		subs[1].len_alloc = sizeof(subs1_buff);

		int status;
		status = ydb_get_s(&ydboctoTblConstraint, 1, &subs[0], &subs[1]);
		assert(YDB_OK == status);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			result = 1;
		}
		YDB_STRING_TO_BUFFER(function->function_hash->v.value->v.string_literal, &subs[2]);
		status = ydb_set_s(&ydboctoTblConstraint, 3, &subs[0], &function_name);
		assert(YDB_OK == status);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			result = 1;
		}
		break;
	case coalesce_STATEMENT:;
		SqlCoalesceCall *coalesce_call;

		UNPACK_SQL_STATEMENT(coalesce_call, stmt, coalesce);
		result |= qualify_check_constraint_column_list(coalesce_call->arguments, table, type, NULL, ensure_same_type);
		break;
	case greatest_STATEMENT:;
		SqlGreatest *greatest_call;

		UNPACK_SQL_STATEMENT(greatest_call, stmt, greatest);
		result |= qualify_check_constraint_column_list(greatest_call->arguments, table, type, NULL, ensure_same_type);
		break;
	case least_STATEMENT:;
		SqlLeast *least_call;

		UNPACK_SQL_STATEMENT(least_call, stmt, least);
		result |= qualify_check_constraint_column_list(least_call->arguments, table, type, NULL, ensure_same_type);
		break;
	case null_if_STATEMENT:;
		SqlNullIf   *null_if;
		SqlValueType tmpType;

		UNPACK_SQL_STATEMENT(null_if, stmt, null_if);
		result |= qualify_check_constraint(null_if->left, table, type, NULL);
		if (result) {
			break;
		}
		result |= qualify_check_constraint(null_if->right, table, &tmpType, NULL);
		if (result) {
			break;
		}
		if (BOOLEAN_OR_STRING_LITERAL == *type) {
			SqlValueType fix_type2;

			if (BOOLEAN_OR_STRING_LITERAL == tmpType) {
				fix_type2 = STRING_LITERAL;
				result |= qualify_check_constraint(null_if->right, table, &tmpType, &fix_type2);
				assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it is 2nd
						    call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(fix_type2 == tmpType);
			} else if (BOOLEAN_VALUE == tmpType) {
				fix_type2 = BOOLEAN_VALUE;
			} else {
				fix_type2 = STRING_LITERAL;
			}
			result |= qualify_check_constraint(null_if->left, table, type, &fix_type2);
			assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it is 2nd call */
			UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
			assert(fix_type2 == *type);
		} else if (BOOLEAN_OR_STRING_LITERAL == tmpType) {
			SqlValueType fix_type2;

			if (BOOLEAN_VALUE == *type) {
				fix_type2 = BOOLEAN_VALUE;
			} else {
				fix_type2 = STRING_LITERAL;
			}
			result |= qualify_check_constraint(null_if->right, table, &tmpType, &fix_type2);
			assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it is 2nd call */
			UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
			assert(fix_type2 == tmpType);
		}
		result |= ensure_same_type(type, &tmpType, null_if->left, null_if->right, NULL);
		break;
	case cas_STATEMENT:;
		SqlCaseStatement *cas;

		/* The below layout is very similar to that in the "cas_STATEMENT" case block of populate_data_type.c.
		 * See there for comments on why the below code is laid out this way.
		 */
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		if (NULL != cas->value) {
			result |= qualify_check_constraint(cas->value, table, type, NULL);
			if (result) {
				break;
			}
			if (BOOLEAN_OR_STRING_LITERAL == *type) {
				SqlValueType fix_type2;

				fix_type2 = STRING_LITERAL;
				result |= qualify_check_constraint(cas->value, table, type, &fix_type2);
				assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it is 2nd
						    call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(STRING_LITERAL == *type);
			}
		} else {
			*type = BOOLEAN_VALUE;
		}
		assert(NULL != cas->branches);
		result |= qualify_check_constraint(cas->branches, table, type, NULL);
		if (result) {
			break;
		}
		if (NULL != cas->optional_else) {
			result |= qualify_check_constraint(cas->optional_else, table, &child_type[0], NULL);
			if (result) {
				break;
			}
			CAST_AMBIGUOUS_TYPES(*type, child_type[0], result, ((ParseContext *)NULL));
			if (result) {
				break;
			}
			CHECK_TYPE_AND_BREAK_ON_MISMATCH(*type, child_type[0], ERR_CASE_BRANCH_TYPE_MISMATCH,
							 &cas->branches->v.cas_branch->value, &cas->optional_else, result);
			if (result) {
				break;
			}
		}
		assert(TABLE_ASTERISK != *type);
		if (BOOLEAN_OR_STRING_LITERAL == *type) {
			SqlCaseBranchStatement *cas_branch, *cur_branch;
			SqlValueType		fix_type2;

			FIX_TYPE_TO_STRING_LITERAL(*type);
			fix_type2 = STRING_LITERAL;
			UNPACK_SQL_STATEMENT(cas_branch, cas->branches, cas_branch);
			cur_branch = cas_branch;
			do {
				result |= qualify_check_constraint(cur_branch->value, table, &child_type[0], &fix_type2);
				assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it is 2nd
						    call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert((fix_type2 == *type) || IS_NUL_VALUE(*type));
				cur_branch = cur_branch->next;
			} while (cur_branch != cas_branch);
			if (NULL != cas->optional_else) {
				result |= qualify_check_constraint(cas->optional_else, table, &child_type[0], &fix_type2);
				assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it is 2nd
						    call */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert((fix_type2 == *type) || IS_NUL_VALUE(*type));
			}
		}
		break;
	case cas_branch_STATEMENT:;
		SqlCaseBranchStatement *cas_branch, *cur_branch;

		/* The below layout is very similar to that in the "cas_branch_STATEMENT" case block of populate_data_type.c.
		 * See there for comments on why the below code is laid out this way.
		 */
		assert(UNKNOWN_SqlValueType != *type); /* "*type" stores the expected type of "cur_branch->condition" */
		UNPACK_SQL_STATEMENT(cas_branch, stmt, cas_branch);
		cur_branch = cas_branch;
		result |= qualify_check_constraint(cur_branch->value, table, &child_type[0], NULL);
		if (result) {
			break;
		}
		do {
			result |= qualify_check_constraint(cur_branch->condition, table, &child_type[1], NULL);
			if (result) {
				break;
			}
			assert(UNKNOWN_SqlValueType != child_type[1]);
			if (BOOLEAN_OR_STRING_LITERAL == child_type[1]) {
				SqlValueType fix_type2;

				/* Check if expected type is BOOLEAN or STRING. If so, fix child_type[1] accordingly. */
				if (BOOLEAN_VALUE == *type) {
					fix_type2 = BOOLEAN_VALUE;
				} else if (STRING_LITERAL == *type) {
					fix_type2 = STRING_LITERAL;
				} else {
					fix_type2 = UNKNOWN_SqlValueType;
				}
				if (UNKNOWN_SqlValueType != fix_type2) {
					result
					    |= qualify_check_constraint(cur_branch->condition, table, &child_type[1], &fix_type2);
					assert(!result); /* type fixing call of "qualify_check_constraint" should never fail as it
							    is 2nd call */
					UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
					assert(fix_type2 == child_type[1]);
				}
			}
			CAST_AMBIGUOUS_TYPES(*type, child_type[1], result, ((ParseContext *)NULL));
			if (result) {
				break;
			}
			CHECK_TYPE_AND_BREAK_ON_MISMATCH(child_type[1], *type, ERR_CASE_VALUE_TYPE_MISMATCH, &cur_branch->condition,
							 NULL, result);
			assert(!result); /* because the above macro would have done a "break" otherwise */
			UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
			if (cas_branch != cur_branch->next) {
				result |= qualify_check_constraint(cur_branch->next->value, table, &child_type[1], NULL);
				if (result) {
					break;
				}
				CAST_AMBIGUOUS_TYPES(child_type[0], child_type[1], result, ((ParseContext *)NULL));
				if (result) {
					break;
				}
				CHECK_TYPE_AND_BREAK_ON_MISMATCH(child_type[0], child_type[1], ERR_CASE_BRANCH_TYPE_MISMATCH,
								 &cur_branch->value, &cur_branch->next->value, result);
				assert(!result); /* because the above macro would have done a "break" otherwise */
				UNUSED(result);	 /* to avoid [clang-analyzer-deadcode.DeadStores] warning */
				assert(child_type[0] == child_type[1]);
			}
			cur_branch = cur_branch->next;
		} while (cur_branch != cas_branch);
		if (result) {
			break;
		}
		/* Store type of the THEN argument in "*type". For caller to use to compare ELSE argument type if one exists */
		*type = child_type[0];
		break;
	case column_list_STATEMENT:
	case column_alias_STATEMENT:
	case column_list_alias_STATEMENT:
	case create_table_STATEMENT:
	case create_view_STATEMENT:
	case select_STATEMENT:
	case table_value_STATEMENT:
	case insert_STATEMENT:
	case delete_from_STATEMENT:
	case update_STATEMENT:
	case join_STATEMENT:
	case create_function_STATEMENT:
	case drop_table_STATEMENT:
	case drop_view_STATEMENT:
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
