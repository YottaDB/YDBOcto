/****************************************************************
 *								*
 * Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	*
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
int qualify_check_constraint_column_list(SqlStatement *v, SqlTable *table, SqlValueType *type, boolean_t do_loop,
					 DataTypeCallback callback) {
	SqlColumnList *column_list, *cur_column_list;
	SqlValueType   current_type;
	int	       result;

	result = 0;
	*type = UNKNOWN_SqlValueType;
	if (NULL != v) {
		SqlStatement *first_value;

		// SqlColumnList
		UNPACK_SQL_STATEMENT(column_list, v, column_list);
		cur_column_list = column_list;
		first_value = NULL; /* needed to appease static code checkers from false warnings */
		do {
			// SqlValue or SqlColumnAlias
			current_type = UNKNOWN_SqlValueType;
			result |= qualify_check_constraint(cur_column_list->value, table, &current_type);
			if (result) {
				break;
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
		} while (do_loop && (cur_column_list != column_list));
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
int qualify_check_constraint(SqlStatement *stmt, SqlTable *table, SqlValueType *type) {
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
			result |= qualify_check_constraint(value->v.calculated, table, type);
			break;
		case BOOLEAN_VALUE:
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case STRING_LITERAL:
		case NUL_VALUE:
			*type = value->type;
			break;
		case FUNCTION_NAME:
			/* No need to do any qualification or type checking in this case */
			break;
		case TABLE_ASTERISK:
		case PARAMETER_VALUE:
			/* These usages are not supported inside CHECK constraints. Issue syntax error. */
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
					/* Only the currently being created table name is allowed in column references inside
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
			boolean_t  column_valid;
			SqlColumn *cur_column, *start_column;

			UNPACK_SQL_STATEMENT(start_column, table->columns, column);
			cur_column = start_column;
			column_valid = FALSE;
			do {
				SqlValue *colName;

				UNPACK_SQL_STATEMENT(colName, cur_column->columnName, value);
				if (!strcmp(colName->v.string_literal, column_name)) {
					column_valid = TRUE;
					break;
				}
				cur_column = cur_column->next;
			} while (cur_column != start_column);
			if (!column_valid) {
				ERROR(ERR_UNKNOWN_COLUMN_NAME, column_name);
				yyerror(&stmt->loc, NULL, NULL, NULL, NULL, NULL);
				result = 1;
			} else {
				/* Now that column qualification is successful, do data type population for caller */
				*type = get_sqlvaluetype_from_sqldatatype(cur_column->data_type_struct.data_type, FALSE);
			}
			break;
		case COERCE_TYPE:
			result |= qualify_check_constraint(value->v.coerce_target, table, &value->pre_coerced_type);
			if (result) {
				yyerror(NULL, NULL, &stmt, NULL, NULL, NULL);
			} else {
				/* This code is similar to that in "populate_data_type.c" */
				*type = get_sqlvaluetype_from_sqldatatype(value->coerced_type.data_type, FALSE);
			}
			break;
		case FUNCTION_HASH:
		case DELIM_VALUE:
		case IS_NULL_LITERAL:
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

		/* Note: The below code is similar to that in populate_data_type.c. Any changes here might need to be done there. */
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		result |= qualify_check_constraint(binary->operands[0], table, &child_type[0]);
		if (result) {
			break;
		}
		if (((BOOLEAN_IN == binary->operation) || (BOOLEAN_NOT_IN == binary->operation))
		    && (column_list_STATEMENT == binary->operands[1]->type)) {
			// SqlColumnList
			result |= qualify_check_constraint_column_list(binary->operands[1], table, &child_type[1], TRUE,
								       ensure_same_type);
		} else {
			// SqlStatement (?)
			result |= qualify_check_constraint(binary->operands[1], table, &child_type[1]);
		}
		if (result) {
			break;
		}
		result = binary_operation_data_type_check(binary, child_type, type, NULL);
		break;
	case unary_STATEMENT:;
		SqlUnaryOperation *unary;

		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		result |= qualify_check_constraint(unary->operand, table, &child_type[0]); /* Sets child_type[0] */
		if (result) {
			break;
		}
		result = unary_operation_data_type_check(unary, child_type, type); /* Sets "*type" */
		break;
	case array_STATEMENT:;
		SqlArray *array;

		UNPACK_SQL_STATEMENT(array, stmt, array);
		result |= qualify_check_constraint(array->argument, table, type);
		/* Currently only ARRAY(single_column_subquery) is supported. In this case, "array->argument" points to a
		 * table_alias_STATEMENT type structure. And so any errors will show up in the above call. No additional
		 * errors possible in the "array" structure. This might change once more features of ARRAY() are supported
		 * at which point the code here might need more error checking.
		 */
		break;
	case function_call_STATEMENT:;
		SqlFunctionCall *fc;

		UNPACK_SQL_STATEMENT(fc, stmt, function_call);
		result |= qualify_check_constraint(fc->function_name, table, type);
		if (result) {
			break;
		}
		result = function_call_data_type_check(fc, type, NULL, table); /* Note: "fc->parameters" also gets qualified here */
		break;
	case coalesce_STATEMENT:;
		SqlCoalesceCall *coalesce_call;

		UNPACK_SQL_STATEMENT(coalesce_call, stmt, coalesce);
		result |= qualify_check_constraint_column_list(coalesce_call->arguments, table, type, TRUE, ensure_same_type);
		break;
	case greatest_STATEMENT:;
		SqlGreatest *greatest_call;

		UNPACK_SQL_STATEMENT(greatest_call, stmt, greatest);
		result |= qualify_check_constraint_column_list(greatest_call->arguments, table, type, TRUE, ensure_same_type);
		break;
	case least_STATEMENT:;
		SqlLeast *least_call;

		UNPACK_SQL_STATEMENT(least_call, stmt, least);
		result |= qualify_check_constraint_column_list(least_call->arguments, table, type, TRUE, ensure_same_type);
		break;
	case null_if_STATEMENT:;
		SqlNullIf *  null_if;
		SqlValueType tmpType;

		UNPACK_SQL_STATEMENT(null_if, stmt, null_if);
		result |= qualify_check_constraint(null_if->left, table, type);
		if (result) {
			break;
		}
		result |= qualify_check_constraint(null_if->right, table, &tmpType);
		if (result) {
			break;
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
			result |= qualify_check_constraint(cas->value, table, type);
			if (result) {
				break;
			}
		} else {
			*type = BOOLEAN_VALUE;
		}
		assert(NULL != cas->branches);
		result |= qualify_check_constraint(cas->branches, table, type);
		if (result) {
			break;
		}
		if (NULL != cas->optional_else) {
			result |= qualify_check_constraint(cas->optional_else, table, &child_type[0]);
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
		break;
	case cas_branch_STATEMENT:;
		SqlCaseBranchStatement *cas_branch, *cur_branch;

		/* The below layout is very similar to that in the "cas_branch_STATEMENT" case block of populate_data_type.c.
		 * See there for comments on why the below code is laid out this way.
		 */
		assert(UNKNOWN_SqlValueType != *type); /* "*type" stores the expected type of "cur_branch->condition" */
		UNPACK_SQL_STATEMENT(cas_branch, stmt, cas_branch);
		cur_branch = cas_branch;
		result |= qualify_check_constraint(cur_branch->value, table, &child_type[0]);
		if (result) {
			break;
		}
		do {
			result |= qualify_check_constraint(cur_branch->condition, table, &child_type[1]);
			if (result) {
				break;
			}
			assert(UNKNOWN_SqlValueType != child_type[1]);
			CAST_AMBIGUOUS_TYPES(*type, child_type[1], result, ((ParseContext *)NULL));
			if (result) {
				break;
			}
			CHECK_TYPE_AND_BREAK_ON_MISMATCH(child_type[1], *type, ERR_CASE_VALUE_TYPE_MISMATCH, &cur_branch->condition,
							 NULL, result);
			assert(!result); /* because the above macro would have done a "break" otherwise */
			if (cas_branch != cur_branch->next) {
				result |= qualify_check_constraint(cur_branch->next->value, table, &child_type[1]);
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
	case select_STATEMENT:
	case table_value_STATEMENT:
	case insert_STATEMENT:
	case delete_from_STATEMENT:
	case update_STATEMENT:
	case join_STATEMENT:
	case create_function_STATEMENT:
	case drop_table_STATEMENT:
	case drop_function_STATEMENT:
	case column_STATEMENT:
	case parameter_type_list_STATEMENT:
	case constraint_STATEMENT:
	case keyword_STATEMENT:
	case begin_STATEMENT:
	case commit_STATEMENT:
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
