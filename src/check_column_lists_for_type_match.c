/****************************************************************
 *								*
 * Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	*
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
#include "octo_types.h"
#include "octo_type_check.h"

#define HAS_DATE(VAL) (IS_TIMESTAMP(VAL) || (DATE_LITERAL == (VAL)))

int fix_value_to_boolean_value(SqlStatement *val, ParseContext *parse_context) {
	boolean_t is_invalid = FALSE;
	SqlValue *value;
	UNPACK_SQL_STATEMENT(value, val, value);
	/* The list of literals that are accepted as BOOLEAN literals is also maintained at
	 * initBoolMap label in "src/aux/_ydboctoplanhelpers.m" and "literal_value" rule in src/parser.y.
	 * Ensure any change here is reflected in other places too.
	 */
	size_t len;
	char  *ptr;
	ptr = value->v.string_literal;
	len = strlen(ptr);
	switch (len) {
	case 1:
		switch (*ptr) {
		case '0':
		case 'n':
		case 'N':
		case 'f':
		case 'F':
			value->u.bool_or_str.truth_value = FALSE;
			value->type = BOOLEAN_VALUE;
			break;
		case '1':
		case 'y':
		case 'Y':
		case 't':
		case 'T':
			value->u.bool_or_str.truth_value = TRUE;
			value->type = BOOLEAN_VALUE;
			break;
		default:
			is_invalid = TRUE;
		}
		break;
	case 2:
		if (0 == strcasecmp(ptr, "no")) {
			value->u.bool_or_str.truth_value = FALSE;
			value->type = BOOLEAN_VALUE;
		} else {
			is_invalid = TRUE;
		}
		break;
	case 3:
		if (0 == strcasecmp(ptr, "yes")) {
			value->u.bool_or_str.truth_value = TRUE;
			value->type = BOOLEAN_VALUE;
		} else {
			is_invalid = TRUE;
		}
		break;
	case 4:
		if (0 == strcasecmp(ptr, "true")) {
			value->u.bool_or_str.truth_value = TRUE;
			value->type = BOOLEAN_VALUE;
		} else {
			is_invalid = TRUE;
		}
		break;
	case 5:
		if (0 == strcasecmp(ptr, "false")) {
			value->u.bool_or_str.truth_value = FALSE;
			value->type = BOOLEAN_VALUE;
		} else {
			is_invalid = TRUE;
		}
		break;
	default:
		/* This is invalid */
		is_invalid = TRUE;
		break;
	}
	if (is_invalid) {
		ERROR(ERR_INVALID_BOOLEAN_SYNTAX, get_user_visible_type_string(BOOLEAN_VALUE));
		yyerror(NULL, NULL, &val, NULL, NULL, NULL);
		return 1;
	} else {
		value->v.string_literal = (value->u.bool_or_str.truth_value ? "1" : "0");
		int status;
		status = parse_literal_to_parameter(parse_context, value, TRUE);
		if (0 != status) {
			return 1;
		}
	}
	return 0;
}

int fix_value_stmt_to_boolean_in_table_values(SqlStatement *val, int col, boolean_t *is_type_mismatch,
					      ParseContext *parse_context) {
	assert(table_value_STATEMENT == val->type);
	SqlTableValue *tv;
	UNPACK_SQL_STATEMENT(tv, val, table_value);

	int	     colno;
	SqlRowValue *row_value, *start_row_value;
	UNPACK_SQL_STATEMENT(row_value, tv->row_value_stmt, row_value);
	start_row_value = row_value;

	do {
		SqlColumnList *start_column_list, *cur_column_list;
		UNPACK_SQL_STATEMENT(start_column_list, row_value->value_list, column_list);
		cur_column_list = start_column_list;
		for (colno = 1; colno != col; colno++) {
			cur_column_list = cur_column_list->next;
		}
		if (value_STATEMENT == cur_column_list->value->type) {
			int status = fix_value_to_boolean_value(cur_column_list->value, parse_context);
			if (status) {
				// Error is issued by fix_value_to_boolean_value(), just return here
				return 1;
			}
		} else {
			// We cannot validate the row values, return and let the caller handle mismatch
			*is_type_mismatch = TRUE;
			return 1;
		}
		row_value = row_value->next;
	} while (row_value != start_row_value);
	// if above loop did not return with an error then we need to set the column type to boolean
	SqlColumn *start_column = tv->column_stmt->v.column;
	SqlColumn *column = start_column;
	for (colno = 1; colno != col; colno++) {
		column = column->next;
	}
	assert(STRING_TYPE == column->data_type_struct.data_type);
	column->data_type_struct.data_type = BOOLEAN_TYPE;
	return 0;
}

/* Checks 2 column lists for type match and does a few other things depending on input type.
 * Issues error as appropriate if column list's type or number of columns does not match.
 * Currently accepts input "stmt" of type "set_operation_STATEMENT" or "insert_STATEMENT".
 * In case of "set_operation_STATEMENT", compares the column lists of the two operands of the SET.
 * In case of "insert_STATEMENT", compares the column lists of the source query and the destination table.
 * Returns
 *	0 if success.
 *	1 if failure.
 */
int check_column_lists_for_type_match(SqlStatement *stmt, ParseContext *parse_context) {
	SqlTableAlias	   *table_alias[2];
	SqlColumnListAlias *cur_cla[2], *start_cla[2];
	SqlStatement	   *sql_stmt;
	boolean_t	    terminate_loop[2] = {FALSE, FALSE};
	SqlColumnListAlias *type_mismatch_cla[2] = {NULL, NULL};
	SqlColumnListAlias *cur_set_cla, *start_set_cla;
	SqlSetOperation	   *set_operand;
	int		    i;
	SqlInsertStatement *insert;
	SqlSetOperation	   *set_operation;
	boolean_t	    is_set_operation, cla1_is_of_type_cl, fixed_type;
	int		    result;
	SqlStatementType    insert_src_type;
	SqlTableAlias	   *src_table_alias;

	if (set_operation_STATEMENT == stmt->type) {
		is_set_operation = TRUE;
		UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
		for (i = 0; i < 2; i++) {
			sql_stmt = set_operation->operand[i];
			if (table_alias_STATEMENT == sql_stmt->type) {
				UNPACK_SQL_STATEMENT(table_alias[i], sql_stmt, table_alias);
				assert(NULL != table_alias[i]->column_list);
				UNPACK_SQL_STATEMENT(start_cla[i], table_alias[i]->column_list, column_list_alias);
			} else {
				assert(set_operation_STATEMENT == sql_stmt->type);
				UNPACK_SQL_STATEMENT(set_operand, sql_stmt, set_operation);
				start_cla[i] = (NULL != set_operand->col_type_list_stmt)
						   ? set_operand->col_type_list_stmt->v.column_list_alias
						   : NULL;
			}
		}
		start_set_cla = NULL;
		cla1_is_of_type_cl = FALSE;
		insert_src_type = invalid_STATEMENT; // Avoids [-Wmaybe-uninitialized]
		src_table_alias = NULL;		     // Avoids [-Wmaybe-uninitialized]
	} else {
		SqlStatement *src_table_alias_stmt, *src_table_alias_stmt2;

		is_set_operation = FALSE;
		set_operation = NULL; /* Not needed but there to prevent [-Wmaybe-uninitialized] warning from C compiler */
		assert(insert_STATEMENT == stmt->type);
		UNPACK_SQL_STATEMENT(insert, stmt, insert);
		src_table_alias_stmt = insert->src_table_alias_stmt;
		insert_src_type = src_table_alias_stmt->type;
		switch (insert_src_type) {
		case table_alias_STATEMENT:
			insert_src_type = src_table_alias_stmt->v.table_alias->table->type;
			break;
		case set_operation_STATEMENT:
			break;
		default:
			assert(FALSE);
			break;
		}
		src_table_alias_stmt2 = drill_to_table_alias(src_table_alias_stmt);
		UNPACK_SQL_STATEMENT(src_table_alias, src_table_alias_stmt2, table_alias);
		UNPACK_SQL_STATEMENT(start_cla[0], src_table_alias->column_list, column_list_alias);
		if (NULL == insert->columns) {
			SqlTableAlias *dst_table_alias;

			UNPACK_SQL_STATEMENT(dst_table_alias, insert->dst_table_alias_stmt, table_alias);
			UNPACK_SQL_STATEMENT(start_cla[1], dst_table_alias->column_list, column_list_alias);
			cla1_is_of_type_cl = FALSE;
		} else {
			SqlStatement  *cl1_stmt;
			SqlColumnList *cl1;

			/* We only have a "SqlColumnList *" pointer in "insert->columns" but the variable
			 * "start_cla[1]" is of type "SqlColumnListAlias *". Therefore assign this with appropriate
			 * type cast. Whenever we are about to access it, we will dereference with the right type below.
			 */
			cl1_stmt = insert->columns;
			UNPACK_SQL_STATEMENT(cl1, cl1_stmt, column_list);
			start_cla[1] = (SqlColumnListAlias *)cl1;
			cla1_is_of_type_cl = TRUE;
		}
	}
	for (i = 0; i < 2; i++) {
		assert(NULL != start_cla[i]);
		/* Following if block avoids [clang-analyzer-core.NullDereference] warning from compiler when accessing cur_cla[0]
		 * and cur_cla[1] in the next while loop.
		 */
		if (NULL == start_cla[i]) {
			return 1;
		}
		cur_cla[i] = start_cla[i];
	}
	fixed_type = FALSE;
	do {
		SqlValueType left_type, right_type;
		boolean_t    is_type_mismatch;

		left_type = cur_cla[0]->type;
		if (!cla1_is_of_type_cl) {
			right_type = cur_cla[1]->type;
		} else {
			SqlColumnList *cl1;
			SqlColumn     *col1;

			/* Note: "cl1->value->type" was changed from "value_STATEMENT" to "column_STATEMENT"
			 * in "insert_statement.c". Hence it is safe to do the below.
			 */
			cl1 = (SqlColumnList *)cur_cla[1];
			UNPACK_SQL_STATEMENT(col1, cl1->value, column);
			right_type = get_sqlvaluetype_from_sqldatatype(col1->data_type_struct.data_type, FALSE);
		}
		/* Assert all possible valid types for "right_type". This is used to simplify the `if` checks below
		 * that determine the value of `is_type_mismatch`. Similar assert for "left_type" is done in the
		 * "default:" switch/case branch below.
		 */
		assert((INTEGER_LITERAL == right_type) || (NUMERIC_LITERAL == right_type) || (STRING_LITERAL == right_type)
		       || (BOOLEAN_VALUE == right_type) || (BOOLEAN_OR_STRING_LITERAL == right_type) || IS_NUL_VALUE(right_type)
		       || (DATE_LITERAL == right_type) || (TIME_LITERAL == right_type)
		       || (TIME_WITH_TIME_ZONE_LITERAL == right_type) || (TIMESTAMP_LITERAL == right_type)
		       || (TIMESTAMP_WITH_TIME_ZONE_LITERAL == right_type));
		/* If not yet found any type mismatch, check for one. If already found one, keep just that.
		 * In general, all types are compatible with only themselves.
		 * Exception is that
		 *	a) NUMERIC and INTEGER are compatible with each other and no other type.
		 *	b) NULL is compatible with any type.
		 *	c) BOOLEAN_OR_STRING_LITERAL is compatible with BOOLEAN_VALUE and STRING_LITERAL.
		 * 	d) STRING_LITERAL is compatible with date/time types if the value is valid so allow it here and validate it
		 *	   in M code.
		 * 	e) Different date/time types are compatible based on whether its a set operation or an insert operation.
		 * 	   Refer to the individual cases below for more info on which ones are valid.
		 * This code is similar to that in "CAST_AMBIGUOUS_TYPES" macro (in "src/populate_data_type.c").
		 */
		if (NULL == type_mismatch_cla[0]) {
			switch (left_type) {
			case BOOLEAN_VALUE:
				is_type_mismatch = !IS_BOOLEAN_TYPE(right_type);
				if (BOOLEAN_OR_STRING_LITERAL == right_type) {
					assert(is_set_operation);
					FIX_TYPE_TO_BOOLEAN_VALUE(cur_cla[1]->type);
					fixed_type = TRUE;
				}
				break;
			case INTEGER_LITERAL:
			case NUMERIC_LITERAL:
				is_type_mismatch = (!IS_NUL_VALUE(right_type) && (INTEGER_LITERAL != right_type)
						    && (NUMERIC_LITERAL != right_type));
				if (BOOLEAN_OR_STRING_LITERAL == right_type) {
					assert(is_set_operation);
					FIX_TYPE_TO_BOOLEAN_VALUE(cur_cla[1]->type);
					fixed_type = TRUE;
				}
				break;
			case STRING_LITERAL:
				is_type_mismatch = !IS_STRING_TYPE(right_type);
				if (BOOLEAN_OR_STRING_LITERAL == right_type) {
					assert(is_set_operation);
					FIX_TYPE_TO_STRING_LITERAL(cur_cla[1]->type);
					fixed_type = TRUE;
				} else if (IS_DATE_TIME_TYPE(right_type)) {
					is_type_mismatch = FALSE;
					if (is_set_operation) {
						// Add a cast operation at cur_cla[0]->column_list->v.column_list->value
						// ensure_same_type.c has similar checks any change here should also reflect there
						cur_cla[0]->type = right_type;
						SqlColumnList *column_list;
						UNPACK_SQL_STATEMENT(column_list, cur_cla[0]->column_list, column_list);
						ADD_DATE_TIME_TIMESTAMP_CAST_STMT(column_list->value, right_type, left_type);
					} /* else this is an insert operation allow it here to prevent the use of date/time prefix
					   * to literals. The value itself is validated in M code later on.
					   */
				} else if (BOOLEAN_VALUE == right_type) {
					if (is_set_operation) {
						/* Following query relies on this check to throw error for 'a'.
						 * 'a' is of type STRING_LITERAL but the right operand is of type BOOLEAN_VALUE.
						 * Which is an error case.
						 * 	select 'a','t' union select false,'f';
						 * If 't' was the left operand column value as shown below, the value will be set
						 * to type BOOLEAN_OR_STRING and we will not reach this code.
						 * So we consider it a mismatch here.
						 *	select 't','t' union select false,'f';
						 */
						is_type_mismatch = TRUE;
					} else {
						if (set_operation_STATEMENT == insert_src_type) {
							is_type_mismatch = TRUE;
						} else if (table_value_STATEMENT == insert_src_type) {
							// If the src_tbl_alias is a values drill down and update
							// string to boolean literals
							/* Following insert relies on allowing this mismatch to let
							 * tmpl_insert_into.ctemplate handle validating input.
							 * 	create table test (id int, foo bool);
							 * 	insert test values(1,'t');
							 * Here STRING_LITERAL is the type which 't' is qualified to be.
							 */
							is_type_mismatch = FALSE;
							int status = fix_value_stmt_to_boolean_in_table_values(
							    src_table_alias->table,
							    cur_cla[0]
								->column_list->v.column_list->value->v.column_alias->column->v
								.column->column_number,
							    &is_type_mismatch, parse_context);
							if (!status) {
								// Fixing the string to boolean was a success, update type
								cur_cla[0]->type = BOOLEAN_VALUE;
							} else {
								assert(status);
								if (is_type_mismatch) {
									// If it was not possible to change string to boolean
									// because of values clause not having value_STATEMENT, then
									// report a mismatch
									break;
								} else {
									// If boolean value syntax was not valid error is already
									// issued so just return
									return 1;
								}
							}
						} else {
							// if the src_tbl_alias is a select then we reach this code if the literal
							// is not a boolean literal
							is_type_mismatch = TRUE;
							ERROR(ERR_INVALID_BOOLEAN_SYNTAX,
							      get_user_visible_type_string(BOOLEAN_VALUE));
							yyerror(NULL, NULL, &cur_cla[0]->column_list, NULL, NULL, NULL);
							return 1;
						}
					}
				}
				break;
			case BOOLEAN_OR_STRING_LITERAL:
				switch (right_type) {
				case BOOLEAN_VALUE:
				case STRING_LITERAL:
					is_type_mismatch = FALSE;
					if (BOOLEAN_VALUE == right_type) {
						if (is_set_operation) {
							FIX_TYPE_TO_BOOLEAN_VALUE(cur_cla[0]->type);
						} else {
							assert((NULL != cur_cla[0]->column_list)
							       && (column_list_STATEMENT == cur_cla[0]->column_list->type));
							assert(value_STATEMENT
							       == cur_cla[0]->column_list->v.column_list->value->type);
							if (set_operation_STATEMENT == insert_src_type) {
								is_type_mismatch = TRUE;
							} else if (select_STATEMENT == insert_src_type) {
								// Convert it here since we are sure this is a value_STATEMENT
								FIX_TYPE_TO_BOOLEAN_VALUE(cur_cla[0]->type);
								int status = fix_value_to_boolean_value(
								    cur_cla[0]->column_list->v.column_list->value, parse_context);
								if (status) {
									return 1;
								}
							} else {
								assert(FALSE);
							}
						}
					} else {
						FIX_TYPE_TO_STRING_LITERAL(cur_cla[0]->type);
					}
					fixed_type = TRUE;
					break;
				case BOOLEAN_OR_STRING_LITERAL:
					assert(is_set_operation);
					/* Note: Below comment is needed to avoid gcc [-Wimplicit-fallthrough=] warning */
					/* fall through */
				case NUL_VALUE:
					is_type_mismatch = FALSE;
					break;
				default:
					is_type_mismatch = TRUE;
					break;
				}
				break;
			case NUL_VALUE:
				is_type_mismatch = FALSE;
				break;
			case DATE_LITERAL:
				// ensure_same_type.c and tmpl_insert_into has similar checks any change here should also reflect
				// there
				is_type_mismatch = HAS_DATE(right_type) ? FALSE : TRUE;
				break;
			case TIME_LITERAL:
				// ensure_same_type.c and tmpl_insert_into has similar checks any change here should also reflect
				// there
				is_type_mismatch
				    = ((TIME_LITERAL == right_type) || (TIME_WITH_TIME_ZONE_LITERAL == right_type)) ? FALSE : TRUE;
				break;
			case TIME_WITH_TIME_ZONE_LITERAL:
				// ensure_same_type.c and tmpl_insert_into has similar checks any change here should also reflect
				// there
				is_type_mismatch
				    = ((TIME_LITERAL == right_type) || (TIME_WITH_TIME_ZONE_LITERAL == right_type)) ? FALSE : TRUE;
				break;
			case TIMESTAMP_LITERAL:
				if (is_set_operation) {
					// ensure_same_type.c has similar checks any change here should also reflect there
					is_type_mismatch = HAS_DATE(right_type) ? FALSE : TRUE;
				} else {
					// tmpl_insert_into.ctemplate has similar checks any change here should also reflect there
					is_type_mismatch = (HAS_DATE(right_type) || (TIME_LITERAL == right_type)) ? FALSE : TRUE;
				}
				break;
			case TIMESTAMP_WITH_TIME_ZONE_LITERAL:
				if (is_set_operation) {
					// ensure_same_type.c has similar checks any change here should also reflect there
					is_type_mismatch = HAS_DATE(right_type) ? FALSE : TRUE;
				} else {
					// tmpl_insert_into.ctemplate has similar checks any change here should also reflect there
					is_type_mismatch = (HAS_DATE(right_type) || (TIME_LITERAL == right_type)) ? FALSE : TRUE;
				}
				break;
			default:
				assert(FALSE);
				FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
				/* This line exists to prevent a -Wmaybe-uninitialized compiler warning and is
				 * unreachable after the preceding assert (Debug builds) or FATAL (Release builds)
				 */
				is_type_mismatch = TRUE;
				break;
			}
			if (is_type_mismatch) {
				/* Record the first type mismatch location */
				type_mismatch_cla[0] = cur_cla[0];
				type_mismatch_cla[1] = cur_cla[1];
			}
		}
		if (is_set_operation) {
			/* Construct `column_list` for `set_operation` (needed by caller `populate_data_type`) */
			OCTO_CMALLOC_STRUCT(cur_set_cla, SqlColumnListAlias);
			if (!IS_NUL_VALUE(left_type)) {
				/* Left side column is not NULL, inherit that type for outer SET operation */
				*cur_set_cla = *cur_cla[0];
			} else {
				/* Left side column is NULL, inherit right side column type
				 * for outer SET operation.
				 */
				*cur_set_cla = *cur_cla[1];
			}
			dqinit(cur_set_cla);
			if (NULL != start_set_cla) {
				dqappend(start_set_cla, cur_set_cla);
			} else {
				start_set_cla = cur_set_cla;
			}
		}
		for (i = 0; i < 2; i++) {
			if ((0 == i) || !cla1_is_of_type_cl) {
				cur_cla[i] = cur_cla[i]->next;
			} else {
				SqlColumnList *cl1;

				cl1 = (SqlColumnList *)cur_cla[i];
				cur_cla[i] = (SqlColumnListAlias *)(cl1->next);
			}
			if (cur_cla[i] == start_cla[i])
				terminate_loop[i] = TRUE;
		}
	} while (!terminate_loop[0] && !terminate_loop[1]);
	result = 0;
	if (terminate_loop[0] != terminate_loop[1]) {
		YYLTYPE location;

		if (is_set_operation) {
			// The # of columns in the two operands (of the SET operation) do not match. Issue error.
			ERROR(ERR_SETOPER_NUMCOLS_MISMATCH, get_set_operation_string(set_operation->type));
			location = ((!terminate_loop[0]) ? cur_cla[0]->column_list->loc : cur_cla[1]->column_list->loc);
			yyerror(&location, NULL, NULL, NULL, NULL, NULL);
			result = 1;
		} else if (!cla1_is_of_type_cl) {
			/* Check if the number of source columns is LESS THAN OR EQUAL TO the number of target table columns.
			 * If so, it is okay. Otherwise issue an error.
			 */
			if (terminate_loop[1]) {
				ERROR(ERR_INSERT_TOO_MANY_EXPRESSIONS, NULL);
				yyerror(NULL, NULL, &cur_cla[0]->column_list, NULL, NULL, NULL);
				result = 1;
			}
		}
		/* else: In the "(NULL != insert_columns)" case, any appropriate error ("ERR_INSERT_TOO_MANY_COLUMNS" or
		 *	"ERR_INSERT_TOO_MANY_EXPRESSIONS" would have been already issued in "src_parser/insert_statement.c"
		 *	so no need to do any checks for that here.
		 */
	}
	if (!result && (NULL != type_mismatch_cla[0])) {
		YYLTYPE location;

		if (is_set_operation) {
			// The type of one column in the two operands (of the SET operation) do not match. Issue error.
			ERROR(ERR_SETOPER_TYPE_MISMATCH, get_set_operation_string(set_operation->type),
			      get_user_visible_type_string(type_mismatch_cla[0]->type),
			      get_user_visible_type_string(type_mismatch_cla[1]->type));
			location = type_mismatch_cla[0]->column_list->loc;
			yyerror(&location, NULL, NULL, NULL, NULL, NULL);
			location = type_mismatch_cla[1]->column_list->loc;
			yyerror(&location, NULL, NULL, NULL, NULL, NULL);
		} else {
			SqlColumnList  *column_list;
			SqlColumnAlias *column_alias;
			SqlColumn      *column;
			SqlValue       *value;
			SqlValueType	cla1_type;
			SqlColumnList  *cl1;

			if (!cla1_is_of_type_cl) {
				UNPACK_SQL_STATEMENT(column_list, type_mismatch_cla[1]->column_list, column_list);
				UNPACK_SQL_STATEMENT(column_alias, column_list->value, column_alias);
				UNPACK_SQL_STATEMENT(column, column_alias->column, column);
				cla1_type = type_mismatch_cla[1]->type;
				cl1 = NULL; // Avoid [-Wmaybe-uninitialized] warning
			} else {

				cl1 = (SqlColumnList *)cur_cla[1];
				UNPACK_SQL_STATEMENT(column, cl1->value, column);
				cla1_type = get_sqlvaluetype_from_sqldatatype(column->data_type_struct.data_type, FALSE);
			}
			UNPACK_SQL_STATEMENT(value, column->columnName, value);
			ERROR(ERR_INSERT_TYPE_MISMATCH, value->v.string_literal, get_user_visible_type_string(cla1_type),
			      get_user_visible_type_string(type_mismatch_cla[0]->type));
			if (cla1_is_of_type_cl) {
				assert(NULL != cl1);
				yyerror(NULL, NULL, &cl1->value, NULL, NULL, NULL);
			}
			location = type_mismatch_cla[0]->column_list->loc;
			yyerror(&location, NULL, NULL, NULL, NULL, NULL);
		}
		result = 1;
	}
	if (is_set_operation) {
		assert(NULL != start_set_cla);
		// assert that col_type_list_stmt is always NULL and use PACK_SQL_STATEMENT() instead of manual assignment below
		if (NULL == set_operation->col_type_list_stmt) {
			SQL_STATEMENT(set_operation->col_type_list_stmt, column_list_alias_STATEMENT);
		}
		set_operation->col_type_list_stmt->v.column_list_alias = start_set_cla;
	}
	if (!result && fixed_type) {
		/* Type checking of column lists ran without errors and there was at least one BOOLEAN_OR_STRING_LITERAL
		 * type in a cla that was fixed to a BOOLEAN_VALUE or a STRING_LITERAL. Retraverse the original structure
		 * so we fix the types of the underlying column lists from the cla which is now accurate.
		 */
		if (is_set_operation) {
			SqlColumnListAlias *fix_type_cla;

			fix_type_cla = (NULL != set_operation->col_type_list_stmt)
					   ? set_operation->col_type_list_stmt->v.column_list_alias
					   : NULL;
			result = populate_data_type_cla_fix(stmt, parse_context, fix_type_cla);
			assert(!result); /* type fixing call of "populate_data_type" should never fail as it is 2nd call */
		}
	}
	return result;
}
