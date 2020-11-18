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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

/* Checks 2 column lists for type match and does a few other things depending on input type.
 * Issues error as appropriate if column list's type or number of columns does not match.
 * Currently accepts input "stmt" of type "set_operation_STATEMENT" or "insert_STATEMENT".
 * In case of "set_operation_STATEMENT", compares the column lists of the two operands of the SET.
 * In case of "insert_STATEMENT", compares the column lists of the source query and the destination table.
 * Returns
 *	0 if success.
 *	1 if failure.
 */
int check_column_lists_for_type_match(SqlStatement *stmt) {
	SqlTableAlias *	    table_alias[2];
	SqlColumnListAlias *cur_cla[2], *start_cla[2];
	SqlStatement *	    sql_stmt;
	boolean_t	    terminate_loop[2] = {FALSE, FALSE};
	SqlColumnListAlias *type_mismatch_cla[2] = {NULL, NULL};
	SqlColumnListAlias *cur_set_cla, *start_set_cla;
	SqlSetOperation *   set_operand;
	int		    i;
	SqlInsertStatement *insert;
	SqlSetOperation *   set_operation;
	boolean_t	    is_set_operation, cla1_is_of_type_cl;
	int		    result;

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
				start_cla[i] = set_operand->col_type_list;
			}
		}
		start_set_cla = NULL;
		cla1_is_of_type_cl = FALSE;
	} else {
		SqlTableAlias *src_table_alias;
		SqlStatement * src_table_alias_stmt, *src_table_alias_stmt2;

		is_set_operation = FALSE;
		set_operation = NULL; /* Not needed but there to prevent [-Wmaybe-uninitialized] warning from C compiler */
		assert(insert_STATEMENT == stmt->type);
		UNPACK_SQL_STATEMENT(insert, stmt, insert);
		src_table_alias_stmt = insert->src_table_alias_stmt;
		src_table_alias_stmt2 = drill_to_table_alias(src_table_alias_stmt);
		UNPACK_SQL_STATEMENT(src_table_alias, src_table_alias_stmt2, table_alias);
		UNPACK_SQL_STATEMENT(start_cla[0], src_table_alias->column_list, column_list_alias);
		if (NULL == insert->columns) {
			UNPACK_SQL_STATEMENT(start_cla[1], insert->dst_table_alias->column_list, column_list_alias);
			cla1_is_of_type_cl = FALSE;
		} else {
			SqlStatement * cl1_stmt;
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
		cur_cla[i] = start_cla[i];
	}
	do {
		SqlValueType left_type, right_type;
		boolean_t    is_type_mismatch;

		left_type = cur_cla[0]->type;
		if (!cla1_is_of_type_cl) {
			right_type = cur_cla[1]->type;
		} else {
			SqlColumnList *cl1;
			SqlColumn *    col1;

			/* Note: "cl1->value->type" was changed from "value_STATEMENT" to "column_STATEMENT"
			 * in "insert_statement.c". Hence it is safe to do the below.
			 */
			cl1 = (SqlColumnList *)cur_cla[1];
			UNPACK_SQL_STATEMENT(col1, cl1->value, column);
			right_type = get_sqlvaluetype_from_sqldatatype(col1->data_type_struct.data_type, FALSE);
		}
		/* Assert all possible valid types. This is used to simplify the `if` checks below
		 * that determine the value of `is_type_mismatch`.
		 */
		assert(IS_LITERAL_PARAMETER(left_type) || (NUL_VALUE == left_type));
		assert(IS_LITERAL_PARAMETER(right_type) || (NUL_VALUE == right_type));
		/* If not yet found any type mismatch, check for one. If already found one, keep just that.
		 * In general, all types are compatible with only themselves.
		 * Exception is that
		 *	a) NUMERIC and INTEGER are compatible with each other and no other type.
		 *	b) NULL is compatible with any type.
		 * This code is similar to that in "CAST_AMBIGUOUS_TYPES" macro (in "src/populate_data_type.c").
		 */
		if (NULL == type_mismatch_cla[0]) {
			switch (left_type) {
			case BOOLEAN_VALUE:
				is_type_mismatch = ((NUL_VALUE != right_type) && (BOOLEAN_VALUE != right_type));
				break;
			case INTEGER_LITERAL:
			case NUMERIC_LITERAL:
				is_type_mismatch = ((NUL_VALUE != right_type) && (INTEGER_LITERAL != right_type)
						    && (NUMERIC_LITERAL != right_type));
				break;
			case STRING_LITERAL:
				is_type_mismatch = ((NUL_VALUE != right_type) && (STRING_LITERAL != right_type));
				break;
			case NUL_VALUE:
				is_type_mismatch = FALSE;
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
			if (NUL_VALUE != left_type) {
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
			SqlColumnList * column_list;
			SqlColumnAlias *column_alias;
			SqlColumn *	column;
			SqlValue *	value;
			SqlValueType	cla1_type;
			SqlColumnList * cl1;

			if (!cla1_is_of_type_cl) {
				UNPACK_SQL_STATEMENT(column_list, type_mismatch_cla[1]->column_list, column_list);
				UNPACK_SQL_STATEMENT(column_alias, column_list->value, column_alias);
				UNPACK_SQL_STATEMENT(column, column_alias->column, column);
				cla1_type = type_mismatch_cla[1]->type;
			} else {

				cl1 = (SqlColumnList *)cur_cla[1];
				UNPACK_SQL_STATEMENT(column, cl1->value, column);
				cla1_type = get_sqlvaluetype_from_sqldatatype(column->data_type_struct.data_type, FALSE);
			}
			UNPACK_SQL_STATEMENT(value, column->columnName, value);
			ERROR(ERR_INSERT_TYPE_MISMATCH, value->v.string_literal, get_user_visible_type_string(cla1_type),
			      get_user_visible_type_string(type_mismatch_cla[0]->type));
			if (cla1_is_of_type_cl) {
				yyerror(NULL, NULL, &cl1->value, NULL, NULL, NULL);
			}
			location = type_mismatch_cla[0]->column_list->loc;
			yyerror(&location, NULL, NULL, NULL, NULL, NULL);
		}
		result = 1;
	}
	if (is_set_operation) {
		assert(NULL != start_set_cla);
		set_operation->col_type_list = start_set_cla;
	}
	return result;
}
