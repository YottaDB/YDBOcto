/****************************************************************
 *								*
 * Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	*
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

/* The following function compares the tables passed to it based on column count and column type.
 * This is a helper function for validate_table_asterisk_comparison() to perform the actual validation.
 * Parameters:
 * - `first_column_alias` is the column_alias value of the first operand in the expression
 * - `second_column_alias` is the column_alias value of the second operand in the expression
 * NOTE: If `set_oper_stmt` of the column_alias is not NULL then `col_type_list_stmt` value of the set_operation is used to perform
 * type comparison. This value is only needed for type comparison. The column count in all cases is done by checking the table_alias
 * of the operands as column count in case of set_operation or select or VALUES can be determined by the table_alias itself.
 * Return:
 * - Issues error if any of the comparisons fail and returns 1.
 * - returns 0 on success.
 */
int compare_column_count_and_column_type_of_tables(SqlColumnAlias *first_column_alias, SqlColumnAlias *second_column_alias,
						   ParseContext *parse_context) {
	SqlTableAlias *first_table_alias, *second_table_alias;
	UNPACK_SQL_STATEMENT(first_table_alias, first_column_alias->table_alias_stmt, table_alias);
	UNPACK_SQL_STATEMENT(second_table_alias, second_column_alias->table_alias_stmt, table_alias);

	// Ensure number of columns are the same
	int first_columns_count, second_columns_count;
	first_columns_count = get_num_cols_in_table_alias(first_table_alias);
	second_columns_count = get_num_cols_in_table_alias(second_table_alias);
	if (first_columns_count != second_columns_count) {
		ERROR(ERR_TABLE_ASTERISK_COLUMN_COUNT_MISMATCH, first_columns_count, second_columns_count);
		return 1;
	}

	// Ensure column type match between the tables
	SqlColumnListAlias *first_cla, *cur_first_cla;
	SqlColumnListAlias *second_cla, *cur_second_cla;
	if (NULL != first_column_alias->set_oper_stmt) {
		SqlSetOperation *set_oper;
		UNPACK_SQL_STATEMENT(set_oper, first_column_alias->set_oper_stmt, set_operation);
		first_cla = (NULL != set_oper->col_type_list_stmt) ? set_oper->col_type_list_stmt->v.column_list_alias : NULL;
	} else {
		UNPACK_SQL_STATEMENT(first_cla, first_table_alias->column_list, column_list_alias);
	}
	if (NULL != second_column_alias->set_oper_stmt) {
		SqlSetOperation *set_oper;
		UNPACK_SQL_STATEMENT(set_oper, second_column_alias->set_oper_stmt, set_operation);
		second_cla = (NULL != set_oper->col_type_list_stmt) ? set_oper->col_type_list_stmt->v.column_list_alias : NULL;
	} else {
		UNPACK_SQL_STATEMENT(second_cla, second_table_alias->column_list, column_list_alias);
	}
	cur_first_cla = first_cla;
	cur_second_cla = second_cla;
	// Following if block avoids [clang-analyzer-core.NullDereference] warning
	// We do not expect these values to be NULL
	assert(NULL != first_cla);
	assert(NULL != second_cla);
	if ((NULL == first_cla) || (NULL == second_cla)) {
		return 0;
	}
	do {
		SqlValueType left_type = cur_first_cla->type;
		SqlValueType right_type = cur_second_cla->type;
		int	     result = 0;
		CAST_AMBIGUOUS_TYPES(left_type, right_type, result, parse_context);
		if (result) {
			return result;
		}
		/* Check if this is a comparison between NUMERIC and INTEGER explicitely because CAST_AMBIGUOUS_TYPES() treats them
		 * as same.
		 */
		if ((((NUMERIC_LITERAL == cur_first_cla->type) && (INTEGER_LITERAL == cur_second_cla->type))
		     || ((INTEGER_LITERAL == cur_first_cla->type) && (NUMERIC_LITERAL == cur_second_cla->type)))
		    || (left_type != right_type)) {
			// Issue error as types don't match
			ERROR(ERR_TABLE_ASTERISK_COLUMN_TYPE_MISMATCH, get_user_visible_type_string(cur_first_cla->type),
			      get_user_visible_type_string(cur_second_cla->type));
			return 1;
		}
		cur_first_cla = cur_first_cla->next;
		cur_second_cla = cur_second_cla->next;
	} while (cur_first_cla != first_cla);
	return 0;
}
