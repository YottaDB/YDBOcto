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
#include "octo_types.h"

#define SET_RET_WITH_GROUP_BY_FIELD(SQL_ELEM, STMT, RET)        \
	{                                                       \
		UNPACK_SQL_STATEMENT(SQL_ELEM, STMT, SQL_ELEM); \
		(RET) = &(SQL_ELEM->group_by_fields);           \
	}

/* Below function is used to get `group_by_fields` values of the passed `stmt`.
 * Input parameters:
 *	* `stmt`: The value to process
 * Return value:
 *	* `group_by_fields` field in `stmt`
 *	* NULL when `group_by_fields` field is absent
 */
group_by_fields_t *get_group_by_fields(SqlStatement *stmt) {
	if (NULL == stmt) {
		return NULL;
	}
	group_by_fields_t *ret = NULL; /* to avoid false [-Wmaybe-uninitialized] warnings from compiler */
	switch (stmt->type) {
	case cas_STATEMENT:;
		SqlCaseStatement *cas;
		SET_RET_WITH_GROUP_BY_FIELD(cas, stmt, ret);
		break;
	case binary_STATEMENT:;
		SqlBinaryOperation *binary;
		SET_RET_WITH_GROUP_BY_FIELD(binary, stmt, ret);
		break;
	case unary_STATEMENT:;
		SqlUnaryOperation *unary;
		SET_RET_WITH_GROUP_BY_FIELD(unary, stmt, ret);
		break;
	case value_STATEMENT:;
		SqlValue *value;
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch (value->type) {
		case CALCULATED_VALUE:;
			SET_RET_WITH_GROUP_BY_FIELD(value, stmt, ret);
			break;
		case COERCE_TYPE:
			SET_RET_WITH_GROUP_BY_FIELD(value, stmt, ret);
			break;
		case BOOLEAN_VALUE:
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case STRING_LITERAL:
		case DATE_LITERAL:
		case TIME_LITERAL:
		case TIMESTAMP_LITERAL:
		case TIME_WITH_TIME_ZONE_LITERAL:
		case TIMESTAMP_WITH_TIME_ZONE_LITERAL:
		case BOOLEAN_OR_STRING_LITERAL:
		case NUL_VALUE:
		case PARAMETER_VALUE:
			/* Following type of queries can reach this block
			 * 	SELECT 'test' FROM names GROUP BY 1 HAVING 'test'!='hello';
			 */
			ret = NULL;
			break;
		case COLUMN_REFERENCE:
		case TABLE_ASTERISK:
		case SELECT_ASTERISK:
			/* We do not expect get_group_by_fields() call for the above types
			 * as the expectation is that they would have been converted to
			 * SqlColumnAlias during the invocation of this function.
			 */
		case FUNCTION_NAME:
		case FUNCTION_HASH:
		case DELIM_VALUE:
		case IS_NULL_LITERAL:
		case INVALID_SqlValueType:
		case UNKNOWN_SqlValueType:
			assert(FALSE);
			ret = NULL;
			break;
		}
		break;
	case column_alias_STATEMENT:
		/* Following type of queries can reach this block
		 *	select count(n1.firstname) from names n1 order by exists(select count(n1.firstname)
		 * 							from names n2 group by n1.firstname);
		 */
		ret = NULL;
		break;
	case function_call_STATEMENT:;
	case coalesce_STATEMENT:;
	case null_if_STATEMENT:;
	case greatest_STATEMENT:;
	case least_STATEMENT:;
		// The above cases are handled by CALCULATED_VALUE in value_STATEMENT
	case column_STATEMENT:
	case column_list_STATEMENT:
	case column_list_alias_STATEMENT:
	case table_alias_STATEMENT:;
	case set_operation_STATEMENT:;
	case array_STATEMENT:;
	case cas_branch_STATEMENT:
	case aggregate_function_STATEMENT:
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
	case discard_xrefs_STATEMENT:
	case row_value_STATEMENT:
	case history_STATEMENT:
	case display_relation_STATEMENT:
	case invalid_STATEMENT:
		assert(FALSE);
		ret = NULL;
		break;
	}
	return ret;
}
