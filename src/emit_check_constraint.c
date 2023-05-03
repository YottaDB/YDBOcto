/****************************************************************
 *								*
 * Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	*
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

#include "octo.h"
#include "octo_types.h"
#include "octo_type_check.h"

/* Input/Output parameters
 * -----------------------
 * "buffer" points to the start of the allocated buffer.
 * "buffer_size" points to the allocation size.
 * "buff_ptr" points to the current location in the allocated buffer where the next byte can be emitted.
 *
 * All 3 parameters can be modified in case a buffer expansion occurs internally hence the need for all of them to be pointers
 * (pointers to "char *", "int" and "char *" respectively).
 *
 * Return value
 * ------------
 *	  0 if something was emitted for this check constraint (most common case).
 *	 -1 if there was an error
 */
int emit_check_constraint(char **buffer, int *buffer_size, char **buff_ptr, struct SqlStatement *stmt) {
	char *operation_name;
	int   ret;

	/* Following NULL check is placed to handle a parmless function processing. No additional processing needed for such a case.
	 * Example query that can pass NULL value to this function is:
	 * 	`create table test (id int check(parmless()>1));`
	 */
	if (NULL == stmt) {
		return 0;
	}
	switch (stmt->type) {
	case value_STATEMENT:;
		SqlValue *value;

		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch (value->type) {
		case CALCULATED_VALUE:
			ret = emit_check_constraint(buffer, buffer_size, buff_ptr, value->v.calculated);
			if (0 > ret) {
				return ret;
			}
			break;
		case COERCE_TYPE:;
			SqlDataType data_type;
			char *	    type_name;

			ret = emit_check_constraint(buffer, buffer_size, buff_ptr, value->v.coerce_target);
			if (0 > ret) {
				return ret;
			}
			data_type = value->u.coerce_type.coerced_type.data_type;
			type_name = get_user_visible_type_string(get_sqlvaluetype_from_sqldatatype(data_type, FALSE));
			if ('\0' == *type_name) {
				return -1; /* Data type was corrupt */
			}
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "::%s", type_name);
			if (SIZE_OR_PRECISION_UNSPECIFIED != value->u.coerce_type.coerced_type.size_or_precision) {
				INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "(%d",
									    value->u.coerce_type.coerced_type.size_or_precision);
				if (SCALE_UNSPECIFIED != value->u.coerce_type.coerced_type.scale) {
					INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, ",%d",
										    value->u.coerce_type.coerced_type.scale);
				}
				INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, ")");
			}
			break;
		case BOOLEAN_VALUE:;
			char *boolean_string;

			switch (value->v.string_literal[0]) {
			case '0':
				boolean_string = "FALSE";
				break;
			case '1':
				boolean_string = "TRUE";
				break;
			case '\0':
				boolean_string = "UNKNOWN";
				break;
			default:
				assert(FALSE);
				return -1;
				break;
			}
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "%s", boolean_string);
			break;
		case BOOLEAN_OR_STRING_LITERAL:
			/* All literals with this type that have not already been cast to a BOOLEAN_VALUE type
			 * should default back to a STRING_LITERAL.
			 */
			FIX_TYPE_TO_STRING_LITERAL(value->type);
			/* Note: Below comment is needed to avoid gcc [-Wimplicit-fallthrough=] warning */
			/* fall through */
		case STRING_LITERAL:
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "'%s'", value->v.string_literal);
			break;
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case FUNCTION_NAME:
		case COLUMN_REFERENCE:
			if (value->is_double_quoted) {
				assert((FUNCTION_NAME == value->type) || (COLUMN_REFERENCE == value->type));
				INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "\"%s\"",
									    value->v.reference);
			} else {
				INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "%s",
									    value->v.reference);
			}
			break;
		case NUL_VALUE:
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "NULL");
			break;
		case TABLE_ASTERISK:
		case SELECT_ASTERISK:
		case PARAMETER_VALUE:
		case FUNCTION_HASH:
		case DELIM_VALUE:
		case IS_NULL_LITERAL:
		case INVALID_SqlValueType:
		case UNKNOWN_SqlValueType:
			/* These usages should not be possible inside CHECK constraints. Assert accordingly. */
			assert(FALSE);
			return -1;
			break;
		}
		break;
	case unary_STATEMENT:;
		SqlUnaryOperation *unary;

		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "(");
		operation_name = get_user_visible_unary_operator_string(unary->operation);
		if ('\0' == *operation_name) {
			return -1; /* Unary operation enum value was corrupt */
		}
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "%s ", operation_name);
		ret = emit_check_constraint(buffer, buffer_size, buff_ptr, unary->operand);
		if (0 > ret) {
			return ret;
		}
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, ")");
		break;
	case binary_STATEMENT:;
		SqlBinaryOperation *binary;

		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "(");
		ret = emit_check_constraint(buffer, buffer_size, buff_ptr, binary->operands[0]);
		if (0 > ret) {
			return ret;
		}
		operation_name = get_user_visible_binary_operator_string(binary->operation);
		if ('\0' == *operation_name) {
			return -1; /* Binary operation enum value was corrupt */
		}
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " %s ", operation_name);
		ret = emit_check_constraint(buffer, buffer_size, buff_ptr, binary->operands[1]);
		if (0 > ret) {
			return ret;
		}
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, ")");
		break;
	case function_call_STATEMENT:;
		SqlFunctionCall *function_call;

		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		ret = emit_check_constraint(buffer, buffer_size, buff_ptr, function_call->function_name);
		if (0 > ret) {
			return ret;
		}
		ret = emit_check_constraint(buffer, buffer_size, buff_ptr, function_call->parameters);
		if (0 > ret) {
			return ret;
		}
		break;
	case column_list_STATEMENT:;
		SqlColumnList *start_column_list, *cur_column_list;

		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "(");
		UNPACK_SQL_STATEMENT(start_column_list, stmt, column_list);
		cur_column_list = start_column_list;
		do {
			ret = emit_check_constraint(buffer, buffer_size, buff_ptr, cur_column_list->value);
			if (0 > ret) {
				return ret;
			}
			cur_column_list = cur_column_list->next;
			if (cur_column_list != start_column_list) {
				INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, ", ");
			}
		} while (cur_column_list != start_column_list);
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, ")");
		break;
	case coalesce_STATEMENT:;
		SqlCoalesceCall *coalesce_call;

		UNPACK_SQL_STATEMENT(coalesce_call, stmt, coalesce);
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "COALESCE");
		ret = emit_check_constraint(buffer, buffer_size, buff_ptr, coalesce_call->arguments);
		if (0 > ret) {
			return ret;
		}
		break;
	case greatest_STATEMENT:;
		SqlGreatest *greatest_call;

		UNPACK_SQL_STATEMENT(greatest_call, stmt, greatest);
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "GREATEST");
		ret = emit_check_constraint(buffer, buffer_size, buff_ptr, greatest_call->arguments);
		if (0 > ret) {
			return ret;
		}
		break;
	case least_STATEMENT:;
		SqlLeast *least_call;

		UNPACK_SQL_STATEMENT(least_call, stmt, least);
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "LEAST");
		ret = emit_check_constraint(buffer, buffer_size, buff_ptr, least_call->arguments);
		if (0 > ret) {
			return ret;
		}
		break;
	case null_if_STATEMENT:;
		SqlNullIf *null_if_call;

		UNPACK_SQL_STATEMENT(null_if_call, stmt, null_if);
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "NULLIF");
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "(");
		ret = emit_check_constraint(buffer, buffer_size, buff_ptr, null_if_call->left);
		if (0 > ret) {
			return ret;
		}
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, ",");
		ret = emit_check_constraint(buffer, buffer_size, buff_ptr, null_if_call->right);
		if (0 > ret) {
			return ret;
		}
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, ")");
		break;
	case cas_STATEMENT:;
		SqlCaseStatement *cas;

		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "CASE ");
		if (NULL != cas->value) {
			ret = emit_check_constraint(buffer, buffer_size, buff_ptr, cas->value);
			if (0 > ret) {
				return ret;
			}
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " ");
		}
		ret = emit_check_constraint(buffer, buffer_size, buff_ptr, cas->branches);
		if (0 > ret) {
			return ret;
		}
		if (NULL != cas->optional_else) {
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " ELSE ");
			ret = emit_check_constraint(buffer, buffer_size, buff_ptr, cas->optional_else);
			if (0 > ret) {
				return ret;
			}
		}
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " END");
		break;
	case cas_branch_STATEMENT:;
		SqlCaseBranchStatement *start_cas_branch;
		SqlCaseBranchStatement *cur_cas_branch;

		UNPACK_SQL_STATEMENT(start_cas_branch, stmt, cas_branch);
		cur_cas_branch = start_cas_branch;
		do {
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "WHEN ");
			ret = emit_check_constraint(buffer, buffer_size, buff_ptr, cur_cas_branch->condition);
			if (0 > ret) {
				return ret;
			}
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " THEN ");
			ret = emit_check_constraint(buffer, buffer_size, buff_ptr, cur_cas_branch->value);
			if (0 > ret) {
				return ret;
			}
			cur_cas_branch = cur_cas_branch->next;
		} while (cur_cas_branch != start_cas_branch);
		break;

	/* The below types are not possible currently in a CHECK constraint */
	case create_table_STATEMENT:
	case column_STATEMENT:
	case keyword_STATEMENT:
	case constraint_STATEMENT:
	case create_function_STATEMENT:
	case parameter_type_list_STATEMENT:
	case data_type_struct_STATEMENT:
	case select_STATEMENT:
	case insert_STATEMENT:
	case drop_table_STATEMENT:
	case drop_function_STATEMENT:
	case truncate_table_STATEMENT:
	case aggregate_function_STATEMENT:
	case join_STATEMENT:
	case column_list_alias_STATEMENT:
	case column_alias_STATEMENT:
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
	case begin_STATEMENT:
	case commit_STATEMENT:
	case set_STATEMENT:
	case show_STATEMENT:
	case no_data_STATEMENT:
	case delim_char_list_STATEMENT:
	case index_STATEMENT:
	case join_type_STATEMENT:
	case discard_all_STATEMENT:
	case row_value_STATEMENT:
	case table_value_STATEMENT:
	case array_STATEMENT:
	case history_STATEMENT:
	case delete_from_STATEMENT:
	case update_STATEMENT:
	case display_relation_STATEMENT:
	case invalid_STATEMENT:
		/* Do not add "default:" case as we want to enumerate each explicit case here instead of having a
		 * general purpose bucket where all types not listed above fall into as that could hide subtle bugs.
		 */
		assert(FALSE);
		FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
		return -1;
		break;
	}
	return 0;
}
