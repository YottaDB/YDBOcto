/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
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
#include "logical_plan.h"

#define IS_PLAN_TYPE_A_LIST(TYPE) ((TYPE == LP_COLUMN_LIST) || (TYPE == LP_ROW_VALUE))

#define EMIT_SNPRINTF(WRITTEN, BUFF_PTR, BUFFER, BUFFER_SIZE, ...)                                \
	{                                                                                         \
		size_t bufSize;                                                                   \
                                                                                                  \
		bufSize = ((0 >= (signed)BUFFER_SIZE) ? 0 : (BUFFER_SIZE - (BUFFER - BUFF_PTR))); \
		WRITTEN = snprintf(BUFF_PTR, bufSize, ##__VA_ARGS__);                             \
		BUFF_PTR += WRITTEN;                                                              \
		assert(0 <= (signed)bufSize);                                                     \
		/* Caller should have reserved space for trailing null hence the "<" below */     \
		assert((0 == bufSize) || (WRITTEN < bufSize));                                    \
	}

#define EMIT_SNPRINTF_JOIN_TYPE_IF_NEEDED(WRITTEN, BUFF_PTR, BUFFER, BUFFER_LEN, PLAN)     \
	{                                                                                  \
		enum SqlJoinType join_type;                                                \
                                                                                           \
		assert(LP_TABLE_JOIN == PLAN->type);                                       \
		join_type = PLAN->extra_detail.lp_table_join.cur_join_type;                \
		if (join_type) {                                                           \
			char *str;                                                         \
                                                                                           \
			switch (join_type) {                                               \
			case NO_JOIN:                                                      \
				str = "NO_JOIN";                                           \
				break;                                                     \
			case CROSS_JOIN:                                                   \
				str = "CROSS_JOIN";                                        \
				break;                                                     \
			case INNER_JOIN:                                                   \
				str = "INNER_JOIN";                                        \
				break;                                                     \
			case RIGHT_JOIN:                                                   \
				str = "RIGHT_JOIN";                                        \
				break;                                                     \
			case LEFT_JOIN:                                                    \
				str = "LEFT_JOIN";                                         \
				break;                                                     \
			case FULL_JOIN:                                                    \
				str = "FULL_JOIN";                                         \
				break;                                                     \
			case NATURAL_JOIN:                                                 \
				str = "NATURAL_JOIN";                                      \
				break;                                                     \
			default:                                                           \
				assert(FALSE);                                             \
				str = "INVALID_JOIN";                                      \
				break;                                                     \
			}                                                                  \
			EMIT_SNPRINTF(WRITTEN, BUFF_PTR, BUFFER, BUFFER_LEN, "%s: ", str); \
		}                                                                          \
	}

/* The below code needs to be kept in sync with `enum OptionalKeyword` in "octo_types.h" */
#define EMIT_SNPRINTF_KEYWORD_IF_NEEDED(WRITTEN, BUFF_PTR, BUFFER, BUFFER_LEN, KEYWORD)                \
	{                                                                                              \
		char *	  keywordName, keyWordStr[64];                                                 \
		SqlValue *value;                                                                       \
                                                                                                       \
		switch (KEYWORD->keyword) {                                                            \
		case NO_KEYWORD:                                                                       \
			keywordName = NULL;                                                            \
			break;                                                                         \
		case OPTIONAL_LIMIT:                                                                   \
			UNPACK_SQL_STATEMENT(value, KEYWORD->v, value);                                \
			snprintf(keyWordStr, sizeof(keyWordStr), "LIMIT %s", value->v.string_literal); \
			keywordName = keyWordStr;                                                      \
			break;                                                                         \
		default:                                                                               \
			keywordName = get_keyword_name(KEYWORD->keyword);                              \
			break;                                                                         \
		}                                                                                      \
		if (NULL != keywordName)                                                               \
			EMIT_SNPRINTF(WRITTEN, BUFF_PTR, BUFFER, BUFFER_LEN, " %s;", keywordName);     \
	}

int emit_plan_helper(char *buffer, size_t buffer_len, int depth, LogicalPlan *plan, LogicalPlan *parent_plan);

void lp_emit_plan(LogicalPlan *plan, char *stage) {
	char * buffer, *buff_ptr;
	size_t buffer_len, written;
	DEBUG_ONLY(size_t actual_len);

	if (DEBUG < config->verbosity_level) {
		return;
	}
	/* Find out how much space is needed by the below first invocation. Hence the 2nd parameter of 0 */
	buffer_len = emit_plan_helper(NULL, 0, 0, plan, NULL);
	// We use malloc here since it is a large temporary buffer
	// No need to force it to stay around until compilation ends
	buffer = malloc(buffer_len + 2); /* 1 byte for EMIT_SNPRINTF("\n") below, 1 byte for trailing null terminator */
	buff_ptr = buffer;
	EMIT_SNPRINTF(written, buff_ptr, buffer, 2, "\n"); /* 2 includes space for \n and trailing null (needed by snprintf) */
	/* Note that "buffer_len" does not include space for the leading \n or trailing null terminator.
	 * So need to add space for trailing null byte (needed by snprintf calls inside that function).
	 */
	DEBUG_ONLY(actual_len =) emit_plan_helper(buff_ptr, buffer_len + 1, 0, plan, NULL);
	DEBUG_ONLY(assert(actual_len == buffer_len));
	DEBUG(INFO_CURPLAN, stage, buffer);
	free(buffer);
	return;
}

int emit_plan_helper(char *buffer, size_t buffer_len, int depth, LogicalPlan *plan, LogicalPlan *parent_plan) {
	char *		    buff_ptr, *table_name, *column_name, *data_type_ptr;
	size_t		    written;
	int		    table_id;
	SqlValue *	    value;
	SqlKey *	    key;
	SqlOptionalKeyword *start_keyword, *cur_keyword;
	boolean_t	    skip_emit;
	SqlColumnAlias *    column_alias;
	SqlStatement *	    column;

	if (NULL == plan)
		return 0;
	buff_ptr = buffer;
	/* If both parent and child plans are of type LP_COLUMN_LIST, then skip printing the line containing
	 * LP_COLUMN_LIST of the child plan. This eliminates redundant output and lets the emitted plan contain
	 * just the actual list (minus the LP_COLUMN_LIST plans at each level which are an implementation overhead
	 * and is best not seen by the user). The variable "skip_emit" helps track this.
	 * The same reasoning applies to a LP_ROW_VALUE type plan too.
	 */
	if ((NULL == parent_plan) || !IS_PLAN_TYPE_A_LIST(parent_plan->type) || !IS_PLAN_TYPE_A_LIST(plan->type)
	    || (parent_plan->type != plan->type)) {
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s%s: ", depth, "", lp_action_type_str[plan->type]);
		skip_emit = FALSE;
	} else {
		skip_emit = TRUE;
	}
	switch (plan->type) {
	case LP_PIECE_NUMBER:
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%d\n", plan->v.lp_piece_number.piece_number);
		break;
	case LP_KEY:
		key = plan->v.lp_key.key;
		if (key->column) {
			UNPACK_SQL_STATEMENT(value, key->column->columnName, value);
			column_name = value->v.string_literal;
		} else {
			column_name = "";
		}
		if (key->table) {
			UNPACK_SQL_STATEMENT(value, key->table->tableName, value);
			table_name = value->v.string_literal;
		} else {
			table_name = "";
		}
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n%*s- table_name: %s\n", depth, "", table_name);
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s- column_name: %s\n", depth, "", column_name);
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s- unique_id: %d\n", depth, "", key->unique_id);
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s- method: %s\n", depth, "", lp_action_type_str[key->type]);
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s- xref_key: %s\n", depth, "",
			      key->is_cross_reference_key ? "true" : "false");
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s- uses_xref_key: %s\n", depth, "",
			      key->cross_reference_output_key ? "true" : "false");
		if (LP_KEY_FIX == key->type) {
			EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s- value:\n", depth, "");
			buff_ptr
			    += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 4, key->fixed_to_value, plan);
		}
		break;
	case LP_COLUMN_LIST:
	case LP_ROW_VALUE:
	case LP_CHECK_CONSTRAINT:
		if (LP_CHECK_CONSTRAINT == plan->type) {
			SqlConstraint *constraint;
			SqlValue *     value;

			constraint = plan->extra_detail.lp_check_constraint.constraint;
			UNPACK_SQL_STATEMENT(value, constraint->name, value);
			EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%s", value->v.string_literal);
		}
		if (!skip_emit) {
			EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n");
		}
		buff_ptr
		    += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.lp_default.operand[0], plan);
		/* For "case LP_COLUMN_LIST", operand[1] is a sibling LP_COLUMN_LIST and should be treated at the same level as
		 * the parent LP_COLUMN_LIST hence using "depth" instead of "depth + 2" like was done for operand[0].
		 * The same reasoning as above applies to "LP_ROW_VALUE" and "LP_CHECK_CONSTRAINT" too.
		 */
		buff_ptr
		    += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth, plan->v.lp_default.operand[1], plan);
		break;
	case LP_VALUE:
		value = plan->v.lp_value.value;
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "'%s'\n", value->v.string_literal);
		break;
	case LP_TABLE:
		UNPACK_SQL_STATEMENT(value, plan->v.lp_table.table_alias->alias, value);
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%s\n", value->v.string_literal);
		break;
	case LP_COLUMN:
		UNPACK_SQL_STATEMENT(value, plan->v.lp_column.column->columnName, value);
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%s\n", value->v.string_literal);
		break;
	case LP_INSERT_INTO_COL:
		/* In this case, the only member that we have is a column_alias so fall through to the below code */
	case LP_COLUMN_ALIAS:
		if (LP_COLUMN_ALIAS == plan->type) {
			column_alias = plan->v.lp_column_alias.column_alias;
		} else {
			column_alias = plan->v.lp_insert_into_col.column_alias;
		}
		column = column_alias->column;
		if (column_STATEMENT == column->type) {
			UNPACK_SQL_STATEMENT(value, column->v.column->columnName, value);
			column_name = value->v.string_literal;
		} else {
			if (value_STATEMENT == column->type) {
				assert(TABLE_ASTERISK == column->v.value->type);
				UNPACK_SQL_STATEMENT(value, column, value);
			} else {
				UNPACK_SQL_STATEMENT(value, column->v.column_list_alias->alias, value);
			}
			column_name = value->v.string_literal;
		}
		UNPACK_SQL_STATEMENT(value, column_alias->table_alias_stmt->v.table_alias->alias, value);
		table_name = value->v.string_literal;
		table_id = column_alias->table_alias_stmt->v.table_alias->unique_id;
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%s(%d).%s\n", table_name, table_id, column_name);
		break;
	case LP_COLUMN_LIST_ALIAS:
		switch (plan->v.lp_column_list_alias.column_list_alias->type) {
		case BOOLEAN_VALUE:
			data_type_ptr = "BOOLEAN_VALUE";
			break;
		case INTEGER_LITERAL:
			data_type_ptr = "INTEGER_LITERAL";
			break;
		case NUMERIC_LITERAL:
			data_type_ptr = "NUMERIC_LITERAL";
			break;
		case STRING_LITERAL:
			data_type_ptr = "STRING_LITERAL";
			break;
		case COLUMN_REFERENCE:
			data_type_ptr = "COLUMN_REFERENCE";
			break;
		case CALCULATED_VALUE:
			data_type_ptr = "CALCULATED_VALUE";
			break;
		case FUNCTION_NAME:
			data_type_ptr = "FUNCTION";
			break;
		case PARAMETER_VALUE:
			data_type_ptr = "PARAMETER";
			break;
		case COERCE_TYPE:
			data_type_ptr = "COERCE_TYPE";
			break;
		case NUL_VALUE:
			data_type_ptr = "NULL";
			break;
		case IS_NULL_LITERAL:
			data_type_ptr = "IS_NULL_LITERAL";
			break;
		case TABLE_ASTERISK:
			/* Note: This is not possible inside SELECT column list as .* would have been expanded to list of columns
			 * but is possible in GROUP BY or ORDER BY column list (.* does not get expanded to list of columns there).
			 */
			data_type_ptr = "TABLE_ASTERISK";
			break;
		default:
			assert(FALSE);
			data_type_ptr = "<INVALID>";
			break;
		}
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n%*s- type: %s", depth, "", data_type_ptr);
		if (NULL != plan->v.lp_column_list_alias.column_list_alias->alias) {
			/* Need to check above since alias can be NULL in case of ORDER BY */
			UNPACK_SQL_STATEMENT(value, plan->v.lp_column_list_alias.column_list_alias->alias, value);
			column_name = value->v.string_literal;
			EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n%*s- alias: %s", depth, "", column_name);
		}
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n");
		break;
	case LP_KEYWORDS:
		start_keyword = plan->v.lp_keywords.keywords;
		assert(NULL != start_keyword);
		cur_keyword = start_keyword;
		do {
			EMIT_SNPRINTF_KEYWORD_IF_NEEDED(written, buff_ptr, buffer, buffer_len, cur_keyword);
			cur_keyword = cur_keyword->next;
		} while (cur_keyword != start_keyword);
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n");
		break;
	default:
		if (LP_TABLE_JOIN == plan->type) {
			LogicalPlan *join_on_condition;

			EMIT_SNPRINTF_JOIN_TYPE_IF_NEEDED(written, buff_ptr, buffer, buffer_len, plan);
			join_on_condition = plan->extra_detail.lp_table_join.join_on_condition;
			if (NULL != join_on_condition) {
				EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n");
				buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2,
							     join_on_condition, plan);
			} else {
				EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n");
			}
		} else {
			if ((LP_ORDER_BY == plan->type) && plan->extra_detail.lp_order_by.direction) {
				char *		     str;
				enum OptionalKeyword direction;

				direction = plan->extra_detail.lp_order_by.direction;
				assert((OPTIONAL_ASC == direction) || (OPTIONAL_DESC == direction));
				str = (OPTIONAL_ASC == direction) ? "ASC" : "DESC";
				EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "ORDER BY %s: ", str);
			}
			if (LP_COERCE_TYPE == plan->type) {
				SqlValueType	   value_type;
				char *		   precision_and_scale;
				SqlDataTypeStruct *data_type_ptr;
				int		   size_or_precision, scale;
				/* The array size below takes into account space needed to store
				 * "(PRECISION,SCALE)" where PRECISION and SCALE are 4-byte integers.
				 * 3 bytes for "(", ")" and "," and 1 byte for NULL terminator.
				 */
				char buff[INT32_TO_STRING_MAX + INT32_TO_STRING_MAX + 4];

				data_type_ptr = &plan->extra_detail.lp_coerce_type.coerce_type;
				value_type = get_sqlvaluetype_from_sqldatatype(data_type_ptr->data_type, FALSE);
				size_or_precision = data_type_ptr->size_or_precision;
				scale = data_type_ptr->scale;
				if (SIZE_OR_PRECISION_UNSPECIFIED != size_or_precision) {
					int len;

					if (SCALE_UNSPECIFIED == scale) {
						len = snprintf(buff, sizeof(buff), "(%d)", size_or_precision);
					} else {
						len = snprintf(buff, sizeof(buff), "(%d,%d)", size_or_precision, scale);
					}
					assert(len < (int)sizeof(buff));
					UNUSED(len); /* needed to avoid [-Wunused-but-set-variable] warning from compiler */
					precision_and_scale = buff;
				} else {
					precision_and_scale = "";
				}
				EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len,
					      "[pre_coerce_type = %s] [post_coerce_type = %s%s]:",
					      get_user_visible_type_string(plan->extra_detail.lp_coerce_type.pre_coerce_type),
					      get_user_visible_type_string(value_type), precision_and_scale);
			}
			EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n");
		}
		buff_ptr
		    += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.lp_default.operand[0], plan);
		buff_ptr
		    += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.lp_default.operand[1], plan);
		break;
	}
	return buff_ptr - buffer;
}
