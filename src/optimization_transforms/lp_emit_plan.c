/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

#define EMIT_SNPRINTF(WRITTEN, BUFF_PTR, BUFFER, BUFFER_SIZE, ...)                                \
	{                                                                                         \
		size_t bufSize;                                                                   \
                                                                                                  \
		bufSize = ((0 >= (signed)BUFFER_SIZE) ? 0 : (BUFFER_SIZE - (BUFFER - BUFF_PTR))); \
		WRITTEN = snprintf(BUFF_PTR, bufSize, ##__VA_ARGS__);                             \
		BUFF_PTR += WRITTEN;                                                              \
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
		char *	  str, keyWordStr[64];                                                         \
		SqlValue *value;                                                                       \
                                                                                                       \
		str = NULL;                                                                            \
		switch (KEYWORD->keyword) {                                                            \
		case NO_KEYWORD:                                                                       \
			break;                                                                         \
		case PRIMARY_KEY:                                                                      \
			str = "PRIMARY_KEY";                                                           \
			break;                                                                         \
		case NOT_NULL:                                                                         \
			str = "NOT_NULL";                                                              \
			break;                                                                         \
		case UNIQUE_CONSTRAINT:                                                                \
			str = "UNIQUE_CONSTRAINT";                                                     \
			break;                                                                         \
		case OPTIONAL_SOURCE:                                                                  \
			str = "SOURCE";                                                                \
			break;                                                                         \
		case OPTIONAL_END:                                                                     \
			str = "END";                                                                   \
			break;                                                                         \
		case OPTIONAL_START:                                                                   \
			str = "START";                                                                 \
			break;                                                                         \
		case OPTIONAL_STARTINCLUDE:                                                            \
			str = "STARTINCLUDE";                                                          \
			break;                                                                         \
		case OPTIONAL_DELIM:                                                                   \
			str = "DELIM";                                                                 \
			break;                                                                         \
		case OPTIONAL_EXTRACT:                                                                 \
			str = "EXTRACT";                                                               \
			break;                                                                         \
		case OPTIONAL_CASCADE:                                                                 \
			str = "CASCADE";                                                               \
			break;                                                                         \
		case OPTIONAL_RESTRICT:                                                                \
			str = "RESTRICT";                                                              \
			break;                                                                         \
		case OPTIONAL_PIECE:                                                                   \
			str = "PIECE";                                                                 \
			break;                                                                         \
		case OPTIONAL_KEY_NUM:                                                                 \
			str = "KEY_NUM";                                                               \
			break;                                                                         \
		case OPTIONAL_ADVANCE:                                                                 \
			str = "ADVANCE";                                                               \
			break;                                                                         \
		case OPTIONAL_LIMIT:                                                                   \
			UNPACK_SQL_STATEMENT(value, KEYWORD->v, value);                                \
			snprintf(keyWordStr, sizeof(keyWordStr), "LIMIT %s", value->v.string_literal); \
			str = keyWordStr;                                                              \
			break;                                                                         \
		case OPTIONAL_DISTINCT:                                                                \
			str = "DISTINCT";                                                              \
			break;                                                                         \
		case OPTIONAL_XREF_INDEX:                                                              \
			str = "XREF_INDEX";                                                            \
			break;                                                                         \
		case OPTIONAL_BOOLEAN_EXPANSION:                                                       \
			str = "BOOLEAN_EXPANSION";                                                     \
			break;                                                                         \
		case OPTIONAL_ASC:                                                                     \
			str = "ASC";                                                                   \
			break;                                                                         \
		case OPTIONAL_DESC:                                                                    \
			str = "DESC";                                                                  \
			break;                                                                         \
		default:                                                                               \
			assert(FALSE);                                                                 \
			break;                                                                         \
		}                                                                                      \
		if (NULL != str)                                                                       \
			EMIT_SNPRINTF(WRITTEN, BUFF_PTR, BUFFER, BUFFER_LEN, " %s;", str);             \
	}

int emit_plan_helper(char *buffer, size_t buffer_len, int depth, LogicalPlan *plan);

void lp_emit_plan(LogicalPlan *plan, char *stage) {
	char * buffer, *buff_ptr;
	size_t buffer_len, written;
	size_t actual_len;

	if (DEBUG < config->verbosity_level) {
		return;
	}
	/* Find out how much space is needed by the below first invocation. Hence the 2nd parameter of 0 */
	buffer_len = emit_plan_helper(NULL, 0, 0, plan);
	// We use malloc here since it is a large temporary buffer
	// No need to force it to stay around until compilation ends
	buffer = malloc(buffer_len + 2); /* 1 byte for EMIT_SNPRINTF("\n") below, 1 byte for trailing null terminator */
	buff_ptr = buffer;
	EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n");
	actual_len = emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), 0, plan);
#ifndef NDEBUG
	assert(actual_len == buffer_len);
#else
	UNUSED(actual_len); /* in Release builds, this avoids a [-Wunused-but-set-variable] warning */
#endif
	DEBUG(INFO_CURPLAN, stage, buffer);
	free(buffer);
	return;
}

int emit_plan_helper(char *buffer, size_t buffer_len, int depth, LogicalPlan *plan) {
	char *		    buff_ptr, *table_name, *column_name, *data_type_ptr;
	size_t		    written;
	int		    table_id;
	SqlValue *	    value;
	SqlKey *	    key;
	SqlOptionalKeyword *start_keyword, *cur_keyword;

	if (NULL == plan)
		return 0;
	buff_ptr = buffer;
	EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%*s%s: ", depth, "", lp_action_type_str[plan->type]);
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
			buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 4, key->fixed_to_value);
		}
		break;
	case LP_COLUMN_LIST:
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n");
		buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.lp_default.operand[0]);
		buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.lp_default.operand[1]);
		break;
	case LP_VALUE:
		value = plan->v.lp_value.value;
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "'%s'\n", value->v.string_literal);
		break;
	case LP_TABLE:
		UNPACK_SQL_STATEMENT(value, plan->v.lp_table.table_alias->alias, value);
		EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "%s\n", value->v.string_literal);
		break;
	case LP_COLUMN_ALIAS:
		if (column_STATEMENT == plan->v.lp_column_alias.column_alias->column->type) {
			UNPACK_SQL_STATEMENT(value, plan->v.lp_column_alias.column_alias->column->v.column->columnName, value);
			column_name = value->v.string_literal;
		} else {
			UNPACK_SQL_STATEMENT(value, plan->v.lp_column_alias.column_alias->column->v.column_list_alias->alias,
					     value);
			column_name = value->v.string_literal;
		}
		UNPACK_SQL_STATEMENT(value, plan->v.lp_column_alias.column_alias->table_alias_stmt->v.table_alias->alias, value);
		table_name = value->v.string_literal;
		table_id = plan->v.lp_column_alias.column_alias->table_alias_stmt->v.table_alias->unique_id;
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
				buff_ptr
				    += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, join_on_condition);
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
				EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len,
					      "[pre_coerce_type = %s] [post_coerce_type = %s]:",
					      get_user_visible_type_string(plan->extra_detail.lp_coerce_type.pre_coerce_type),
					      get_user_visible_type_string(plan->extra_detail.lp_coerce_type.coerce_type));
			}
			EMIT_SNPRINTF(written, buff_ptr, buffer, buffer_len, "\n");
		}
		buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.lp_default.operand[0]);
		buff_ptr += emit_plan_helper(buff_ptr, buffer_len - (buff_ptr - buffer), depth + 2, plan->v.lp_default.operand[1]);
		break;
	}
	return buff_ptr - buffer;
}
