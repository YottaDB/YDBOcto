/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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
#include <string.h>

#include "octo.h"
#include "octo_types.h"

// Construct a WHERE statement by finding all columns in left and all columns in right
// which have the same name, and saying left.COLUMN = right.COLUMN AND ...
SqlStatement *natural_join_condition(SqlStatement *left, SqlStatement *right) {
	SqlStatement *r_matched_column, *ret, *cur_condition, *t_condition;
	SqlStatement *l_qual_col_name, *r_qual_col_name;
	SqlJoin *j_left, *j_right, *cur_join;
	SqlJoin *r_cur_join;
	SqlTableAlias *cur_alias, *r_cur_alias;
	SqlColumnListAlias *cl_start, *cl_cur;
	SqlValue *value;
	SqlBinaryOperation *binary;
	char *column_name, *table_name, *r_table_name;
	size_t column_name_len, table_name_len, r_table_name_len;
	UNPACK_SQL_STATEMENT(j_left, left, join);
	UNPACK_SQL_STATEMENT(j_right, right, join);

	ret = NULL;

	cur_join = j_left;
	do {
		SqlStatement *left_sql_stmt;

		left_sql_stmt = drill_to_table_alias(cur_join->value);
		UNPACK_SQL_STATEMENT(cur_alias, left_sql_stmt, table_alias);
		UNPACK_SQL_STATEMENT(cl_start, cur_alias->column_list, column_list_alias);
		UNPACK_SQL_STATEMENT(value, cur_alias->alias, value);
		table_name = value->v.string_literal;
		table_name_len = strlen(table_name);
		cl_cur = cl_start;
		do {
			UNPACK_SQL_STATEMENT(value, cl_start->alias, value);
			assert(value->type != CALCULATED_VALUE);
			column_name = value->v.string_literal;
			column_name_len = strlen(column_name);
			// Check each of rights tables for the item in question
			r_cur_join = j_right;
			do {
				SqlStatement *right_sql_stmt;

				right_sql_stmt = drill_to_table_alias(r_cur_join->value);
				UNPACK_SQL_STATEMENT(r_cur_alias, right_sql_stmt, table_alias);
				r_matched_column = match_column_in_table(r_cur_alias, column_name, column_name_len);
				if(r_matched_column != NULL) {
					UNPACK_SQL_STATEMENT(value, r_cur_alias->alias, value);
					r_table_name = value->v.string_literal;
					r_table_name_len = strlen(table_name);

					// Create a value for the left item
					SQL_STATEMENT(l_qual_col_name, value_STATEMENT);
					MALLOC_STATEMENT(l_qual_col_name, value, SqlValue);
					UNPACK_SQL_STATEMENT(value, l_qual_col_name, value);
					value->type = COLUMN_REFERENCE;
					value->v.string_literal = octo_cmalloc(memory_chunks,
							table_name_len + column_name_len + 2);
					sprintf(value->v.string_literal, "%s.%s", table_name, column_name);
					//
					// Create a value for the right item
					SQL_STATEMENT(r_qual_col_name, value_STATEMENT);
					MALLOC_STATEMENT(r_qual_col_name, value, SqlValue);
					UNPACK_SQL_STATEMENT(value, r_qual_col_name, value);
					value->type = COLUMN_REFERENCE;
					value->v.string_literal = octo_cmalloc(memory_chunks,
							r_table_name_len + column_name_len + 2);
					sprintf(value->v.string_literal, "%s.%s", r_table_name, column_name);

					// Put both in a binary statement
					SQL_STATEMENT(cur_condition, binary_STATEMENT);
					MALLOC_STATEMENT(cur_condition, binary, SqlBinaryOperation);
					UNPACK_SQL_STATEMENT(binary, cur_condition, binary);
					binary->operands[0] = l_qual_col_name;
					binary->operands[1] = r_qual_col_name;
					binary->operation = BOOLEAN_EQUALS;
					if(ret == NULL) {
						ret = cur_condition;
					} else {
						SQL_STATEMENT(t_condition, binary_STATEMENT);
						MALLOC_STATEMENT(t_condition, binary, SqlBinaryOperation);
						UNPACK_SQL_STATEMENT(binary, t_condition, binary);
						binary->operation = BOOLEAN_AND;
						binary->operands[0] = ret;
						binary->operands[1] = cur_condition;
						ret = t_condition;
					}
				}
				r_cur_join = r_cur_join->next;
			} while(r_cur_join != j_right);
			cl_cur = cl_cur->next;
		} while(cl_cur != cl_start);
		cur_join = cur_join->next;
	} while(cur_join != j_left);
	return ret;
}
