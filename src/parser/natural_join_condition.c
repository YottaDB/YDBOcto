/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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

/* Input parameters:
 *	"start" is start of list of tablejoins in this query
 *	"r_join" is current join (right end of the tablejoin list) whose "condition" needs to be filled in.
 *
 * This function goes through all joins from "start" till before "r_join" and checks all those columns against
 *	the columns in "r_join" and constructs a JOIN condition which is basically a WHERE clause that implements
 *	an EQUIJOIN of all columns with the same name on either side of the NATURAL JOIN
 *	(e.g. WHERE left.COLUMN1 = right.COLUMN1 AND left.COLUMN2 = right.COLUMN2 ...)
 *
 * Returns:
 *	0 in case of success.
 *	1 in case of errors (duplicate common columns etc.) so caller can take appropriate action.
 */
int natural_join_condition(SqlJoin *start, SqlJoin *r_join) {
	SqlStatement  *ret;
	SqlJoin	      *l_join;
	SqlStatement  *r_sql_stmt;
	SqlTableAlias *r_table_alias;
	char	      *r_table_name;
	size_t	       r_table_name_len;
	SqlValue      *value;

	assert(NULL != start);
	assert(NULL != r_join);
	assert(start != r_join);
	assert(NATURAL_JOIN == r_join->type);
	r_sql_stmt = drill_to_table_alias(r_join->value);
	UNPACK_SQL_STATEMENT(r_table_alias, r_sql_stmt, table_alias);
	UNPACK_SQL_STATEMENT(value, r_table_alias->alias, value);
	r_table_name = value->v.string_literal;
	r_table_name_len = strlen(r_table_name);
	ret = NULL;
	/* Loop through each table on left side of NATURAL JOIN */
	for (l_join = start; l_join != r_join; l_join = l_join->next) {
		SqlStatement	   *l_sql_stmt;
		SqlTableAlias	   *l_table_alias;
		SqlColumnListAlias *l_cl_start, *l_cl_cur;
		char		   *l_table_name;
		size_t		    l_table_name_len;

		l_sql_stmt = drill_to_table_alias(l_join->value);
		UNPACK_SQL_STATEMENT(l_table_alias, l_sql_stmt, table_alias);
		UNPACK_SQL_STATEMENT(l_cl_start, l_table_alias->column_list, column_list_alias);
		UNPACK_SQL_STATEMENT(value, l_table_alias->alias, value);
		l_table_name = value->v.string_literal;
		l_table_name_len = strlen(l_table_name);
		l_cl_cur = l_cl_start;
		do { /* Loop through each column on left side table of the NATURAL JOIN */
			if (NULL == l_cl_cur->duplicate_of_column) {
				char		   *l_column_name;
				size_t		    l_column_name_len;
				SqlColumnListAlias *r_matched_column;
				boolean_t	    ambiguous;

				UNPACK_SQL_STATEMENT(value, l_cl_cur->alias, value);
				assert(CALCULATED_VALUE != value->type);
				l_column_name = value->v.string_literal;
				l_column_name_len = strlen(l_column_name);
				r_matched_column
				    = match_column_in_table(r_table_alias, l_column_name, l_column_name_len, &ambiguous, FALSE);
				/* FALSE for last parameter so no error is issued in "match_column_in_table"
				 * if multiple columns are found on right table with the same name. This is
				 * because we will issue the more accurate ERR_COMMON_COLUMN error below.
				 */
				if (NULL != r_matched_column) {
					SqlStatement	   *l_qual_col_name, *r_qual_col_name;
					SqlStatement	   *cur_condition;
					SqlBinaryOperation *binary;

					if (NULL != r_matched_column->duplicate_of_column) {
						ERROR(ERR_COMMON_COLUMN, l_column_name, "left");
						return 1;
					}
					if (ambiguous) {
						ERROR(ERR_COMMON_COLUMN, l_column_name, "right");
						return 1;
					}
					assert(NULL == r_matched_column->duplicate_of_column);
					r_matched_column->duplicate_of_column = l_cl_cur;

					// Create a value for the left item
					SQL_STATEMENT(l_qual_col_name, value_STATEMENT);
					MALLOC_STATEMENT(l_qual_col_name, value, SqlValue);
					UNPACK_SQL_STATEMENT(value, l_qual_col_name, value);
					value->type = COLUMN_REFERENCE;
					value->v.string_literal
					    = octo_cmalloc(memory_chunks, l_table_name_len + l_column_name_len + 2);
					sprintf(value->v.string_literal, "%s.%s", l_table_name, l_column_name);
					//
					// Create a value for the right item
					SQL_STATEMENT(r_qual_col_name, value_STATEMENT);
					MALLOC_STATEMENT(r_qual_col_name, value, SqlValue);
					UNPACK_SQL_STATEMENT(value, r_qual_col_name, value);
					value->type = COLUMN_REFERENCE;
					value->v.string_literal
					    = octo_cmalloc(memory_chunks, r_table_name_len + l_column_name_len + 2);
					sprintf(value->v.string_literal, "%s.%s", r_table_name, l_column_name);

					// Put both in a binary statement
					SQL_STATEMENT(cur_condition, binary_STATEMENT);
					MALLOC_STATEMENT(cur_condition, binary, SqlBinaryOperation);
					UNPACK_SQL_STATEMENT(binary, cur_condition, binary);
					binary->operands[0] = l_qual_col_name;
					binary->operands[1] = r_qual_col_name;
					binary->operation = BOOLEAN_EQUALS;
					if (NULL == ret) {
						ret = cur_condition;
					} else {
						SqlStatement *t_condition;

						SQL_STATEMENT(t_condition, binary_STATEMENT);
						MALLOC_STATEMENT(t_condition, binary, SqlBinaryOperation);
						UNPACK_SQL_STATEMENT(binary, t_condition, binary);
						binary->operation = BOOLEAN_AND;
						binary->operands[0] = ret;
						binary->operands[1] = cur_condition;
						ret = t_condition;
					}
				}
			}
			/* else: This is a duplicate column as part of a previous NATURAL JOIN. Ignore this. */
			l_cl_cur = l_cl_cur->next;
		} while (l_cl_cur != l_cl_start);
	}
	r_join->condition = ret;
	return 0;
}
