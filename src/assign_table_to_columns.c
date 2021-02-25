/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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

void assign_table_to_columns(SqlStatement *table_statement) {
	SqlColumn *cur_column, *start_column;
	SqlTable * table;
	int	   column_number, piece_number;

	UNPACK_SQL_STATEMENT(table, table_statement, create_table);
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	column_number = 0;
	piece_number = 1;
	do {
		boolean_t	    delim_is_empty, remove_piece_keyword;
		SqlOptionalKeyword *keyword, *piece_keyword;

		column_number++;
		cur_column->table = table_statement;
		keyword = get_keyword(cur_column, OPTIONAL_DELIM);
		delim_is_empty = FALSE;
		if (NULL != keyword) {
			SqlValue *value;
			char *	  delim, ch;

			cur_column->delim = keyword->v;
			/* Check if DELIM is "". If so, ignore any PIECE specifications as we want the entire node. */
			UNPACK_SQL_STATEMENT(value, keyword->v, value);
			delim = value->v.reference;
			ch = *delim;
			assert((DELIM_IS_DOLLAR_CHAR == ch) || (DELIM_IS_LITERAL == ch));
			if (DELIM_IS_LITERAL == ch) {
				delim++; /* skip first byte to get actual delimiter */
				ch = *delim;
				delim_is_empty = ('\0' == ch);
			}
		} else {
			assert(NULL == cur_column->delim);
		}
		/* Assign each column a PIECE number if one was not explicitly specified.
		 * PRIMARY KEY columns (those that have a PRIMARY_KEY or OPTIONAL_KEY_NUM specified) are not
		 * counted towards the default piece #.
		 */
		piece_keyword = get_keyword(cur_column, OPTIONAL_PIECE);
		remove_piece_keyword = FALSE;
		if ((NULL == get_keyword(cur_column, PRIMARY_KEY)) && (NULL == get_keyword(cur_column, OPTIONAL_KEY_NUM))) {
			/* Add PIECE keyword only if DELIM is not "" */
			if (NULL == piece_keyword) {
				if (!delim_is_empty) {
					SqlOptionalKeyword *column_keywords, *new_piece_keyword;

					new_piece_keyword = add_optional_piece_keyword_to_sql_column(piece_number);
					UNPACK_SQL_STATEMENT(column_keywords, cur_column->keywords, keyword);
					dqappend(column_keywords, new_piece_keyword);
				}
				/* PIECE was not explicitly specified for this non-key column so count this column towards
				 * the default piece number that is used for other non-key columns with no PIECE specified.
				 * Note that this is done even in the case DELIM "" is specified for a non-key column.
				 */
				piece_number++;
			} else if (delim_is_empty) {
				/* PIECE numbers are not applicable for non-key columns with DELIM "" so remove it */
				remove_piece_keyword = TRUE;
			}
		} else if (NULL != piece_keyword) {
			/* PIECE numbers (if specified) are not applicable for primary key columns so remove it */
			remove_piece_keyword = TRUE;
		}
		if (remove_piece_keyword) {
			SqlOptionalKeyword *next;

			next = piece_keyword->next; /* Note down next before "dqdel" */
			dqdel(piece_keyword);
			if (piece_keyword == cur_column->keywords->v.keyword) {
				/* We removed the first element in the keyword list. Update column keyword list head pointer */
				cur_column->keywords->v.keyword = next;
			}
		}
		cur_column->column_number = column_number;
		cur_column = cur_column->next;
	} while (cur_column != start_column);
}
