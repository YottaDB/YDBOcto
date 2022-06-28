/****************************************************************
 *								*
 * Copyright (c) 2020-2022 YottaDB LLC and/or its subsidiaries.	*
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

/* Function invoked by the rule named "insert_statement" in src/parser/insert.y
 * Returns
 *	non-NULL in case of success
 *	NULL     in case of error
 */
SqlStatement *insert_statement(SqlStatement *table_name, SqlStatement *column_name_list, SqlStatement *query_expression,
			       int *plan_id, ParseContext *parse_context) {
	SqlStatement *	    ret, *validated_query_expression;
	SqlStatement *	    join_stmt;
	SqlInsertStatement *insert;
	SqlJoin *	    join;
	SqlTableAlias *	    table_alias;
	SqlTable *	    table;

	assert(value_STATEMENT == table_name->type);
	join_stmt = table_reference(table_name, NULL, plan_id);
	if (NULL == join_stmt) {
		return NULL;
	}
	UNPACK_SQL_STATEMENT(join, join_stmt, join);
	UNPACK_SQL_STATEMENT(table_alias, join->value, table_alias);
	UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
	if (!table->readwrite) {
		ERROR(ERR_TABLE_READONLY, "INSERT", table_name->v.value->v.reference);
		return NULL;
	}
	if (NULL != column_name_list) {
		/* Do a few error checks here. More error checks are done later in "qualify_query.c" and "populate_data_type.c" */
		SqlColumnList *start_cl, *cur_cl;

		UNPACK_SQL_STATEMENT(start_cl, column_name_list, column_list);
		cur_cl = start_cl;
		do {
			SqlColumn *    tbl_col;
			SqlValue *     col_name;
			SqlColumnList *cur_cl2;

			UNPACK_SQL_STATEMENT(col_name, cur_cl->value, value);
			tbl_col = find_column(col_name->v.string_literal, table);
			/* Note: If user specified a hidden key column name, we should treat it as if the column name was not found.
			 * This is because the user is not supposed to specify explicit values for hidden column names.
			 */
			if ((NULL == tbl_col) || tbl_col->is_hidden_keycol) {
				SqlValue *tbl_name;

				UNPACK_SQL_STATEMENT(tbl_name, table->tableName, value);
				ERROR(ERR_TABLE_UNKNOWN_COLUMN_NAME, col_name->v.string_literal, tbl_name->v.string_literal);
				yyerror(NULL, NULL, &cur_cl->value, NULL, NULL, NULL);
				return NULL;
			}
			/* Update cur_cl to point to "SqlColumn" structure instead of "SqlValue" (having just the column name) */
			cur_cl->value->type = column_STATEMENT;
			cur_cl->value->v.column = tbl_col;
			cur_cl = cur_cl->next;
			/* Check if duplicate column names are specified. If so, issue error. */
			for (cur_cl2 = cur_cl; start_cl != cur_cl2; cur_cl2 = cur_cl2->next) {
				SqlValue *col_name2;

				UNPACK_SQL_STATEMENT(col_name2, cur_cl2->value, value);
				if (!strcmp(col_name->v.string_literal, col_name2->v.string_literal)) {
					ERROR(ERR_DUPLICATE_COLUMN, col_name->v.string_literal);
					yyerror(NULL, NULL, &cur_cl2->value, NULL, NULL, NULL);
					return NULL;
				}
			}
		} while (cur_cl != start_cl);
	}
	SQL_STATEMENT(ret, insert_STATEMENT);
	MALLOC_STATEMENT(ret, insert, SqlInsertStatement);
	UNPACK_SQL_STATEMENT(insert, ret, insert);
	/* All we want here is "table_alias->table" but we need "table_alias" in order to later store this as a LP_TABLE
	 * logical plan. Hence the call to "table_reference()" above. Otherwise a call to "find_table()" would have sufficed.
	 */
	insert->dst_table_alias_stmt = join->value;
	insert->columns = column_name_list;
	insert->src_table_alias_stmt = query_expression;
	validated_query_expression = validate_query_expression(ret, parse_context, insert_STATEMENT);
	return validated_query_expression;
}
