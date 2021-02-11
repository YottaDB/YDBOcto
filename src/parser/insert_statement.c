/****************************************************************
 *								*
 * Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	*
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
		ERROR(ERR_TABLE_READONLY, "INSERT");
		return NULL;
	}
	if (NULL != column_name_list) {
		SqlColumnList *	    start_cl, *cur_cl;
		SqlTableAlias *	    src_table_alias;
		SqlColumnListAlias *start_cla, *cur_cla;
		SqlStatement *	    table_alias_stmt;

		table_alias_stmt = drill_to_table_alias(query_expression);
		UNPACK_SQL_STATEMENT(src_table_alias, table_alias_stmt, table_alias);
		UNPACK_SQL_STATEMENT(start_cla, src_table_alias->column_list, column_list_alias);
		cur_cla = start_cla;
		/* Check if column names specified are valid columns in the source table. If not issue error. */
		UNPACK_SQL_STATEMENT(start_cl, column_name_list, column_list);
		cur_cl = start_cl;
		do {
			SqlColumn *    tbl_col;
			SqlValue *     col_name;
			SqlColumnList *cur_cl2;

			if (NULL == cur_cla) {
				ERROR(ERR_INSERT_TOO_MANY_COLUMNS, NULL);
				yyerror(NULL, NULL, &cur_cl->value, NULL, NULL, NULL);
				return NULL;
			}
			UNPACK_SQL_STATEMENT(col_name, cur_cl->value, value);
			tbl_col = find_column(col_name->v.string_literal, table);
			if (NULL == tbl_col) {
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
			if (NULL != cur_cla) {
				cur_cla = cur_cla->next;
				if (start_cla == cur_cla) {
					cur_cla = NULL;
				}
			}
		} while (cur_cl != start_cl);
		if (NULL != cur_cla) {
			ERROR(ERR_INSERT_TOO_MANY_EXPRESSIONS, NULL);
			yyerror(NULL, NULL, &cur_cla->column_list, NULL, NULL, NULL);
			return NULL;
		}
	}
	/* else: An "ERR_INSERT_TOO_MANY_EXPRESSIONS" error is issued (if needed) later in "check_column_lists_for_type_match.c" */
	SQL_STATEMENT(ret, insert_STATEMENT);
	MALLOC_STATEMENT(ret, insert, SqlInsertStatement);
	UNPACK_SQL_STATEMENT(insert, ret, insert);
	/* All we want here is "table_alias->table" but we need "table_alias" in order to later store this as a LP_TABLE
	 * logical plan. Hence the call to "table_reference()" above. Otherwise a call to "find_table()" would have sufficed.
	 */
	insert->dst_table_alias = table_alias;
	insert->columns = column_name_list;
	insert->src_table_alias_stmt = query_expression;
	validated_query_expression = validate_query_expression(ret, parse_context, insert_STATEMENT);
	/* INSERT INTO currently works for simplistic queries but there is still a lot of functionality to be
	 * implemented before it can be officially called supported. But to avoid one huge MR at the end, we are going
	 * to merge incremental sets of changes but keep the functionality disabled by returning NULL from this function.
	 * Once the full functionality of INSERT INTO has been implemented, the following "if" block of code can be removed.
	 * TODO: YDBOcto#502 : Temporary "if" code block that can be removed once INSERT INTO is fully implemented.
	 */
	if (NULL != validated_query_expression) {
		ERROR(ERR_FEATURE_NOT_IMPLEMENTED, "INSERT INTO");
		return NULL;
	}
	return validated_query_expression;
}
