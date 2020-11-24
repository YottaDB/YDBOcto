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
		SqlColumnList *start_cl, *cur_cl;

		/* The validation of query_expression columns with column_name_list colums is deferred to qualify_query() to account
		 * for asterisk expansion. Reasoning explained below.
		 *
		 * Consider the following example query:
		 * 	`INSERT INTO names(id,firstname,lastname) SELECT * FROM names;`
		 * Here the column validation (comparison between insert into table columns given by column_name_list parameter and
		 * query_expression select column list) would fail as asterisk is not yet expanded before the call to
		 * insert_statement().
		 *
		 * Before the asterisk deferral change it would have been in query_specification() call, so when the execution
		 * reaches insert_statement() the query would have been logically similar to the following:
		 * 	`INSERT INTO names(id,firstname,lastname) SELECT (id),(firstname),(lastname) FROM names;`
		 * but since asterisk processing is deferred we will need to defer this check as well.
		 *
		 * Validate ERR_TABLE_UNKNOWN_COLUMN_NAME and ERR_DUPLICATE_COLUMN here itself because:
		 * ERR_TABLE_UNKNOWN_COLUMN_NAME -
		 * Relies on table_name (first parameter) which is obtained by column_name grammar as a result it
		 * can only be a simple value_STATEMENT representing the table name. So the table corresponding to this table_name
		 * will already be qualified. Hence no need to worry about asterisk here. Also the column_name_list (second
		 * parameter) which is being validate here will not have any asterisk usage. So no need to defer for now.
		 * ERR_DUPLICATE_COLUMN -
		 * Just validates that duplicate column name is not used in the column_name_list. Since asterisk cannot be
		 * used in column_name_list and here its just a comparison within the list itself no need to defer.
		 */
		UNPACK_SQL_STATEMENT(start_cl, column_name_list, column_list);
		cur_cl = start_cl;
		do {
			SqlColumn *    tbl_col;
			SqlValue *     col_name;
			SqlColumnList *cur_cl2;

			UNPACK_SQL_STATEMENT(col_name, cur_cl->value, value);
			tbl_col = find_column(col_name->v.string_literal, table);
			/* If user specified a hidden key column name, treat it as if the column name was not found.
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
	return validated_query_expression;
}
