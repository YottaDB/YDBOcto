/****************************************************************
 *								*
 * Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	*
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

/* Function invoked by the rule named "update_statement_searched" in src/parser/update.y
 * Returns
 *	non-NULL in case of success
 *	NULL     in case of error
 */
SqlStatement *update_statement(SqlStatement *table_name, SqlStatement *alias_name, SqlStatement *set_clause_list,
			       SqlStatement *where_clause, int *plan_id, ParseContext *parse_context) {
	SqlStatement * join_stmt;
	SqlJoin *      join;
	SqlTableAlias *table_alias;
	SqlTable *     table;

	assert(value_STATEMENT == table_name->type);
	join_stmt = table_reference(table_name, NULL, plan_id);
	if (NULL == join_stmt) {
		return NULL;
	}
	UNPACK_SQL_STATEMENT(join, join_stmt, join);
	UNPACK_SQL_STATEMENT(table_alias, join->value, table_alias);
	/* Untill UPDATE is allowed with views (YDBOcto#924) generate an error if
	 * the table_name corresponds to a view.
	 */
	IF_VIEW_ISSUE_UNSUPPORTED_OPERATION_ERROR(table_alias->table, update_STATEMENT);
	UNPACK_SQL_STATEMENT(table, table_alias->table, create_table);
	if (!table->readwrite) {
		ERROR(ERR_TABLE_READONLY, "UPDATE", table_name->v.value->v.reference);
		return NULL;
	}
	table_alias->alias = ((NULL != alias_name) ? alias_name : table->tableName);

	/* Do a few error checks here on the columns specified in the SET clause.
	 * More error checks are done later in "qualify_query.c" and "populate_data_type.c".
	 * Note: The error checks done here are very similar to that in "src/parser/insert_statement.c".
	 *       But the code is different enough that it is not easily possible to avoid duplication.
	 * Note: "set_clause_list" is actually a "SqlUpdateColumnValue *" pointer and not "SqlStatement *"
	 *       pointer so do the needed type cast as part of assigning.
	 */
	SqlUpdateColumnValue *ucv, *ucv_head;
	boolean_t	      is_default_column;

	ucv_head = (SqlUpdateColumnValue *)set_clause_list;
	ucv = ucv_head;
	do {
		SqlColumn *tbl_col;
		SqlValue * col_name;

		UNPACK_SQL_STATEMENT(col_name, ucv->col_name, value);
		tbl_col = find_column(col_name->v.string_literal, table);
		/* Note: If user specified a hidden key column name, we should treat it as if the column name was not found.
		 * This is because the user is not supposed to specify explicit values for hidden column names.
		 */
		if ((NULL == tbl_col) || tbl_col->is_hidden_keycol) {
			SqlValue *tbl_name;

			UNPACK_SQL_STATEMENT(tbl_name, table->tableName, value);
			ERROR(ERR_TABLE_UNKNOWN_COLUMN_NAME, col_name->v.string_literal, tbl_name->v.string_literal);
			yyerror(NULL, NULL, &ucv->col_name, NULL, NULL, NULL);
			return NULL;
		}

		is_default_column = FALSE;
		if (keyword_STATEMENT == ucv->col_value->type) {
			assert(OPTIONAL_DEFAULT == ucv->col_value->v.keyword->keyword);
			is_default_column = TRUE;
			if (!IS_COLUMN_IDENTITY(tbl_col)) {
				// Issue error as till YDBOcto#555 is implemented DEFAULT value for a column is only allowed
				// for identity columns.
				ERROR(ERR_FEATURE_NOT_IMPLEMENTED,
				      "DEFAULT value for a column is only allowed for IDENTITY columns");
				yyerror(NULL, NULL, &ucv->col_name, NULL, NULL, NULL);
				return NULL;
			}
		}
		/* Check if the column being modified is a GENERATED ALWAYS AS IDENTITY. In this case update can only be done if
		 * DEFAULT is the value (YDBOcto#555). Till that is implemented, issue an error.
		 */
		if (IS_COLUMN_ALWAYS_IDENTITY(tbl_col)) {
			if (is_default_column) {
				// Column value can be set to DEFAULT when the column is defined as an IDENTITY
			} else {
				ERROR(ERR_UPDATE_OF_GENERATED_ALWAYS_IDENTITY, col_name->v.string_literal);
				yyerror(NULL, NULL, &ucv->col_name, NULL, NULL, NULL);
				return NULL;
			}
		}
		/* Update "col_name" to point to "SqlColumn" structure instead of "SqlValue" (having just the column name). */
		ucv->col_name->type = column_STATEMENT;
		ucv->col_name->v.column = tbl_col;
		ucv = ucv->next;

		/* Check if duplicate column names are specified. If so, issue error. */
		SqlUpdateColumnValue *ucv2;

		for (ucv2 = ucv; ucv2 != ucv_head; ucv2 = ucv2->next) {
			SqlValue *col_name2;

			UNPACK_SQL_STATEMENT(col_name2, ucv2->col_name, value);
			if (!strcmp(col_name->v.string_literal, col_name2->v.string_literal)) {
				ERROR(ERR_DUPLICATE_COLUMN, col_name->v.string_literal);
				yyerror(NULL, NULL, &ucv2->col_name, NULL, NULL, NULL);
				return NULL;
			}
		}
	} while (ucv != ucv_head);

	SqlUpdateStatement *update;
	SqlStatement *	    ret, *validated_query_expression;

	SQL_STATEMENT(ret, update_STATEMENT);
	MALLOC_STATEMENT(ret, update, SqlUpdateStatement);
	UNPACK_SQL_STATEMENT(update, ret, update);
	update->src_join = join_stmt;

	update->col_value_list = ucv_head;
	update->where_clause = where_clause;
	validated_query_expression = validate_query_expression(ret, parse_context, update_STATEMENT);
	return validated_query_expression;
}
