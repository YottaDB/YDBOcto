/****************************************************************
 *								*
 * Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	*
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

#define ERROR_IF_COLUMN_IS_IDENTITY_AND_NO_OVERRIDE_SPECIFIED(TBL_COL, OPTIONAL_WORDS, TABLE, COL_NAME, LOC)        \
	{                                                                                                           \
		if (IS_COLUMN_ALWAYS_IDENTITY(TBL_COL)) {                                                           \
			if ((NULL == OPTIONAL_WORDS)                                                                \
			    || (OPTIONAL_OVERRIDING_SYSTEM_VALUE != OPTIONAL_WORDS->v.keyword->keyword)) {          \
				/* Issue an error as inserting into a GENERATED_ALWAYS_IDENTITY column is invalid*/ \
				ERROR(ERR_INSERT_ON_GENERATED_ALWAYS_IDENTITY, COL_NAME->v.string_literal);         \
				if (NULL != LOC) {                                                                  \
					yyerror(NULL, NULL, &LOC, NULL, NULL, NULL);                                \
				}                                                                                   \
				return NULL;                                                                        \
			}                                                                                           \
		}                                                                                                   \
	}

/* Function invoked by the rule named "insert_statement" in src/parser/insert.y
 * Returns
 *	non-NULL in case of success
 *	NULL     in case of error
 */
SqlStatement *insert_statement(SqlStatement *table_name, SqlStatement *column_name_list, SqlStatement *optional_words,
			       SqlStatement *query_expression, int *plan_id, ParseContext *parse_context) {
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
			ERROR_IF_COLUMN_IS_IDENTITY_AND_NO_OVERRIDE_SPECIFIED(tbl_col, optional_words, table, col_name,
									      cur_cl->value);
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
	insert->optional_words = optional_words;
	if (NULL == column_name_list) {
		SqlStatement *src_table_alias_stmt = drill_to_table_alias(query_expression);

		SqlTableAlias *src_table_alias;
		UNPACK_SQL_STATEMENT(src_table_alias, src_table_alias_stmt, table_alias);

		SqlColumnListAlias *start_cla[2], *cur_cla[2];
		UNPACK_SQL_STATEMENT(start_cla[0], src_table_alias->column_list, column_list_alias); // source table columns
		UNPACK_SQL_STATEMENT(start_cla[1], table_alias->column_list, column_list_alias);     // destination table columns
		cur_cla[0] = start_cla[0];
		cur_cla[1] = start_cla[1];

		/* Check if a GENERATED ALWAYS IDENTITY is being inserted to. If so issue an error.
		 * Following loop is advanced using the source table column list as we want to only check those columns in
		 * destination which are being explicitely assigned a value in the source table.
		 */
		do {
			SqlColumnList *cur_dst_cl;
			UNPACK_SQL_STATEMENT(cur_dst_cl, cur_cla[1]->column_list, column_list);

			SqlColumnAlias *cur_dst_ca;
			UNPACK_SQL_STATEMENT(cur_dst_ca, cur_dst_cl->value, column_alias);

			SqlColumn *cur_dst_column;
			UNPACK_SQL_STATEMENT(cur_dst_column, cur_dst_ca->column, column);

			SqlStatement *loc_stmt = NULL; // dummy variable to allow the following macro usage when LOC is NULL
			ERROR_IF_COLUMN_IS_IDENTITY_AND_NO_OVERRIDE_SPECIFIED(cur_dst_column, optional_words, table,
									      cur_dst_column->columnName->v.value, loc_stmt);
			cur_cla[0] = cur_cla[0]->next;
			cur_cla[1] = cur_cla[1]->next;
			if ((cur_cla[0] != start_cla[0]) && (cur_cla[1] == start_cla[1])) {
				/* Source table has more columns than the destination table.
				 * Other checks for insert_STATEMENT will validate these errors no need to worry about this here.
				 */
				break;
			}
		} while (cur_cla[0] != start_cla[0]);
	}
	validated_query_expression = validate_query_expression(ret, parse_context, insert_STATEMENT);
	return validated_query_expression;
}
