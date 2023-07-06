/****************************************************************
 *								*
 * Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	*
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

/* This function is responsible for forming a VIEW'S SqlStatement object.
 * Input:
 * 	create_view_stmt - Parse structure for the view with a yet to be qualified view definition
 * Return:
 *	non-NULL SqlStatement pointer having fully qualified view_STATEMENT object on success
 *	NULL on failure
 */
SqlStatement *view_definition(SqlStatement *create_view_stmt, ParseContext *parse_context) {
	assert(create_view_STATEMENT == create_view_stmt->type);

	SqlView *view;
	UNPACK_SQL_STATEMENT(view, create_view_stmt, create_view);

	SqlStatement *viewName = view->viewName;
	if (config->is_auto_upgrade_octo929) {
		SqlValue *value;
		char *	  view_name, uppercase_name[OCTO_MAX_IDENT + 1];

		UNPACK_SQL_STATEMENT(value, viewName, value);
		if (!value->is_double_quoted) {
			char *start, *end, *dst, *dst_end;

			start = value->v.string_literal;
			end = start + strlen(start);
			dst = uppercase_name;
			dst_end = dst + sizeof(uppercase_name);
			TOUPPER(dst, dst_end, start, end);
			view_name = uppercase_name;
		} else {
			view_name = value->v.string_literal;
		}
		fprintf(config->octo929_sqlfile_stream, "DROP VIEW IF EXISTS \"%s\";\n", view_name);
	}
	SqlStatement *column_name_list = view->column_name_list;
	SqlStatement *query_expression = view->src_table_alias_stmt;
	assert(value_STATEMENT == viewName->type);
	SqlStatement *	 src_table_alias_stmt;
	SqlStatementType save_command_tag = parse_context->command_tag;
	// Pass command type to be select as we are processing view definition
	src_table_alias_stmt = validate_query_expression(query_expression, parse_context, select_STATEMENT);
	// Set back the parse_context command tag to what it was before
	parse_context->command_tag = save_command_tag;
	if (NULL == src_table_alias_stmt) {
		// underlying query would have issued an error
		return NULL;
	}
	view->src_table_alias_stmt = src_table_alias_stmt;
	// Process column names
	if (NULL != column_name_list) {
		/* Assign view column name as alias for the underlying view definition columns */
		SqlColumnListAlias *start_cla, *cur_cla;
		SqlStatement *	    table_alias_stmt = drill_to_table_alias(view->src_table_alias_stmt);
		SqlTableAlias *	    table_alias;
		UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
		UNPACK_SQL_STATEMENT(start_cla, table_alias->column_list, column_list_alias);
		cur_cla = start_cla;

		SqlColumnList *start_cl, *cur_cl;
		UNPACK_SQL_STATEMENT(start_cl, column_name_list, column_list);
		cur_cl = start_cl;
		do {
			/* Update alias such that name of the column in underlying select query is treated
			 * to be same as the view column name.
			 */
			cur_cla->alias = cur_cl->value;
			cur_cla->user_specified_alias = TRUE;
			cur_cl = cur_cl->next;
			cur_cla = cur_cla->next;
			if ((start_cla == cur_cla) && (cur_cl != start_cl)) {
				// CREATE VIEW specifies more column names than columns
				ERROR(ERR_VIEW_MORE_COLUMN_NAMES, "")
				yyerror(NULL, NULL, &cur_cl->value, NULL, NULL, NULL);
				return NULL;
			}
		} while (cur_cl != start_cl);
	}
	// Validate that no duplicate column names exist
	SqlStatement *	    table_alias_stmt;
	SqlTableAlias *	    table_alias;
	SqlColumnListAlias *start_cla, *cur_cla, *cur_cla2;
	table_alias_stmt = drill_to_table_alias(view->src_table_alias_stmt);
	UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
	UNPACK_SQL_STATEMENT(start_cla, table_alias->column_list, column_list_alias);
	cur_cla = start_cla;
	do {
		SqlValue *cur_col_name_val;
		UNPACK_SQL_STATEMENT(cur_col_name_val, cur_cla->alias, value);
		cur_cla = cur_cla->next;
		for (cur_cla2 = cur_cla; start_cla != cur_cla2; cur_cla2 = cur_cla2->next) {
			SqlValue *next_col_name_val;
			UNPACK_SQL_STATEMENT(next_col_name_val, cur_cla2->alias, value);
			if (!strcmp(cur_col_name_val->v.string_literal, next_col_name_val->v.string_literal)) {
				ERROR(ERR_DUPLICATE_COLUMN, cur_col_name_val->v.string_literal);
				yyerror(NULL, NULL, &cur_cla2->alias, NULL, NULL, NULL);
				return NULL;
			}
		}
	} while (cur_cla != start_cla);
	if (0 != qualify_view_dependency(view->src_table_alias_stmt, view)) {
		return NULL;
	}
	return create_view_stmt;
}
