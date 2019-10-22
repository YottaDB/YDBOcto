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

#include "octo.h"
#include "octo_types.h"
#include "logical_plan.h"

LogicalPlan *lp_generate_xref_plan(LogicalPlan *plan, SqlTable *table, SqlColumn *column, int unique_id) {
	LogicalPlan *root, *project, *output, *column_list, *select, *cur, *lp_cla;
	LogicalPlan *table_join, *criteria, *lp_output_key, *lp_table, *select_options, *lp_keywords;
	SqlColumnAlias *cla;
	SqlTableAlias *table_alias;
	SqlStatement *table_alias_statement;
	SqlColumn *key_columns[MAX_KEY_COUNT];
	SqlOptionalKeyword *keywords;
	SqlKey *output_key;
	int cur_key, max_key;

	// Setup the output key
	OCTO_CMALLOC_STRUCT(output_key, SqlKey);
	memset(output_key, 0, sizeof(SqlKey));
	output_key->table = table;
	output_key->column = column;
	output_key->unique_id = unique_id;
	output_key->is_cross_reference_key = TRUE;

	MALLOC_LP_2ARGS(root, LP_INSERT);
	root->counter = plan->counter;
	MALLOC_LP(project, root->v.operand[0], LP_PROJECT);
	MALLOC_LP(output, root->v.operand[1], LP_OUTPUT);
	MALLOC_LP(column_list, project->v.operand[0], LP_COLUMN_LIST);
	MALLOC_LP(select, project->v.operand[1], LP_SELECT);

	// Setup the table alias that will be shared for all columns
	SQL_STATEMENT(table_alias_statement, table_alias_STATEMENT);
	MALLOC_STATEMENT(table_alias_statement, table_alias, SqlTableAlias);
	UNPACK_SQL_STATEMENT(table_alias, table_alias_statement, table_alias);
	/// TODO: copy table so we can more easily clean? Fill out other fields?
	PACK_SQL_STATEMENT(table_alias->table, table, table);
	table_alias->unique_id = get_plan_unique_number(plan);
	table_alias->alias = table->tableName;

	// Populate column list with a the column, followed by all the keys for this table
	MALLOC_LP(cur, column_list->v.operand[0], LP_WHERE);
	MALLOC_LP(lp_cla, cur->v.operand[0], LP_COLUMN_ALIAS);
	// This is used to pass information about types to the functions which output the final result
	// to the user; this table should never get there, so leave it out
	//MALLOC_LP_2ARGS(cur->v.operand[1], LP_COLUMN_LIST_ALIAS);
	OCTO_CMALLOC_STRUCT(lp_cla->v.column_alias, SqlColumnAlias);
	memset(lp_cla->v.column_alias, 0, sizeof(SqlColumnAlias));
	cla = lp_cla->v.column_alias;
	/// TODO: copy column so we can more easily clean things up?
	PACK_SQL_STATEMENT(cla->column, column, column);
	cla->table_alias = table_alias_statement;
	// Get a list of key columns
	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn*));
	max_key = get_key_columns(table, key_columns);
	for(cur_key = 0; cur_key <= max_key; cur_key++) {
		MALLOC_LP_2ARGS(column_list->v.operand[1], LP_COLUMN_LIST);
		column_list = column_list->v.operand[1];
		MALLOC_LP(cur, column_list->v.operand[0], LP_WHERE);
		MALLOC_LP(lp_cla, cur->v.operand[0], LP_COLUMN_ALIAS);
		// This is used to pass information about types to the functions which output the final result
		// to the user; this table should never get there, so leave it out
		//MALLOC_LP_2ARGS(cur->v.operand[1], LP_COLUMN_LIST_ALIAS);
		OCTO_CMALLOC_STRUCT(lp_cla->v.column_alias, SqlColumnAlias);
		memset(lp_cla->v.column_alias, 0, sizeof(SqlColumnAlias));
		cla = lp_cla->v.column_alias;
		/// TODO: copy column so we can more easily clean things up?
		PACK_SQL_STATEMENT(cla->column, key_columns[cur_key], column);
		cla->table_alias = table_alias_statement;
	}

	// Generate a normal table join for SELECT, then populate CRITERIA and KEYS
	MALLOC_LP(table_join, select->v.operand[0], LP_TABLE_JOIN);
	MALLOC_LP(criteria, select->v.operand[1], LP_CRITERIA);
	MALLOC_LP(lp_table, table_join->v.operand[0], LP_TABLE);
	lp_table->v.table_alias = table_alias;
	MALLOC_LP_2ARGS(criteria->v.operand[0], LP_KEYS);
	MALLOC_LP(select_options, criteria->v.operand[1], LP_SELECT_OPTIONS);
	MALLOC_LP_2ARGS(select_options->v.operand[0], LP_WHERE);
	MALLOC_LP(lp_keywords, select_options->v.operand[1], LP_KEYWORDS);
	// Insert a keyword indicating that we are building an index
	OCTO_CMALLOC_STRUCT(lp_keywords->v.keywords, SqlOptionalKeyword);
	memset(lp_keywords->v.keywords, 0, sizeof(SqlOptionalKeyword));
	keywords = lp_keywords->v.keywords;
	dqinit(keywords);
	keywords->keyword = OPTIONAL_XREF_INDEX;

	// Select an LP_KEY to output things to that is correct
	MALLOC_LP(lp_output_key, output->v.operand[0], LP_KEY);
	lp_output_key->v.key = output_key;

	// Optimize this new plan
	return optimize_logical_plan(root);
}
