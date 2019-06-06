/* Copyright (C) 2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
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
	output_key = (SqlKey*)octo_cmalloc(memory_chunks, sizeof(SqlKey));
	memset(output_key, 0, sizeof(SqlKey));
	output_key->table = table;
	output_key->column = column;
	output_key->unique_id = unique_id;
	output_key->is_cross_reference_key = TRUE;

	MALLOC_LP(root, LP_INSERT);
	project = MALLOC_LP(root->v.operand[0], LP_PROJECT);
	output = MALLOC_LP(root->v.operand[1], LP_OUTPUT);
	column_list = MALLOC_LP(project->v.operand[0], LP_COLUMN_LIST);
	select = MALLOC_LP(project->v.operand[1], LP_SELECT);

	// Setup the table alias that will be shared for all columns
	SQL_STATEMENT(table_alias_statement, table_alias_STATEMENT);
	MALLOC_STATEMENT(table_alias_statement, table_alias, SqlTableAlias);
	UNPACK_SQL_STATEMENT(table_alias, table_alias_statement, table_alias);
	/// TODO: copy table so we can more easily clean? Fill out other fields?
	PACK_SQL_STATEMENT(table_alias->table, table, table);
	table_alias->unique_id = get_plan_unique_number(plan);
	table_alias->alias = table->tableName;

	// Populate column list with a the column, followed by all the keys for this table
	cur = MALLOC_LP(column_list->v.operand[0], LP_WHERE);
	lp_cla = MALLOC_LP(cur->v.operand[0], LP_COLUMN_ALIAS);
	// This is used to pass information about types to the functions which output the final result
	// to the user; this table should never get there, so leave it out
	//MALLOC_LP(cur->v.operand[1], LP_COLUMN_LIST_ALIAS);
	lp_cla->v.column_alias = (SqlColumnAlias*)octo_cmalloc(memory_chunks, sizeof(SqlColumnAlias));
	memset(lp_cla->v.column_alias, 0, sizeof(SqlColumnAlias));
	cla = lp_cla->v.column_alias;
	/// TODO: copy column so we can more easily clean things up?
	PACK_SQL_STATEMENT(cla->column, column, column);
	cla->table_alias = table_alias_statement;
	// Get a list of key columns
	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn*));
	max_key = get_key_columns(table, key_columns);
	for(cur_key = 0; cur_key <= max_key; cur_key++) {
		MALLOC_LP(column_list->v.operand[1], LP_COLUMN_LIST);
		column_list = column_list->v.operand[1];
		cur = MALLOC_LP(column_list->v.operand[0], LP_WHERE);
		lp_cla = MALLOC_LP(cur->v.operand[0], LP_COLUMN_ALIAS);
		// This is used to pass information about types to the functions which output the final result
		// to the user; this table should never get there, so leave it out
		//MALLOC_LP(cur->v.operand[1], LP_COLUMN_LIST_ALIAS);
		lp_cla->v.column_alias = (SqlColumnAlias*)octo_cmalloc(memory_chunks, sizeof(SqlColumnAlias));
		memset(lp_cla->v.column_alias, 0, sizeof(SqlColumnAlias));
		cla = lp_cla->v.column_alias;
		/// TODO: copy column so we can more easily clean things up?
		PACK_SQL_STATEMENT(cla->column, key_columns[cur_key], column);
		cla->table_alias = table_alias_statement;
	}

	// Generate a normal table join for SELECT, then populate CRITERIA and KEYS
	table_join = MALLOC_LP(select->v.operand[0], LP_TABLE_JOIN);
	criteria = MALLOC_LP(select->v.operand[1], LP_CRITERIA);
	lp_table = MALLOC_LP(table_join->v.operand[0], LP_TABLE);
	lp_table->v.table_alias = table_alias;
	MALLOC_LP(criteria->v.operand[0], LP_KEYS);
	select_options = MALLOC_LP(criteria->v.operand[1], LP_SELECT_OPTIONS);
	MALLOC_LP(select_options->v.operand[0], LP_WHERE);
	lp_keywords = MALLOC_LP(select_options->v.operand[1], LP_KEYWORDS);
	// Insert a keyword indicating that we are building an index
	lp_keywords->v.keywords = (SqlOptionalKeyword*)octo_cmalloc(memory_chunks, sizeof(SqlOptionalKeyword));
	memset(lp_keywords->v.keywords, 0, sizeof(SqlOptionalKeyword));
	keywords = lp_keywords->v.keywords;
	dqinit(keywords);
	keywords->keyword = OPTIONAL_POPULATE_INDEX;

	// Select an LP_KEY to output things to that is correct
	lp_output_key = MALLOC_LP(output->v.operand[0], LP_KEY);
	lp_output_key->v.key = output_key;

	// Optimize this new plan
	optimize_logical_plan(root);
	return root;
}
