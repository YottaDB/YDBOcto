/* Copyright (C) 2018 YottaDB, LLC
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
#include <string.h>

#include <libyottadb.h>

#include "octo.h"
#include "template_strings.h"
#include "logical_plan.h"
#include "physical_plan.h"

PhysicalPlan *emit_insert_statement(ydb_buffer_t *cursor_global,
                                ydb_buffer_t *cursor_exe_global, struct SqlStatement *stmt)
{
	SqlInsertStatement *insert;
	SqlSelectStatement *select;
	SqlTable *table;
	SqlTableAlias *table_alias;
	LogicalPlan *plan, *table_wrapper;
	PhysicalPlan *pplan;
	char buffer[MAX_STR_CONST];

	TRACE(ERR_ENTERING_FUNCTION, "emit_insert_statement");

	UNPACK_SQL_STATEMENT(insert, stmt, insert);

	// The same as select, except we override the logical plan output
	UNPACK_SQL_STATEMENT(select, insert->source, select);
	table = insert->destination;
	plan = generate_logical_plan(insert->source, &config->plan_id);
	table_alias = (SqlTableAlias*)malloc(sizeof(SqlTableAlias));
	memset(table_alias, 0, sizeof(SqlTableAlias));
	PACK_SQL_STATEMENT(table_alias->table, table, table);
	table_alias->alias = copy_sql_statement(table->tableName);
	// unique_id doesn't matter for this
	MALLOC_LP(table_wrapper, LP_TABLE);
	table_wrapper->v.table_alias = table_alias;
	plan->v.operand[1] = table_wrapper;
	plan = optimize_logical_plan(plan);
	lp_emit_plan(buffer, MAX_STR_CONST, plan);
	printf("%s\n", buffer);
	pplan = generate_physical_plan(plan, NULL);
	emit_physical_plan(pplan);
	return pplan;
}
