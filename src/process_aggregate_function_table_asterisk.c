/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
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

/*
 * Called by aggregate_function_STATEMENT case in qualify_statement() to expand table.* in aggregate functions.
 * SqlColumnAlias for TABLE.ASTERISK node is already qualified with the correct table_alias_stmt. We just iterate through its
 * column_list to retrieve all the columns required by table.*.
 * The input parameter "af" is used as follows.
 * 1) SqlColumnList given by "af->parameter" is updated to have SqlColumnAlias nodes for all columns of table.
 * 2) The aggregate function type information is obtained from "af->type". Used to determine if expansion is required.
 */
void process_aggregate_function_table_asterisk(SqlAggregateFunction *af) {

	SqlAggregateType type;
	type = af->type;
	if ((AGGREGATE_COUNT_ASTERISK == type) || (AGGREGATE_COUNT == type)) {
		if (AGGREGATE_COUNT == type) {
			/* COUNT(t1.*) behaves differently from COUNT(t1.col) in terms of NULL value counting
			 * even if t1 has only one column. See YDBOcto #759 for details. Therefore, need to distinguish the two.
			 * Hence the type change below. COUNT(t1.col) will continue to have AGGREGATE_COUNT as the type.
			 * COUNT(t1.*) will have type AGGREGATE_COUNT_TABLE_ASTERISK.
			 */
			af->type = AGGREGATE_COUNT_TABLE_ASTERISK;
		}
		return;
	}

	SqlStatement *specification_list;
	specification_list = af->parameter;
	assert(NULL != specification_list);

	/* Replace TABLENAME.ASTERISK node in specification_list with column list corresponding to TABLENAME table. */
	SqlColumnAlias *column_alias;
	SqlColumnList * cl_new;
	SqlStatement *	table_alias_stmt;
	SqlTableAlias * table_alias;

	GET_TABLE_ASTERISK_COLUMN_ALIAS_FROM_COLUMN_LIST(column_alias, specification_list);
	cl_new = NULL;
	table_alias_stmt = column_alias->table_alias_stmt;
	assert(NULL != table_alias_stmt);
	UNPACK_SQL_STATEMENT(table_alias, table_alias_stmt, table_alias);
	if (NULL != table_alias->column_list) {
		SqlColumnListAlias *inner_cur_cla, *inner_start_cla;
		SqlStatement *	    column_alias_stmt;
		SqlColumnAlias *    column_alias;
		SqlColumnList *	    cl;

		UNPACK_SQL_STATEMENT(inner_start_cla, table_alias->column_list, column_list_alias);
		inner_cur_cla = inner_start_cla;
		if (AGGREGATE_COUNT_DISTINCT != type) {
			/* For AVG, SUM, MIN & MAX aggregate functions having a TABLENAME.ASTERISK is NOT valid even if it
			 * only has a single column. It is treated as a "record" type by Postgres and so Octo follows the same
			 * model and issues an error here. Return table.* as is and let populate_data_type() issue an error.
			 */
			return;
		} else {
			/* COUNT(DISTINCT t1.*) behaves differently from COUNT(DISTINCT t1.col) in terms of NULL value counting
			 * even if t1 has only one column. See YDBOcto #759 for details. Therefore, need to distinguish the two.
			 * Hence the type change below. COUNT(DISTINCT t1.col) will continue to have AGGREGATE_COUNT_DISTINCT
			 * as the type. COUNT(DISTINCT t1.*) will have type AGGREGATE_COUNT_DISTINCT_TABLE_ASTERISK.
			 */
			af->type = AGGREGATE_COUNT_DISTINCT_TABLE_ASTERISK;
		}
		do {
			OCTO_CMALLOC_STRUCT(cl, SqlColumnList);
			dqinit(cl);
			column_alias = get_column_alias_for_column_list_alias(inner_cur_cla, table_alias_stmt);
			PACK_SQL_STATEMENT(column_alias_stmt, column_alias, column_alias);
			cl->value = column_alias_stmt;
			if (NULL == cl_new)
				cl_new = cl;
			else
				dqappend(cl_new, cl);

			inner_cur_cla = inner_cur_cla->next;
		} while (inner_cur_cla != inner_start_cla);
		specification_list->v.column_list = cl_new;
	}
}
