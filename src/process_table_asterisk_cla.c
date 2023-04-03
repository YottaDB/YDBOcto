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
#include <string.h>

/* This function is invoked by "qualify_statement.c" for processing TABLENAME.ASTERISK usage.
 *
 * TABLENAME.ASTERISK usages are processed as follows (YDBOcto#759).
 * 1) In WHERE, GROUP BY and HAVING no expansion.
 * 2) In SELECT column list, is replaced with the list of columns making up the table.
 * 3) In ORDER BY clause (when no GROUP BY clause), is left untouched but an expanded list of columns making up the table is
 *    appended. The untouched table.* column list alias helps with later ORDER BY processing in ordering ROW(NULL) ahead of
 *    Composite NULL (see https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/759#note_736104049 for more details).
 *    This ORDER BY processing happens in "tmpl_column_list_combine.ctemplate" (see "is_order_by_table_asterisk" variable).
 * 4) In ORDER BY clause (when GROUP BY clause exists) no expansion as grouped value will be used by physical plan.
 *
 * Input parameters
 * ----------------
 * a) "c_cla_cur" points to current column list alias element. If this is replaced (SELECT column list case), then other parameters
 *    "specification_list" and "c_cla_head" are also updated in case the replacement creates a new head of the list.
 * b) "qualify_query_stage" : Indicates what part of the query we are currently qualifying (SELECT column list, ORDER BY etc.).
 *
 * SqlColumnAlias for TABLENAME.ASTERISK node is already qualified with the correct "table_alias_stmt".
 * We just iterate through its column_list to retrieve all the columns required by table.
 */
void process_table_asterisk_cla(SqlStatement *specification_list, SqlColumnListAlias **c_cla_cur, SqlColumnListAlias **c_cla_head,
				QualifyQueryStage qualify_query_stage) {
	SqlColumnListAlias *cla_cur, *cla_head, *result, *cla;
	SqlColumnAlias *    column_alias;
	SqlStatement *	    cur_table_alias_stmt;
	SqlTableAlias *	    cur_table_alias;
	SqlColumnList *	    cl_cur;

	switch (qualify_query_stage) {
	case QualifyQuery_ORDER_BY:
	case QualifyQuery_SELECT_COLUMN_LIST:
		/* These cases need additional processing (expansion and/or replacement of TABLENAME.ASTERISK usage) */
		break;
	default:
		/* These cases need no more processing. Keep the TABLENAME.ASTERISK usage intact. */
		return;
	}
	cla_cur = *c_cla_cur;
	cla_head = *c_cla_head;
	assert(NULL != cla_cur);
	result = NULL;
	UNPACK_SQL_STATEMENT(cl_cur, cla_cur->column_list, column_list);
	UNPACK_SQL_STATEMENT(column_alias, cl_cur->value, column_alias);
	assert(is_stmt_table_asterisk(column_alias->column)); /* caller should have ensured this */
	cur_table_alias_stmt = column_alias->table_alias_stmt;
	assert(NULL != cur_table_alias_stmt);
	UNPACK_SQL_STATEMENT(cur_table_alias, cur_table_alias_stmt, table_alias);
	/* Update the list with new nodes */
	UNPACK_SQL_STATEMENT(cla, cur_table_alias->column_list, column_list_alias);
	if (QualifyQuery_SELECT_COLUMN_LIST == qualify_query_stage) {
		SqlColumnListAlias *tmp;
		result = copy_column_list_alias_list(cla, cur_table_alias_stmt, cla_cur->keywords);
		tmp = result->prev;
		/* Replace TABLENAME.ASTERISK cla with a linked list of clas corresponding to columns of table */
		REPLACE_COLUMNLISTALIAS(cla_cur, result, cla_head, specification_list);
		*c_cla_cur = tmp;
		*c_cla_head = cla_head;
	} else {
		assert(QualifyQuery_ORDER_BY == qualify_query_stage);
		assert(NULL != cur_table_alias->parent_table_alias);
		assert(0 == cur_table_alias->parent_table_alias->v.table_alias->aggregate_function_or_group_by_or_having_specified);
		result = copy_column_list_alias_list(cla, cur_table_alias_stmt, cla_cur->keywords);
		/* Append linked list of clas corresponding to columns of table after untouched TABLENAME.ASTERISK
		 * cla. Do not update c_cla_cur as in case SELECT DISTINCT is used we want to match ORDER BY column
		 * with SELECT column.
		 */
		cla_cur = cla_cur->next;
		dqappend(cla_cur, result);
	}
}
