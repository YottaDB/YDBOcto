/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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

#define CALL_DECOMPRESS_HELPER(value, out, out_length)                       \
	{                                                                    \
		if (value != NULL) {                                         \
			value = R2A(value);                                  \
			decompress_statement_helper(value, out, out_length); \
		}                                                            \
	}

void *decompress_statement_helper(SqlStatement *stmt, char *out, int out_length);

SqlStatement *decompress_statement(char *buffer, int out_length) {
	/* We should never come here to decompress a binary function/table/view definition while loading octo-seed.sql
	 * as we will be looking at potentially older/incompatible binary definitions and can lead to memory corruption issues.
	 * Hence the below assert.
	 */
	assert(!config->in_auto_load_octo_seed);
	return (SqlStatement *)decompress_statement_helper((SqlStatement *)buffer, buffer, out_length);
}

/*
 * Returns a pointer to a new memory location for stmt within the out buffer
 *
 * If the out buffer is NULL, doesn't copy the statement, but just counts size
 */
void *decompress_statement_helper(SqlStatement *stmt, char *out, int out_length) {
	SqlTable	     *table;
	SqlColumn	     *cur_column, *start_column;
	SqlValue	     *value;
	SqlOptionalKeyword   *start_keyword, *cur_keyword;
	SqlFunction	     *function;
	SqlParameterTypeList *cur_parameter_type_list, *start_parameter_type_list;

	assert(((char *)stmt) < out + out_length);
	if (NULL == stmt) {
		return NULL;
	}
	// In each case below, after storing the pointer of the statement in
	//  a temporary variable we mark the statement as NULL, then check here
	//  to see if the statement has been marked NULL, and if so, skip it
	// This lets us play a bit fast-and-loose with the structures to
	//  avoid extra copies during runtime, and not have issues with double
	//  frees
	if (NULL == stmt->v.value) {
		return NULL;
	}
	if (data_type_struct_STATEMENT == stmt->type) {
		/* Relevant data is the SqlDataTypeStruct member in the `stmt` union member, which is NOT a pointer.
		 * So, do not do R2A conversion and just return as-is. See similar note in compress_statement.c.
		 */
		return stmt;
	}
	stmt->v.value = R2A(stmt->v.value);
	switch (stmt->type) {
	case create_table_STATEMENT:
		UNPACK_SQL_STATEMENT(table, stmt, create_table);
		if (NULL == table->bin_defn_offset) {
			/* This object is already decompressed i.e. its pointers would have already
			 * gone through R2A. No need of again going through CALL_DECOMPRESS_HELPER()
			 */
			break;
		}
		table->bin_defn_offset = NULL;

		CALL_DECOMPRESS_HELPER(table->tableName, out, out_length);
		CALL_DECOMPRESS_HELPER(table->source, out, out_length);
		CALL_DECOMPRESS_HELPER(table->columns, out, out_length);
		CALL_DECOMPRESS_HELPER(table->delim, out, out_length);
		CALL_DECOMPRESS_HELPER(table->aim_type, out, out_length);
		/* table->readwrite is not a pointer value so no need to call CALL_DECOMPRESS_HELPER on this member */
		/* table->oid is not a pointer value so no need to call CALL_DECOMPRESS_HELPER on this member */
		/* table->if_not_exists_specified is not a pointer value */
		break;
	case create_view_STATEMENT:;
		SqlView *view;
		UNPACK_SQL_STATEMENT(view, stmt, create_view);
		CALL_DECOMPRESS_HELPER(view->viewName, out, out_length);
		CALL_DECOMPRESS_HELPER(view->src_table_alias_stmt, out, out_length);
		break;
	case table_alias_STATEMENT:;
		SqlTableAlias *table_alias;
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		if (NULL == table_alias->bin_defn_offset) {
			/* This object is already decompressed i.e. its pointers would have already
			 * gone through R2A. No need of again going through CALL_DECOMPRESS_HELPER()
			 */
			break;
		}
		table_alias->bin_defn_offset = NULL;
		CALL_DECOMPRESS_HELPER(table_alias->table, out, out_length);
		CALL_DECOMPRESS_HELPER(table_alias->alias, out, out_length);
		CALL_DECOMPRESS_HELPER(table_alias->parent_table_alias, out, out_length);
		CALL_DECOMPRESS_HELPER(table_alias->column_list, out, out_length);
		CALL_DECOMPRESS_HELPER(table_alias->correlation_specification, out, out_length);
		CALL_DECOMPRESS_HELPER(table_alias->table_asterisk_column_alias, out, out_length);
		table_alias->unique_id = config->plan_id++;
		break;
	case select_STATEMENT:;
		SqlSelectStatement *select;
		UNPACK_SQL_STATEMENT(select, stmt, select);
		CALL_DECOMPRESS_HELPER(select->table_list, out, out_length);
		CALL_DECOMPRESS_HELPER(select->where_expression, out, out_length);
		CALL_DECOMPRESS_HELPER(select->select_list, out, out_length);
		CALL_DECOMPRESS_HELPER(select->group_by_expression, out, out_length);
		CALL_DECOMPRESS_HELPER(select->having_expression, out, out_length);
		CALL_DECOMPRESS_HELPER(select->order_by_expression, out, out_length);
		CALL_DECOMPRESS_HELPER(select->optional_words, out, out_length);
		break;
	case join_STATEMENT:;
		SqlJoin *start_join, *cur_join;
		UNPACK_SQL_STATEMENT(cur_join, stmt, join);
		start_join = cur_join;
		do {
			CALL_DECOMPRESS_HELPER(cur_join->value, out, out_length);
			CALL_DECOMPRESS_HELPER(cur_join->condition, out, out_length);
			if (0 == cur_join->next) {
				cur_join->next = start_join;
			} else {
				cur_join->next = R2A(cur_join->next);
			}
			cur_join->next->prev = cur_join;
			cur_join = cur_join->next;
		} while (cur_join != start_join);
		if (table_alias_STATEMENT == start_join->value->type) {
			start_join->max_unique_id = start_join->value->v.table_alias->unique_id + 1;
		} else if (set_operation_STATEMENT == start_join->value->type) {
			/* Since right branch of a set operation will have the last table_alias created for the operation.
			 * The right most table_alias unique_id should be considered as it will be the last unique_id set.
			 */
			start_join->max_unique_id = start_join->value->v.set_operation->operand[1]->v.table_alias->unique_id + 1;
		} else {
			assert(FALSE);
		}
		break;
	case column_list_alias_STATEMENT:;
		SqlColumnListAlias *cur_cla, *start_cla;
		UNPACK_SQL_STATEMENT(cur_cla, stmt, column_list_alias);
		start_cla = cur_cla;
		do {
			if (NULL == cur_cla->bin_defn_offset) {
				/* This column_alias_STATEMENT case block can be reached again in a nested/recursive call while the
				 * original column list is still not all decompressed. If we notice in the nested call that a
				 * cla is already decompressed, we need to return right away because it could be in the middle of
				 * being decompressed (in the parent call) which means that the cur_cla->next link might not be
				 * accurate at this point (which is what we need to traverse the linked list). That is the reason
				 * why we need the break below.
				 */
				break;
			}
			cur_cla->bin_defn_offset = NULL;
			CALL_DECOMPRESS_HELPER(cur_cla->column_list, out, out_length);
			CALL_DECOMPRESS_HELPER(cur_cla->alias, out, out_length);
			CALL_DECOMPRESS_HELPER(cur_cla->keywords, out, out_length);
			CALL_DECOMPRESS_HELPER(cur_cla->outer_query_column_alias, out, out_length);
			if (0 == cur_cla->next) {
				cur_cla->next = start_cla;
			} else {
				cur_cla->next = R2A(cur_cla->next);
			}
			cur_cla->next->prev = cur_cla;
			cur_cla = cur_cla->next;
		} while (cur_cla != start_cla);
		break;
	case column_alias_STATEMENT:;
		SqlColumnAlias *column_alias;
		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		if (NULL == column_alias->bin_defn_offset) {
			/* This object is already decompressed i.e. its pointers would have already
			 * gone through R2A. No need of again going through CALL_DECOMPRESS_HELPER()
			 */
			break;
		}
		column_alias->bin_defn_offset = NULL;
		CALL_DECOMPRESS_HELPER(column_alias->column, out, out_length);
		CALL_DECOMPRESS_HELPER(column_alias->table_alias_stmt, out, out_length);
		CALL_DECOMPRESS_HELPER(column_alias->set_oper_stmt, out, out_length);
		break;
	case aggregate_function_STATEMENT:;
		SqlAggregateFunction *aggr;
		UNPACK_SQL_STATEMENT(aggr, stmt, aggregate_function);
		CALL_DECOMPRESS_HELPER(aggr->parameter, out, out_length);
		CALL_DECOMPRESS_HELPER(aggr->table_alias_stmt, out, out_length);
		break;
	case set_operation_STATEMENT:;
		SqlSetOperation *set_oper;
		UNPACK_SQL_STATEMENT(set_oper, stmt, set_operation);
		CALL_DECOMPRESS_HELPER(set_oper->operand[0], out, out_length);
		CALL_DECOMPRESS_HELPER(set_oper->operand[1], out, out_length);
		CALL_DECOMPRESS_HELPER(set_oper->col_type_list_stmt, out, out_length);
		break;
	case row_value_STATEMENT:;
		SqlRowValue *cur_row_value, *start_row_value;
		UNPACK_SQL_STATEMENT(cur_row_value, stmt, row_value);
		start_row_value = cur_row_value;
		do {
			CALL_DECOMPRESS_HELPER(cur_row_value->value_list, out, out_length);
			if (0 == cur_row_value->next) {
				cur_row_value->next = start_row_value;
			} else {
				cur_row_value->next = R2A(cur_row_value->next);
			}
			cur_row_value->next->prev = cur_row_value;
			cur_row_value = cur_row_value->next;
		} while (cur_row_value != start_row_value);
		break;
	case table_value_STATEMENT:;
		SqlTableValue *table_value;
		UNPACK_SQL_STATEMENT(table_value, stmt, table_value);
		CALL_DECOMPRESS_HELPER(table_value->row_value_stmt, out, out_length);
		CALL_DECOMPRESS_HELPER(table_value->column_stmt, out, out_length);
		break;
	case create_function_STATEMENT:
		UNPACK_SQL_STATEMENT(function, stmt, create_function);
		CALL_DECOMPRESS_HELPER(function->function_name, out, out_length);
		CALL_DECOMPRESS_HELPER(function->parameter_type_list, out, out_length);
		CALL_DECOMPRESS_HELPER(function->return_type, out, out_length);
		CALL_DECOMPRESS_HELPER(function->extrinsic_function, out, out_length);
		CALL_DECOMPRESS_HELPER(function->function_hash, out, out_length);
#ifndef NDEBUG
		/* Validate that the function oid noted at compress_statement.c time still exists.
		 * (i.e. the function did not get deleted in between because a CHECK constraint relied on it).
		 * A DROP FUNCTION on that function in the meantime should have errored out.
		 */
		ydb_buffer_t octo_global, function_subs[4];
		YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
		YDB_STRING_TO_BUFFER(OCTOLIT_FUNCTIONS, &function_subs[0]);
		YDB_STRING_TO_BUFFER(function->function_name->v.value->v.string_literal, &function_subs[1]);
		YDB_STRING_TO_BUFFER(function->function_hash->v.value->v.string_literal, &function_subs[2]);
		YDB_STRING_TO_BUFFER(OCTOLIT_OID, &function_subs[3]);

		ydb_buffer_t ret;
		char	     oid_buff[INT32_TO_STRING_MAX];
		ret.buf_addr = &oid_buff[0];
		ret.len_alloc = sizeof(oid_buff);

		int status;
		status = ydb_get_s(&octo_global, 4, &function_subs[0], &ret);
		/* Either the function exists, or the EXTRACT column was a string literal, in which case there
		 * will be no SQL function dependency.
		 */
		assert((YDB_OK == status) || (YDB_ERR_GVUNDEF == status));
#endif
		break;
	case parameter_type_list_STATEMENT:
		UNPACK_SQL_STATEMENT(cur_parameter_type_list, stmt, parameter_type_list);
		if (NULL == cur_parameter_type_list) {
			// No parameter types were specified, nothing to decompress
			break;
		}
		start_parameter_type_list = cur_parameter_type_list;
		do {
			if (0 == cur_parameter_type_list->next) {
				cur_parameter_type_list->next = start_parameter_type_list;
			} else {
				cur_parameter_type_list->next = R2A(cur_parameter_type_list->next);
			}
			CALL_DECOMPRESS_HELPER(cur_parameter_type_list->data_type_struct, out, out_length);
			cur_parameter_type_list->next->prev = cur_parameter_type_list;
			cur_parameter_type_list = cur_parameter_type_list->next;
		} while (cur_parameter_type_list != start_parameter_type_list);
		break;
	case data_type_struct_STATEMENT:
		/* Not possible as we would have returned earlier. But included to avoid a [-Wswitch] compiler warning. */
		assert(FALSE);
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		switch (value->type) {
		case CALCULATED_VALUE:
			CALL_DECOMPRESS_HELPER(value->v.calculated, out, out_length);
			break;
		case COERCE_TYPE:
			CALL_DECOMPRESS_HELPER(value->v.coerce_target, out, out_length);
			break;
		case BOOLEAN_VALUE:
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case BOOLEAN_OR_STRING_LITERAL:
			/* value of such type which are unresolved to either BOOLEAN or STRING till this point
			 * will be set to STRING type later on by hash_canonical_query(). Treat this value similar
			 * to a STRING_LITERAL at this point.
			 */
		case STRING_LITERAL:
		case DELIM_VALUE:
		case NUL_VALUE:
		case FUNCTION_NAME:
		case FUNCTION_HASH:
		case COLUMN_REFERENCE:
		case TABLE_ASTERISK:
			value->v.string_literal = R2A(value->v.string_literal);
			break;
		case SELECT_ASTERISK:
		default:
			assert(FALSE);
			FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
			return NULL;
			break;
		}
		break;
	case column_STATEMENT:
		UNPACK_SQL_STATEMENT(cur_column, stmt, column);
		start_column = cur_column;
		do {
			if (NULL == cur_column->bin_defn_offset) {
				/* This column_STATEMENT case block can be reached again in a nested/recursive call while the
				 * original column list is still not all decompressed. If we notice in the nested call that a
				 * column is already decompressed, we need to return right away because it could be in the middle of
				 * being decompressed (in the parent call) which means that the cur_column->next link might not be
				 * accurate at this point (which is what we need to traverse the linked list). That is the reason
				 * why we need the break below.
				 */
				break;
			}
			cur_column->bin_defn_offset = NULL;
			CALL_DECOMPRESS_HELPER(cur_column->table, out, out_length);
			CALL_DECOMPRESS_HELPER(cur_column->columnName, out, out_length);
			CALL_DECOMPRESS_HELPER(cur_column->keywords, out, out_length);
			/* Fix "cur_column->delim" now that "cur_column->keywords" is set up */
			cur_keyword = get_keyword(cur_column, OPTIONAL_DELIM);
			cur_column->delim = ((NULL != cur_keyword) ? cur_keyword->v : NULL);
			if (0 == cur_column->next) {
				cur_column->next = start_column;
			} else {
				cur_column->next = R2A(cur_column->next);
			}
			cur_column->next->prev = cur_column;
			// cur_column->table = (SqlStatement *)out;
			/* table is first element in compressed structure i.e. "out" */
			cur_column = cur_column->next;
		} while (cur_column != start_column);
		break;
	case keyword_STATEMENT:
		UNPACK_SQL_STATEMENT(start_keyword, stmt, keyword);
		cur_keyword = start_keyword;
		do {
			CALL_DECOMPRESS_HELPER(cur_keyword->v, out, out_length);
			if (cur_keyword->next == 0) {
				cur_keyword->next = start_keyword;
			} else {
				cur_keyword->next = R2A(cur_keyword->next);
			}
			cur_keyword->next->prev = cur_keyword;
			cur_keyword = cur_keyword->next;
		} while (cur_keyword != start_keyword);
		break;
	case constraint_STATEMENT:;
		SqlConstraint *constraint;

		UNPACK_SQL_STATEMENT(constraint, stmt, constraint);
		CALL_DECOMPRESS_HELPER(constraint->name, out, out_length);
		CALL_DECOMPRESS_HELPER(constraint->definition, out, out_length);
		if (OPTIONAL_CHECK_CONSTRAINT == constraint->type) {
			CALL_DECOMPRESS_HELPER(constraint->v.check_columns, out, out_length);
		} else if (UNIQUE_CONSTRAINT == constraint->type) {
			CALL_DECOMPRESS_HELPER(constraint->v.uniq_gblname, out, out_length);
		}
		break;
	case unary_STATEMENT:;
		SqlUnaryOperation *unary;

		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		CALL_DECOMPRESS_HELPER(unary->operand, out, out_length);
		break;
	case binary_STATEMENT:;
		int		    i;
		SqlBinaryOperation *binary;

		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		for (i = 0; i < 2; i++) {
			CALL_DECOMPRESS_HELPER(binary->operands[i], out, out_length);
		}
		break;
	case function_call_STATEMENT:;
		SqlFunctionCall *function_call;

		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		CALL_DECOMPRESS_HELPER(function_call->function_name, out, out_length);
		CALL_DECOMPRESS_HELPER(function_call->function_schema, out, out_length);
		CALL_DECOMPRESS_HELPER(function_call->parameters, out, out_length);
		break;
	case coalesce_STATEMENT:;
		SqlCoalesceCall *coalesce_call;

		UNPACK_SQL_STATEMENT(coalesce_call, stmt, coalesce);
		CALL_DECOMPRESS_HELPER(coalesce_call->arguments, out, out_length);
		break;
	case greatest_STATEMENT:;
		SqlGreatest *greatest;

		UNPACK_SQL_STATEMENT(greatest, stmt, greatest);
		CALL_DECOMPRESS_HELPER(greatest->arguments, out, out_length);
		break;
	case least_STATEMENT:;
		SqlLeast *least;

		UNPACK_SQL_STATEMENT(least, stmt, least);
		CALL_DECOMPRESS_HELPER(least->arguments, out, out_length);
		break;
	case null_if_STATEMENT:;
		SqlNullIf *null_if;

		UNPACK_SQL_STATEMENT(null_if, stmt, null_if);
		CALL_DECOMPRESS_HELPER(null_if->left, out, out_length);
		CALL_DECOMPRESS_HELPER(null_if->right, out, out_length);
		break;
	case column_list_STATEMENT:;
		SqlColumnList *start_column_list, *cur_column_list;

		UNPACK_SQL_STATEMENT(start_column_list, stmt, column_list);
		cur_column_list = start_column_list;
		do {
			CALL_DECOMPRESS_HELPER(cur_column_list->value, out, out_length);
			if (cur_column_list->next == 0) {
				cur_column_list->next = start_column_list;
			} else {
				cur_column_list->next = R2A(cur_column_list->next);
			}
			cur_column_list->next->prev = cur_column_list;
			cur_column_list = cur_column_list->next;
		} while (cur_column_list != start_column_list);
		break;
	case cas_STATEMENT:;
		SqlCaseStatement *cas;

		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		CALL_DECOMPRESS_HELPER(cas->value, out, out_length);
		CALL_DECOMPRESS_HELPER(cas->branches, out, out_length);
		CALL_DECOMPRESS_HELPER(cas->optional_else, out, out_length);
		break;
	case cas_branch_STATEMENT:;
		SqlCaseBranchStatement *start_cas_branch, *cur_cas_branch;

		UNPACK_SQL_STATEMENT(start_cas_branch, stmt, cas_branch);
		cur_cas_branch = start_cas_branch;
		do {
			CALL_DECOMPRESS_HELPER(cur_cas_branch->condition, out, out_length);
			CALL_DECOMPRESS_HELPER(cur_cas_branch->value, out, out_length);
			if (cur_cas_branch->next == 0) {
				cur_cas_branch->next = start_cas_branch;
			} else {
				cur_cas_branch->next = R2A(cur_cas_branch->next);
			}
			cur_cas_branch->next->prev = cur_cas_branch;
			cur_cas_branch = cur_cas_branch->next;
		} while (cur_cas_branch != start_cas_branch);
		break;
	case array_STATEMENT:;
		SqlArray *array;

		UNPACK_SQL_STATEMENT(array, stmt, array);
		CALL_DECOMPRESS_HELPER(array->argument, out, out_length);
		break;
	/* The below types are not possible currently in a CREATE TABLE definition */
	case insert_STATEMENT:
	case drop_table_STATEMENT:
	case drop_view_STATEMENT:
	case drop_function_STATEMENT:
	case truncate_table_STATEMENT:
	case begin_STATEMENT:
	case commit_STATEMENT:
	case dynamic_sql_STATEMENT:
	case set_STATEMENT:
	case show_STATEMENT:
	case no_data_STATEMENT:
	case delim_char_list_STATEMENT:
	case index_STATEMENT:
	case join_type_STATEMENT:
	case discard_all_STATEMENT:
	case history_STATEMENT:
	case delete_from_STATEMENT:
	case update_STATEMENT:
	case display_relation_STATEMENT:
	case invalid_STATEMENT:
		/* Do not add "default:" case as we want to enumerate each explicit case here instead of having a
		 * general purpose bucket where all types not listed above fall into as that could hide subtle bugs.
		 */
		assert(FALSE);
		FATAL(ERR_UNKNOWN_KEYWORD_STATE, "");
		return NULL;
		break;
	}
	return stmt;
}
