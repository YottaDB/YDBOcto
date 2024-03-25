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
#include "octo_type_check.h"

#define CALL_COMPRESS_HELPER(temp, value, new_value, out, out_length)                                 \
	{                                                                                             \
		(temp) = compress_statement_helper((value), (out), (out_length), is_view_processing); \
		if (NULL != (out)) {                                                                  \
			(new_value) = (temp);                                                         \
			if (NULL != new_value) {                                                      \
				A2R((new_value));                                                     \
			}                                                                             \
		}                                                                                     \
	}

#define MAX_BIN_DEFN_OFFSET (void *)-1
void *compress_statement_helper(SqlStatement *stmt, char *out, int *out_length, boolean_t is_view_processing);

void compress_statement(SqlStatement *stmt, char **out, int *out_length, boolean_t is_view_processing) {
	*out_length = 0;
	compress_statement_helper(stmt, NULL, out_length, is_view_processing);
	if (0 != *out_length) {
		*out = malloc(*out_length);
		*out_length = 0;
		compress_statement_helper(stmt, *out, out_length, is_view_processing);
	} else {
		assert(FALSE);
		*out = NULL;
	}
	return;
}

/*
 * Returns a pointer to a new memory location for stmt within the out buffer
 *
 * If the out buffer is NULL, doesn't copy the statement, but just counts size
 */
void *compress_statement_helper(SqlStatement *stmt, char *out, int *out_length, boolean_t is_view_processing) {
	SqlColumn	     *cur_column, *start_column, *new_column;
	SqlOptionalKeyword   *start_keyword, *cur_keyword, *new_keyword;
	SqlStatement	     *new_stmt;
	SqlTable	     *table, *new_table;
	SqlFunction	     *function, *new_function;
	SqlParameterTypeList *new_parameter_type_list, *cur_parameter_type_list, *start_parameter_type_list;
	SqlValue	     *value, *new_value;
	int		      len;
	void		     *r, *ret;
	// Following code is to prevent [-Wmaybe-uninitialized] compiler warning from gcc 7.5.0
	// Asserts before the use of new_stmt ensure its value is never NULL
	new_stmt = NULL;
	if ((NULL == stmt) || (NULL == stmt->v.value))
		return NULL;

	if (NULL != out) {
		new_stmt = ((void *)&out[*out_length]);
		memcpy(new_stmt, stmt, sizeof(SqlStatement));
		ret = new_stmt;
	} else {
		ret = NULL;
	}
	*out_length += sizeof(SqlStatement);
	if (NULL != out) {
		if (data_type_struct_STATEMENT == new_stmt->type) {
			/* In this case, the relevant data is the SqlDataTypeStruct member from the union member of `stmt`,
			 * i.e. NOT a pointer. So, do not perform the A2R conversion and just return as is.
			 * See similar note in decompress_statement.c.
			 */
			return ret;
		}
		new_stmt->v.value = ((void *)&out[*out_length]);
		A2R(new_stmt->v.value);
	}
	switch (stmt->type) {
	case create_table_STATEMENT:
		UNPACK_SQL_STATEMENT(table, stmt, create_table);
		if (NULL != out) {
			if (MAX_BIN_DEFN_OFFSET == table->bin_defn_offset) {
				new_table = ((void *)&out[*out_length]);
				table->bin_defn_offset = new_table;
				memcpy(new_table, table, sizeof(SqlTable));
			} else {
				assert(NULL != new_stmt);
				new_stmt->v.create_table = table->bin_defn_offset;
				A2R(new_stmt->v.create_table);
				return ret;
			}
		} else {
			if (MAX_BIN_DEFN_OFFSET == table->bin_defn_offset) {
				break;
			} else {
				/* Magic number to inform the next reference to this SqlTable to not add
				 * sizeof(SqlTable) again as we will be referencing pre-allocated value.
				 */
				table->bin_defn_offset = MAX_BIN_DEFN_OFFSET;
			}
		}
		*out_length += sizeof(SqlTable);
		CALL_COMPRESS_HELPER(r, table->tableName, new_table->tableName, out, out_length);
		CALL_COMPRESS_HELPER(r, table->source, new_table->source, out, out_length);
		CALL_COMPRESS_HELPER(r, table->columns, new_table->columns, out, out_length);
		CALL_COMPRESS_HELPER(r, table->delim, new_table->delim, out, out_length);
		CALL_COMPRESS_HELPER(r, table->aim_type, new_table->aim_type, out, out_length);
		/* table->readwrite is not a pointer value so no need to call CALL_COMPRESS_HELPER on this member */
		/* table->oid is not a pointer value so no need to call CALL_COMPRESS_HELPER on this member */
		/* table->if_not_exists_specified is not a pointer value */
		break;
	case create_view_STATEMENT:;
		/* Refer to https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1485833114 for details regarding
		 * why `bin_defn_offset` is not used in this case block.
		 */
		SqlView *view, *new_view;

		UNPACK_SQL_STATEMENT(view, stmt, create_view);
		if (NULL != out) {
			new_view = ((void *)&out[*out_length]);
			memcpy(new_view, view, sizeof(SqlView));
		}
		*out_length += sizeof(SqlView);
		CALL_COMPRESS_HELPER(r, view->viewName, new_view->viewName, out, out_length);
		CALL_COMPRESS_HELPER(r, view->src_table_alias_stmt, new_view->src_table_alias_stmt, out, out_length);
		break;
	case table_alias_STATEMENT:;
		SqlTableAlias *table_alias, *new_table_alias;
		UNPACK_SQL_STATEMENT(table_alias, stmt, table_alias);
		if (NULL != out) {
			/* `MAX_BIN_DEFN_OFFSET` value indicates that this instance is the first instance we are
			 * encountering during this invocation and its fields need to be compressed.
			 * Note: we do not expect NULL value here as the first invocation of compress_statement()
			 *       takes care of assigning MAX_BIN_DEFN_OFFSET when a reference is being compressed instead of the
			 *	 actual structure.
			 */
			if (MAX_BIN_DEFN_OFFSET == table_alias->bin_defn_offset) {
				new_table_alias = ((void *)&out[*out_length]);
				table_alias->bin_defn_offset = new_table_alias;
				memcpy(new_table_alias, table_alias, sizeof(SqlTableAlias));
			} else {
				/* Not `MAX_BIN_DEFN_OFFSET` which means we this value is already compressed. Refer to it
				 * using the address stored in `bin_defn_offset`.
				 */
				assert(NULL != new_stmt);
				new_stmt->v.table_alias = table_alias->bin_defn_offset;
				A2R(new_stmt->v.table_alias);
				return ret;
			}
		} else {
			/* This is a call to identify the lenght of binary storage required.
			 * If this is the first occurence of this structure then `bin_defn_offset`
			 * will have a value of NULL. In such a case consider the structures fields
			 * for computing the final size and set `bin_defn_offset` to MAX_BIN_DEFN_OFFSET such that
			 * any other reference to this structure can know that this structure can be
			 * referenced using a pointer, no need to include its actual size.
			 */
			if (MAX_BIN_DEFN_OFFSET == table_alias->bin_defn_offset) {
				break;
			} else {
				/* Refer to table_alias_STATEMENT comments. */
				table_alias->bin_defn_offset = MAX_BIN_DEFN_OFFSET;
			}
		}
		*out_length += sizeof(SqlTableAlias);
		CALL_COMPRESS_HELPER(r, table_alias->table, new_table_alias->table, out, out_length);
		CALL_COMPRESS_HELPER(r, table_alias->alias, new_table_alias->alias, out, out_length);
		CALL_COMPRESS_HELPER(r, table_alias->parent_table_alias, new_table_alias->parent_table_alias, out, out_length);
		CALL_COMPRESS_HELPER(r, table_alias->column_list, new_table_alias->column_list, out, out_length);
		CALL_COMPRESS_HELPER(r, table_alias->correlation_specification, new_table_alias->correlation_specification, out,
				     out_length);
		CALL_COMPRESS_HELPER(r, table_alias->table_asterisk_column_alias, new_table_alias->table_asterisk_column_alias, out,
				     out_length);
		/* table_alias->unique_id
		 * group_by_column_count
		 * aggregate_depth
		 * aggregate_function_or_group_by_or_having_specified
		 * do_group_by_checks
		 * qualify_query_stage
		 * are not all pointer values so no need to call CALL_COMPRESS_HELPER on this member
		 * bin_defn_offset
		 * is only used during compression so no need to consider this value.
		 */
		break;
	case select_STATEMENT:;
		SqlSelectStatement *select, *new_select;
		UNPACK_SQL_STATEMENT(select, stmt, select);
		if (NULL != out) {
			new_select = ((void *)&out[*out_length]);
			memcpy(new_select, select, sizeof(SqlSelectStatement));
		}
		*out_length += sizeof(SqlSelectStatement);
		CALL_COMPRESS_HELPER(r, select->table_list, new_select->table_list, out, out_length);
		CALL_COMPRESS_HELPER(r, select->where_expression, new_select->where_expression, out, out_length);
		CALL_COMPRESS_HELPER(r, select->select_list, new_select->select_list, out, out_length);
		CALL_COMPRESS_HELPER(r, select->group_by_expression, new_select->group_by_expression, out, out_length);
		CALL_COMPRESS_HELPER(r, select->having_expression, new_select->having_expression, out, out_length);
		CALL_COMPRESS_HELPER(r, select->order_by_expression, new_select->order_by_expression, out, out_length);
		CALL_COMPRESS_HELPER(r, select->optional_words, new_select->optional_words, out, out_length);
		break;
	case join_STATEMENT:;
		SqlJoin *start_join, *cur_join, *new_join;
		UNPACK_SQL_STATEMENT(start_join, stmt, join);
		cur_join = start_join;
		do {
			if (NULL != out) {
				new_join = ((void *)&out[*out_length]);
				memcpy(new_join, cur_join, sizeof(SqlJoin));
				new_join->next = new_join->prev = NULL;
			}
			*out_length += sizeof(SqlJoin);
			CALL_COMPRESS_HELPER(r, cur_join->value, new_join->value, out, out_length);
			CALL_COMPRESS_HELPER(r, cur_join->condition, new_join->condition, out, out_length);
			cur_join = cur_join->next;
			if ((NULL != out) && (cur_join != start_join)) {
				new_join->next = ((void *)&out[*out_length]);
				A2R(new_join->next);
			}
		} while (cur_join != start_join);
		break;
	case column_list_alias_STATEMENT:;
		SqlColumnListAlias *cur_cla, *start_cla, *new_cla;
		UNPACK_SQL_STATEMENT(start_cla, stmt, column_list_alias);
		cur_cla = start_cla;
		do {
			if (NULL != out) {
				if (MAX_BIN_DEFN_OFFSET == cur_cla->bin_defn_offset) {
					new_cla = ((void *)&out[*out_length]);
					cur_cla->bin_defn_offset = new_cla;
					memcpy(new_cla, cur_cla, sizeof(SqlColumnListAlias));
					new_cla->next = new_cla->prev = NULL;
					*out_length += sizeof(SqlColumnListAlias);
					CALL_COMPRESS_HELPER(r, cur_cla->column_list, new_cla->column_list, out, out_length);
					CALL_COMPRESS_HELPER(r, cur_cla->alias, new_cla->alias, out, out_length);
					CALL_COMPRESS_HELPER(r, cur_cla->keywords, new_cla->keywords, out, out_length);
					CALL_COMPRESS_HELPER(r, cur_cla->outer_query_column_alias,
							     new_cla->outer_query_column_alias, out, out_length);
					// duplicate_of_column
					// tbl_and_col_id
				} else {
					if (cur_cla == start_cla) {
						assert(NULL != new_stmt);
						new_stmt->v.column_list_alias = cur_cla->bin_defn_offset;
						new_cla = new_stmt->v.column_list_alias;
						A2R(new_stmt->v.column_list_alias);
					} else {
						new_cla = cur_cla->bin_defn_offset;
					}
				}
			} else {
				if (MAX_BIN_DEFN_OFFSET == cur_cla->bin_defn_offset) {
					// Do nothing
				} else {
					/* Refer to table_alias_STATEMENT comments */
					cur_cla->bin_defn_offset = MAX_BIN_DEFN_OFFSET;
					*out_length += sizeof(SqlColumnListAlias);
					CALL_COMPRESS_HELPER(r, cur_cla->column_list, new_cla->column_list, out, out_length);
					CALL_COMPRESS_HELPER(r, cur_cla->alias, new_cla->alias, out, out_length);
					CALL_COMPRESS_HELPER(r, cur_cla->keywords, new_cla->keywords, out, out_length);
					CALL_COMPRESS_HELPER(r, cur_cla->outer_query_column_alias,
							     new_cla->outer_query_column_alias, out, out_length);
					// duplicate_of_column
					// tbl_and_col_id
				}
			}
			cur_cla = cur_cla->next;
			if ((NULL != out) && (cur_cla != start_cla)) {
				if (MAX_BIN_DEFN_OFFSET == cur_cla->bin_defn_offset) {
					new_cla->next = ((void *)&out[*out_length]);
				} else {
					new_cla->next = cur_cla->bin_defn_offset;
				}
				A2R(new_cla->next);
			}
		} while (cur_cla != start_cla);
		break;
	case column_alias_STATEMENT:;
		SqlColumnAlias *column_alias, *new_column_alias;
		UNPACK_SQL_STATEMENT(column_alias, stmt, column_alias);
		if (NULL != out) {
			if (MAX_BIN_DEFN_OFFSET == column_alias->bin_defn_offset) {
				new_column_alias = ((void *)&out[*out_length]);
				column_alias->bin_defn_offset = new_column_alias;
				memcpy(new_column_alias, column_alias, sizeof(SqlColumnAlias));
			} else {
				assert(NULL != new_stmt);
				new_stmt->v.column_alias = column_alias->bin_defn_offset;
				A2R(new_stmt->v.column_alias);
				return ret;
			}
		} else {
			if (MAX_BIN_DEFN_OFFSET == column_alias->bin_defn_offset) {
				break;
			} else {
				/* Refer to table_alias_STATEMENT comments */
				column_alias->bin_defn_offset = MAX_BIN_DEFN_OFFSET;
			}
		}
		*out_length += sizeof(SqlColumnAlias);
		CALL_COMPRESS_HELPER(r, column_alias->column, new_column_alias->column, out, out_length);
		CALL_COMPRESS_HELPER(r, column_alias->table_alias_stmt, new_column_alias->table_alias_stmt, out, out_length);
		CALL_COMPRESS_HELPER(r, column_alias->set_oper_stmt, new_column_alias->set_oper_stmt, out, out_length);
		break;
	case aggregate_function_STATEMENT:;
		SqlAggregateFunction *aggr, *new_aggr;
		UNPACK_SQL_STATEMENT(aggr, stmt, aggregate_function);
		if (NULL != out) {
			new_aggr = ((void *)&out[*out_length]);
			memcpy(new_aggr, aggr, sizeof(SqlAggregateFunction));
		}
		*out_length += sizeof(SqlAggregateFunction);
		CALL_COMPRESS_HELPER(r, aggr->parameter, new_aggr->parameter, out, out_length);
		CALL_COMPRESS_HELPER(r, aggr->table_alias_stmt, new_aggr->table_alias_stmt, out, out_length);
		break;
	case set_operation_STATEMENT:;
		SqlSetOperation *set_oper, *new_set_oper;
		UNPACK_SQL_STATEMENT(set_oper, stmt, set_operation);
		if (NULL != out) {
			new_set_oper = ((void *)&out[*out_length]);
			memcpy(new_set_oper, set_oper, sizeof(SqlSetOperation));
		}
		*out_length += sizeof(SqlSetOperation);
		CALL_COMPRESS_HELPER(r, set_oper->operand[0], new_set_oper->operand[0], out, out_length);
		CALL_COMPRESS_HELPER(r, set_oper->operand[1], new_set_oper->operand[1], out, out_length);
		CALL_COMPRESS_HELPER(r, set_oper->col_type_list_stmt, new_set_oper->col_type_list_stmt, out, out_length);
		break;
	case row_value_STATEMENT:;
		SqlRowValue *cur_row_value, *start_row_value, *new_row_value;
		UNPACK_SQL_STATEMENT(cur_row_value, stmt, row_value);
		start_row_value = cur_row_value;
		do {
			if (NULL != out) {
				new_row_value = ((void *)&out[*out_length]);
				memcpy(new_row_value, cur_row_value, sizeof(SqlRowValue));
				new_row_value->next = new_row_value->prev = NULL;
			}
			*out_length += sizeof(SqlRowValue);
			CALL_COMPRESS_HELPER(r, cur_row_value->value_list, new_row_value->value_list, out, out_length);
			cur_row_value = cur_row_value->next;
			if ((NULL != out) && (cur_row_value != start_row_value)) {
				new_row_value->next = ((void *)&out[*out_length]);
				A2R(new_row_value->next);
			}
		} while (cur_row_value != start_row_value);
		break;
	case table_value_STATEMENT:;
		SqlTableValue *table_value, *new_table_value;
		UNPACK_SQL_STATEMENT(table_value, stmt, table_value);
		if (NULL != out) {
			new_table_value = ((void *)&out[*out_length]);
			memcpy(new_table_value, table_value, sizeof(SqlTableValue));
		}
		*out_length += sizeof(SqlTableValue);
		CALL_COMPRESS_HELPER(r, table_value->row_value_stmt, new_table_value->row_value_stmt, out, out_length);
		CALL_COMPRESS_HELPER(r, table_value->column_stmt, new_table_value->column_stmt, out, out_length);
		break;
	case create_function_STATEMENT:
		UNPACK_SQL_STATEMENT(function, stmt, create_function);
		if (NULL != out) {
			new_function = ((void *)&out[*out_length]);
			memcpy(new_function, function, sizeof(SqlFunction));
		}
		*out_length += sizeof(SqlFunction);
		CALL_COMPRESS_HELPER(r, function->function_name, new_function->function_name, out, out_length);
		CALL_COMPRESS_HELPER(r, function->parameter_type_list, new_function->parameter_type_list, out, out_length);
		CALL_COMPRESS_HELPER(r, function->return_type, new_function->return_type, out, out_length);
		CALL_COMPRESS_HELPER(r, function->extrinsic_function, new_function->extrinsic_function, out, out_length);
		CALL_COMPRESS_HELPER(r, function->function_hash, new_function->function_hash, out, out_length);
		break;
	case parameter_type_list_STATEMENT:
		UNPACK_SQL_STATEMENT(cur_parameter_type_list, stmt, parameter_type_list);
		if (NULL == cur_parameter_type_list) {
			// No parameter types were specified, nothing to compress
			break;
		}
		start_parameter_type_list = cur_parameter_type_list;
		do {
			if (NULL != out) {
				new_parameter_type_list = ((void *)&out[*out_length]);
				memcpy(new_parameter_type_list, cur_parameter_type_list, sizeof(SqlParameterTypeList));
				new_parameter_type_list->next = new_parameter_type_list->prev = NULL;
			}
			*out_length += sizeof(SqlParameterTypeList);

			CALL_COMPRESS_HELPER(r, cur_parameter_type_list->data_type_struct,
					     new_parameter_type_list->data_type_struct, out, out_length);
			cur_parameter_type_list = cur_parameter_type_list->next;
			if ((NULL != out) && (cur_parameter_type_list != start_parameter_type_list)) {
				new_parameter_type_list->next = ((void *)&out[*out_length]);
				A2R(new_parameter_type_list->next);
			}
		} while (cur_parameter_type_list != start_parameter_type_list);
		break;
	case data_type_struct_STATEMENT:
		/* Hit this case only for the initial length count, i.e. (NULL == out).
		 * See note on "data_type_struct_STATEMENT" above.
		 */
		assert(NULL == out);
		break;
	case value_STATEMENT:
		UNPACK_SQL_STATEMENT(value, stmt, value);
		if (NULL != out) {
			new_value = ((void *)&out[*out_length]);
			memcpy(new_value, value, sizeof(SqlValue));
		} else {
			new_value = NULL; /* Needed to avoid false [-Wmaybe-uninitialized] warnings from compiler */
		}
		*out_length += sizeof(SqlValue);
		switch (value->type) {
		case CALCULATED_VALUE:
			CALL_COMPRESS_HELPER(r, value->v.calculated, new_value->v.calculated, out, out_length);
			break;
		case COERCE_TYPE:
			if ((is_view_processing) && (NULL != new_value)) {
				new_value->u.coerce_type.coerced_type.scale_parameter_index = 0;
				new_value->u.coerce_type.coerced_type.size_or_precision_parameter_index = 0;
			}
			CALL_COMPRESS_HELPER(r, value->v.coerce_target, new_value->v.coerce_target, out, out_length);
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
			len = strlen(value->v.string_literal);
			if (NULL != out) {
				memcpy(&out[*out_length], value->v.string_literal, len);
				new_value->v.string_literal = &out[*out_length];
				A2R(new_value->v.string_literal);
				if (is_view_processing) {
					new_value->parameter_index = 0;
				}
			}
			*out_length += len;
			if (NULL != out) {
				out[*out_length] = '\0';
			}
			*out_length += 1;
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
			if (NULL != out) {
				if (MAX_BIN_DEFN_OFFSET == cur_column->bin_defn_offset) {
					new_column = ((void *)&out[*out_length]);
					cur_column->bin_defn_offset = new_column;
					memcpy(new_column, cur_column, sizeof(SqlColumn));
					new_column->next = new_column->prev = NULL;
					/* Each column refers to the same SqlStatement corresponding
					 * to the SqlTable. SqlStatement has bin_defn_offset which
					 * ensures that the same reference is referred by all the SqlColumn
					 * nodes.
					 */
					*out_length += sizeof(SqlColumn);
					CALL_COMPRESS_HELPER(r, cur_column->table, new_column->table, out, out_length);
					CALL_COMPRESS_HELPER(r, cur_column->columnName, new_column->columnName, out, out_length);
					CALL_COMPRESS_HELPER(r, cur_column->keywords, new_column->keywords, out, out_length);

				} else {
					if (cur_column == start_column) {
						assert(NULL != new_stmt);
						new_stmt->v.column = cur_column->bin_defn_offset;
						new_column = new_stmt->v.column;
						A2R(new_stmt->v.column);
					} else {
						new_column = cur_column->bin_defn_offset;
					}
				}
			} else {
				if (MAX_BIN_DEFN_OFFSET == cur_column->bin_defn_offset) {
					// Do nothing
				} else {
					/* Refer to table_alias_STATEMENT comments */
					cur_column->bin_defn_offset = MAX_BIN_DEFN_OFFSET;
					*out_length += sizeof(SqlColumn);
					CALL_COMPRESS_HELPER(r, cur_column->table, new_column->table, out, out_length);
					CALL_COMPRESS_HELPER(r, cur_column->columnName, new_column->columnName, out, out_length);
					CALL_COMPRESS_HELPER(r, cur_column->keywords, new_column->keywords, out, out_length);
				}
			}
			/* cur_column->delim can be derived from cur_column->keywords and so does not need to be compressed */
			cur_column = cur_column->next;
			if ((NULL != out) && (cur_column != start_column)) {
				if (MAX_BIN_DEFN_OFFSET == cur_column->bin_defn_offset) {
					new_column->next = ((void *)&out[*out_length]);
				} else {
					new_column->next = cur_column->bin_defn_offset;
				}
				A2R(new_column->next);
			}
		} while (cur_column != start_column);
		break;
	case keyword_STATEMENT:
		UNPACK_SQL_STATEMENT(start_keyword, stmt, keyword);
		cur_keyword = start_keyword;
		do {
			if (NULL != out) {
				new_keyword = ((void *)&out[*out_length]);
				memcpy(new_keyword, cur_keyword, sizeof(SqlOptionalKeyword));
				new_keyword->next = new_keyword->prev = NULL;
			}
			*out_length += sizeof(SqlOptionalKeyword);
			CALL_COMPRESS_HELPER(r, cur_keyword->v, new_keyword->v, out, out_length);
			cur_keyword = cur_keyword->next;
			if ((NULL != out) && (cur_keyword != start_keyword)) {
				new_keyword->next = ((void *)&out[*out_length]);
				A2R(new_keyword->next);
			}
		} while (cur_keyword != start_keyword);
		break;
	case constraint_STATEMENT:;
		SqlConstraint *constraint, *new_constraint;

		UNPACK_SQL_STATEMENT(constraint, stmt, constraint);
		if (NULL != out) {
			new_constraint = ((void *)&out[*out_length]);
			memcpy(new_constraint, constraint, sizeof(SqlConstraint));
		}
		*out_length += sizeof(SqlConstraint);
		CALL_COMPRESS_HELPER(r, constraint->name, new_constraint->name, out, out_length);
		CALL_COMPRESS_HELPER(r, constraint->definition, new_constraint->definition, out, out_length);
		if (OPTIONAL_CHECK_CONSTRAINT == constraint->type) {
			CALL_COMPRESS_HELPER(r, constraint->v.check_columns, new_constraint->v.check_columns, out, out_length);
		} else if (UNIQUE_CONSTRAINT == constraint->type) {
			CALL_COMPRESS_HELPER(r, constraint->v.uniq_gblname, new_constraint->v.uniq_gblname, out, out_length);
		}
		break;
	case unary_STATEMENT:;
		SqlUnaryOperation *unary, *new_unary;

		UNPACK_SQL_STATEMENT(unary, stmt, unary);
		if (NULL != out) {
			new_unary = ((void *)&out[*out_length]);
			memcpy(new_unary, unary, sizeof(SqlUnaryOperation));
		}
		*out_length += sizeof(SqlUnaryOperation);
		CALL_COMPRESS_HELPER(r, unary->operand, new_unary->operand, out, out_length);
		break;
	case binary_STATEMENT:;
		SqlBinaryOperation *binary, *new_binary;
		UNPACK_SQL_STATEMENT(binary, stmt, binary);
		if (NULL != out) {
			new_binary = ((void *)&out[*out_length]);
			memcpy(new_binary, binary, sizeof(SqlBinaryOperation));
		}
		*out_length += sizeof(SqlBinaryOperation);

		int i;
		for (i = 0; i < 2; i++) {
			CALL_COMPRESS_HELPER(r, binary->operands[i], new_binary->operands[i], out, out_length);
		}
		break;
	case function_call_STATEMENT:;
		SqlFunctionCall *function_call, *new_function_call;
		UNPACK_SQL_STATEMENT(function_call, stmt, function_call);
		if (NULL != out) {
			new_function_call = ((void *)&out[*out_length]);
			memcpy(new_function_call, function_call, sizeof(SqlFunctionCall));
		}
		*out_length += sizeof(SqlFunctionCall);

		CALL_COMPRESS_HELPER(r, function_call->function_name, new_function_call->function_name, out, out_length);
		/* We only need the hash and oid parts of the function schema as that is what will be used at decompress time.
		 * But it is easy to store the entire structure so we do that here.
		 */
		CALL_COMPRESS_HELPER(r, function_call->function_schema, new_function_call->function_schema, out, out_length);
		CALL_COMPRESS_HELPER(r, function_call->parameters, new_function_call->parameters, out, out_length);
		break;
	case coalesce_STATEMENT:;
		SqlCoalesceCall *coalesce_call, *new_coalesce_call;
		UNPACK_SQL_STATEMENT(coalesce_call, stmt, coalesce);
		if (NULL != out) {
			new_coalesce_call = ((void *)&out[*out_length]);
			memcpy(new_coalesce_call, coalesce_call, sizeof(SqlCoalesceCall));
		}
		*out_length += sizeof(SqlCoalesceCall);
		CALL_COMPRESS_HELPER(r, coalesce_call->arguments, new_coalesce_call->arguments, out, out_length);
		break;
	case greatest_STATEMENT:;
		SqlGreatest *greatest, *new_greatest;
		UNPACK_SQL_STATEMENT(greatest, stmt, greatest);
		if (NULL != out) {
			new_greatest = ((void *)&out[*out_length]);
			memcpy(new_greatest, greatest, sizeof(SqlGreatest));
		}
		*out_length += sizeof(SqlGreatest);
		CALL_COMPRESS_HELPER(r, greatest->arguments, new_greatest->arguments, out, out_length);
		break;
	case least_STATEMENT:;
		SqlLeast *least, *new_least;
		UNPACK_SQL_STATEMENT(least, stmt, least);
		if (NULL != out) {
			new_least = ((void *)&out[*out_length]);
			memcpy(new_least, least, sizeof(SqlLeast));
		}
		*out_length += sizeof(SqlLeast);
		CALL_COMPRESS_HELPER(r, least->arguments, new_least->arguments, out, out_length);
		break;
	case null_if_STATEMENT:;
		SqlNullIf *null_if, *new_null_if;
		UNPACK_SQL_STATEMENT(null_if, stmt, null_if);
		if (NULL != out) {
			new_null_if = ((void *)&out[*out_length]);
			memcpy(new_null_if, null_if, sizeof(SqlNullIf));
		}
		*out_length += sizeof(SqlNullIf);
		CALL_COMPRESS_HELPER(r, null_if->left, new_null_if->left, out, out_length);
		CALL_COMPRESS_HELPER(r, null_if->right, new_null_if->right, out, out_length);
		break;
	case column_list_STATEMENT:;
		SqlColumnList *start_column_list, *cur_column_list, *new_column_list;

		UNPACK_SQL_STATEMENT(start_column_list, stmt, column_list);
		cur_column_list = start_column_list;
		do {
			if (NULL != out) {
				new_column_list = ((void *)&out[*out_length]);
				memcpy(new_column_list, cur_column_list, sizeof(SqlColumnList));
				new_column_list->next = new_column_list->prev = NULL;
			}
			*out_length += sizeof(SqlColumnList);
			CALL_COMPRESS_HELPER(r, cur_column_list->value, new_column_list->value, out, out_length);
			cur_column_list = cur_column_list->next;
			if ((NULL != out) && (cur_column_list != start_column_list)) {
				new_column_list->next = ((void *)&out[*out_length]);
				A2R(new_column_list->next);
			}
		} while (cur_column_list != start_column_list);
		break;
	case cas_STATEMENT:;
		SqlCaseStatement *cas, *new_cas;
		UNPACK_SQL_STATEMENT(cas, stmt, cas);
		if (NULL != out) {
			new_cas = ((void *)&out[*out_length]);
			memcpy(new_cas, cas, sizeof(SqlCaseStatement));
		}
		*out_length += sizeof(SqlCaseStatement);
		CALL_COMPRESS_HELPER(r, cas->value, new_cas->value, out, out_length);
		CALL_COMPRESS_HELPER(r, cas->branches, new_cas->branches, out, out_length);
		CALL_COMPRESS_HELPER(r, cas->optional_else, new_cas->optional_else, out, out_length);
		break;
	case cas_branch_STATEMENT:;
		SqlCaseBranchStatement *start_cas_branch, *cur_cas_branch, *new_cas_branch;

		UNPACK_SQL_STATEMENT(start_cas_branch, stmt, cas_branch);
		cur_cas_branch = start_cas_branch;
		do {
			if (NULL != out) {
				new_cas_branch = ((void *)&out[*out_length]);
				memcpy(new_cas_branch, cur_cas_branch, sizeof(SqlCaseBranchStatement));
				new_cas_branch->next = new_cas_branch->prev = NULL;
			}
			*out_length += sizeof(SqlCaseBranchStatement);
			CALL_COMPRESS_HELPER(r, cur_cas_branch->condition, new_cas_branch->condition, out, out_length);
			CALL_COMPRESS_HELPER(r, cur_cas_branch->value, new_cas_branch->value, out, out_length);
			cur_cas_branch = cur_cas_branch->next;
			if ((NULL != out) && (cur_cas_branch != start_cas_branch)) {
				new_cas_branch->next = ((void *)&out[*out_length]);
				A2R(new_cas_branch->next);
			}
		} while (cur_cas_branch != start_cas_branch);
		break;
	case array_STATEMENT:;
		SqlArray *array, *new_array;
		UNPACK_SQL_STATEMENT(array, stmt, array);
		if (NULL != out) {
			new_array = ((void *)&out[*out_length]);
			memcpy(new_array, array, sizeof(SqlArray));
		}
		*out_length += sizeof(SqlArray);
		CALL_COMPRESS_HELPER(r, array->argument, new_array->argument, out, out_length);
		break;
	/* The below types are not possible currently in a CREATE TABLE/FUNCTION/VIEW definition */
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
	return ret;
}
