/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
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

#define CALL_COMPRESS_HELPER(temp, value, new_value, out, out_length)             \
	{                                                                         \
		(temp) = compress_statement_helper((value), (out), (out_length)); \
		if (NULL != (out)) {                                              \
			(new_value) = (temp);                                     \
			if (NULL != new_value) {                                  \
				A2R((new_value));                                 \
			}                                                         \
		}                                                                 \
	}

void *compress_statement_helper(SqlStatement *stmt, char *out, int *out_length);

void compress_statement(SqlStatement *stmt, char **out, int *out_length) {
	*out_length = 0;
	compress_statement_helper(stmt, NULL, out_length);
	if (0 != *out_length) {
		*out = malloc(*out_length);
		*out_length = 0;
		compress_statement_helper(stmt, *out, out_length);
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
void *compress_statement_helper(SqlStatement *stmt, char *out, int *out_length) {
	SqlColumn *	      cur_column, *start_column, *new_column;
	SqlOptionalKeyword *  start_keyword, *cur_keyword, *new_keyword;
	SqlStatement *	      new_stmt;
	SqlTable *	      table, *new_table;
	SqlFunction *	      function, *new_function;
	SqlParameterTypeList *new_parameter_type_list, *cur_parameter_type_list, *start_parameter_type_list;
	SqlValue *	      value, *new_value;
	int		      len;
	void *		      r, *ret;

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
			new_table = ((void *)&out[*out_length]);
			memcpy(new_table, table, sizeof(SqlTable));
		}
		*out_length += sizeof(SqlTable);
		CALL_COMPRESS_HELPER(r, table->tableName, new_table->tableName, out, out_length);
		CALL_COMPRESS_HELPER(r, table->source, new_table->source, out, out_length);
		CALL_COMPRESS_HELPER(r, table->columns, new_table->columns, out, out_length);
		CALL_COMPRESS_HELPER(r, table->delim, new_table->delim, out, out_length);
		/* table->readwrite is not a pointer value so no need to call CALL_COMPRESS_HELPER on this member */
		/* table->oid is not a pointer value so no need to call CALL_COMPRESS_HELPER on this member */
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
			CALL_COMPRESS_HELPER(r, value->v.coerce_target, new_value->v.coerce_target, out, out_length);
			break;
		case BOOLEAN_VALUE:
		case NUMERIC_LITERAL:
		case INTEGER_LITERAL:
		case STRING_LITERAL:
		case DELIM_VALUE:
		case NUL_VALUE:
		case FUNCTION_NAME:
		case FUNCTION_HASH:
		case COLUMN_REFERENCE:
			len = strlen(value->v.string_literal);
			if (NULL != out) {
				memcpy(&out[*out_length], value->v.string_literal, len);
				new_value->v.string_literal = &out[*out_length];
				A2R(new_value->v.string_literal);
			}
			*out_length += len;
			if (NULL != out) {
				out[*out_length] = '\0';
			}
			*out_length += 1;
			break;
		default:
			assert(FALSE);
			break;
		}
		break;
	case column_STATEMENT:
		UNPACK_SQL_STATEMENT(cur_column, stmt, column);
		start_column = cur_column;
		do {
			if (NULL != out) {
				new_column = ((void *)&out[*out_length]);
				memcpy(new_column, cur_column, sizeof(SqlColumn));
				new_column->next = new_column->prev = NULL;
				new_column->table = (void *)0; /* offset 0 : table is first element in compressed structure
								*            which means a relative offset of 0.
								*/
			}
			*out_length += sizeof(SqlColumn);
			CALL_COMPRESS_HELPER(r, cur_column->columnName, new_column->columnName, out, out_length);
			CALL_COMPRESS_HELPER(r, cur_column->keywords, new_column->keywords, out, out_length);
			/* cur_column->delim can be derived from cur_column->keywords and so does not need to be compressed */
			cur_column = cur_column->next;
			if ((NULL != out) && (cur_column != start_column)) {
				new_column->next = ((void *)&out[*out_length]);
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

	/* The below types are not possible currently in a CREATE TABLE definition */
	case select_STATEMENT:
	case insert_STATEMENT:
	case drop_table_STATEMENT:
	case drop_function_STATEMENT:
	case aggregate_function_STATEMENT:
	case join_STATEMENT:
	case column_list_alias_STATEMENT:
	case column_alias_STATEMENT:
	case table_alias_STATEMENT:
	case set_operation_STATEMENT:
	case begin_STATEMENT:
	case commit_STATEMENT:
	case set_STATEMENT:
	case show_STATEMENT:
	case no_data_STATEMENT:
	case delim_char_list_STATEMENT:
	case index_STATEMENT:
	case join_type_STATEMENT:
	case discard_all_STATEMENT:
	case row_value_STATEMENT:
	case table_value_STATEMENT:
	case array_STATEMENT:
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
