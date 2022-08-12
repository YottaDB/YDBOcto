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

#define CALL_DECOMPRESS_HELPER(value, out, out_length)                       \
	{                                                                    \
		if (value != NULL) {                                         \
			value = R2A(value);                                  \
			decompress_statement_helper(value, out, out_length); \
		}                                                            \
	}

void *decompress_statement_helper(SqlStatement *stmt, char *out, int out_length);

SqlStatement *decompress_statement(char *buffer, int out_length) {
	return (SqlStatement *)decompress_statement_helper((SqlStatement *)buffer, buffer, out_length);
}

/*
 * Returns a pointer to a new memory location for stmt within the out buffer
 *
 * If the out buffer is NULL, doesn't copy the statement, but just counts size
 */
void *decompress_statement_helper(SqlStatement *stmt, char *out, int out_length) {
	SqlTable *	      table;
	SqlColumn *	      cur_column, *start_column;
	SqlValue *	      value;
	SqlOptionalKeyword *  start_keyword, *cur_keyword;
	SqlFunction *	      function;
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
		CALL_DECOMPRESS_HELPER(table->tableName, out, out_length);
		CALL_DECOMPRESS_HELPER(table->source, out, out_length);
		CALL_DECOMPRESS_HELPER(table->columns, out, out_length);
		CALL_DECOMPRESS_HELPER(table->delim, out, out_length);
		CALL_DECOMPRESS_HELPER(table->aim_type, out, out_length);
		/* table->readwrite is not a pointer value so no need to call CALL_DECOMPRESS_HELPER on this member */
		/* table->oid is not a pointer value so no need to call CALL_DECOMPRESS_HELPER on this member */
		/* table->if_not_exists_specified is not a pointer value */
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
		assert(YDB_OK == status);
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
		case STRING_LITERAL:
		case DELIM_VALUE:
		case NUL_VALUE:
		case FUNCTION_NAME:
		case FUNCTION_HASH:
		case COLUMN_REFERENCE:
			value->v.string_literal = R2A(value->v.string_literal);
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
			cur_column->table = (SqlStatement *)out; /* table is first element in compressed structure i.e. "out" */
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
		} else {
			assert(UNIQUE_CONSTRAINT == constraint->type);
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

	/* The below types are not possible currently in a CREATE TABLE definition */
	case select_STATEMENT:
	case insert_STATEMENT:
	case drop_table_STATEMENT:
	case drop_function_STATEMENT:
	case truncate_table_STATEMENT:
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
	return stmt;
}
