/****************************************************************
 *								*
 * Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	*
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

char *get_type_string_from_sql_data_type(SqlDataType type) {
	switch (type) {
	case BOOLEAN_TYPE:
		return "BOOLEAN";
	case NUMERIC_TYPE:
		return "NUMERIC";
	case INTEGER_TYPE:
		return "INTEGER";
	case STRING_TYPE:
		return "VARCHAR";
	case DATE_TYPE:
		return "DATE";
	case TIME_TYPE:
		return "TIME";
	case TIME_WITH_TIME_ZONE_TYPE:
		return "TIME WITH TIME ZONE";
	case TIMESTAMP_TYPE:
		return "TIMESTAMP";
	case TIMESTAMP_WITH_TIME_ZONE_TYPE:
		return "TIMESTAMP WITH TIME ZONE";
	case UNKNOWN_SqlDataType:
	default:
		assert(FALSE);
		ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
		break;
	}
	return "";
}
// Emits DDL specification for the given function
// Args:
//	FILE *output: output file to write DDL to
//	SqlStatement *stmt: a SqlFunction type SqlStatement
// Returns:
//	0 for success, 1 for error
int emit_create_function(FILE *output, struct SqlStatement *stmt) {
	SqlFunction	     *function;
	SqlParameterTypeList *start_parameter_type, *cur_parameter_type;
	SqlValue	     *function_name;
	SqlDataType	      data_type;
	int		      defn_len = 0;

	if (stmt == NULL)
		return 0;
	function = stmt->v.create_function;
	assert(function->function_name);
	// assert(function->parameter_type_list);
	UNPACK_SQL_STATEMENT(function_name, function->function_name, value);
	if (function_name->is_double_quoted) {
		defn_len += fprintf(output, "CREATE FUNCTION \"%s\"(", function_name->v.string_literal);
	} else {
		defn_len += fprintf(output, "CREATE FUNCTION `%s`(", function_name->v.string_literal);
	}
	if (NULL != function->parameter_type_list) { // Skip parameter types if none were specified
		UNPACK_SQL_STATEMENT(start_parameter_type, function->parameter_type_list, parameter_type_list);
		cur_parameter_type = start_parameter_type;
		do {
			/* Note that size/precision modifiers are discarded for CREATE FUNCTION statements,
			 * per https://www.postgresql.org/docs/current/sql-createfunction.html
			 */
			data_type = cur_parameter_type->data_type_struct->v.data_type_struct.data_type;
			enum OptionalKeyword keyword = cur_parameter_type->data_type_struct->v.data_type_struct.format;
			if (IS_DATE_TIME_DATA_TYPE(data_type) && (OPTIONAL_DATE_TIME_TEXT != keyword)) {
				assert(NO_KEYWORD != keyword);
				switch (data_type) {
				case DATE_TYPE:
				case TIME_TYPE:
				case TIMESTAMP_TYPE:
					defn_len += fprintf(output, " %s(%s)", get_type_string_from_sql_data_type(data_type),
							    get_date_time_format_string(keyword));
					break;
				case TIME_WITH_TIME_ZONE_TYPE:
					defn_len
					    += fprintf(output, " TIME(%s) WITH TIME ZONE", get_date_time_format_string(keyword));
					break;
				case TIMESTAMP_WITH_TIME_ZONE_TYPE:
					defn_len += fprintf(output, " TIMESTAMP(%s) WITH TIME ZONE",
							    get_date_time_format_string(keyword));
					break;
				default:
					assert(FALSE);
					break;
				}
			} else {
				defn_len += fprintf(output, " %s", get_type_string_from_sql_data_type(data_type));
			}
			cur_parameter_type = cur_parameter_type->next;
			if (start_parameter_type != cur_parameter_type)
				defn_len += fprintf(output, ", ");
		} while (start_parameter_type != cur_parameter_type);
	}
	defn_len += fprintf(output, ") RETURNS ");
	data_type = function->return_type->v.data_type_struct.data_type;
	enum OptionalKeyword keyword = function->return_type->v.data_type_struct.format;
	if (IS_DATE_TIME_DATA_TYPE(data_type) && (OPTIONAL_DATE_TIME_TEXT != keyword)) {
		assert(NO_KEYWORD != keyword);
		switch (data_type) {
		case DATE_TYPE:
		case TIME_TYPE:
		case TIMESTAMP_TYPE:
			defn_len += fprintf(output, "%s(%s)", get_type_string_from_sql_data_type(data_type),
					    get_date_time_format_string(keyword));
			break;
		case TIME_WITH_TIME_ZONE_TYPE:
			defn_len += fprintf(output, "TIME(%s) WITH TIME ZONE", get_date_time_format_string(keyword));
			break;
		case TIMESTAMP_WITH_TIME_ZONE_TYPE:
			defn_len += fprintf(output, "TIMESTAMP(%s) WITH TIME ZONE", get_date_time_format_string(keyword));
			break;
		default:
			assert(FALSE);
			break;
		}
	} else {
		defn_len += fprintf(output, "%s", get_type_string_from_sql_data_type(data_type));
	}
	defn_len += fprintf(output, " AS %s", function->extrinsic_function->v.value->v.string_literal);

	defn_len += fprintf(output, ";");
	return defn_len;
}
