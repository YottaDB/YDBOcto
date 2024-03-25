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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

// Emits DDL specification for the given table
// Args:
//	FILE *output: output file to write DDL to
//	SqlStatement *stmt: a SqlTable type SqlStatement
// Returns:
//	0 for success, 1 for error
int emit_create_table(FILE *output, struct SqlStatement *stmt) {
	int		    status = 0, buffer_size, defn_len = 0;
	SqlColumn	   *start_column, *cur_column;
	SqlTable	   *table;
	SqlValue	   *value;
	SqlOptionalKeyword *keyword;
	char		   *buffer;

	if (NULL == stmt) {
		return 0;
	}
	buffer_size = OCTO_INIT_BUFFER_LEN;
	buffer = (char *)malloc(sizeof(char) * buffer_size);
	table = stmt->v.create_table;
	assert(table->tableName);
	assert(table->columns);
	UNPACK_SQL_STATEMENT(value, table->tableName, value);
	if (value->is_double_quoted) {
		defn_len += fprintf(output, "CREATE TABLE \"%s\" (", value->v.reference);
	} else {
		defn_len += fprintf(output, "CREATE TABLE `%s` (", value->v.reference);
	}
	UNPACK_SQL_STATEMENT(start_column, table->columns, column);
	cur_column = start_column;
	do {
		status = emit_column_specification(&buffer, &buffer_size, cur_column);
		if (0 > status) {
			free(buffer);
			return -1;
		}
		/* Note: status can be 0 if it is a hidden key column in which case nothing would be emitted */
		if (0 < status) {
			defn_len += fprintf(output, "%s", buffer);
		}
		cur_column = cur_column->next;
		if (start_column != cur_column) {
			if (0 < status) {
				defn_len += fprintf(output, ", ");
			}
		}
	} while (start_column != cur_column);
	defn_len += fprintf(output, ")");
	assert(NULL != table->source);
	UNPACK_SQL_STATEMENT(keyword, table->source, keyword);
	UNPACK_SQL_STATEMENT(value, keyword->v, value);
	m_escape_string2(&buffer, &buffer_size, value->v.reference);
	defn_len += fprintf(output, " GLOBAL \"%s\"", buffer);
	if (NULL != table->delim) {
		char ch, *delim;

		defn_len += fprintf(output, " DELIM ");
		UNPACK_SQL_STATEMENT(keyword, table->delim, keyword);
		UNPACK_SQL_STATEMENT(value, keyword->v, value);
		delim = value->v.reference;
		ch = *delim;
		delim++; /* Skip first byte to get actual delimiter */
		assert((DELIM_IS_DOLLAR_CHAR == ch) || (DELIM_IS_LITERAL == ch));
		if (DELIM_IS_LITERAL == ch) {
			m_escape_string2(&buffer, &buffer_size, delim);
			defn_len += fprintf(output, "\"%s\"", buffer);
		} else {
			assert(!MEMCMP_LIT(delim, "$CHAR(")); /* this is added in parser.y */
			delim += sizeof("$CHAR") - 1;	      /* Skip "$CHAR" */
			defn_len += fprintf(output, "%s", delim);
		}
	}
	if (table->readwrite) {
		defn_len += fprintf(output, " READWRITE");
	} else {
		defn_len += fprintf(output, " READONLY");
	}
	if (table->aim_type) {
		UNPACK_SQL_STATEMENT(keyword, table->aim_type, keyword);
		UNPACK_SQL_STATEMENT(value, keyword->v, value);
		m_escape_string2(&buffer, &buffer_size, value->v.reference);
		defn_len += fprintf(output, " AIMTYPE %s", buffer);
	}
	defn_len += fprintf(output, ";");
	free(buffer);
	return defn_len;
}
