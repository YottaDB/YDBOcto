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
#include "octo_types.h"
#include "template_strings.h"

/**
 * Creates a temporary table definition which represents a join of the two tables in question
 *
 * Does not modify the WHERE condition to account for the join type
 *
 */
SqlTable *emit_temporary_join_table(SqlTable *table[], int tableCount)
{
	SqlColumn *cur_column, *start_column;
	SqlColumn *key_columns[MAX_KEY_COUNT];
	SqlOptionalKeyword *keyword;
	SqlValue *value;
	SqlStatement *result;
	SqlTable *ret_table;
	int i, primary_key_seen = 0, max_key_num = 1, tableOneKeyEnd = 0;
	char buffer[MAX_STR_CONST], *buff_ptr = buffer;
	char buffer2[MAX_STR_CONST], buffer3[MAX_STR_CONST];
	char *key_names[MAX_KEY_COUNT];
	char *source_global, *temp;
	char *tableName, *columnName, *tableName1, *tableName2;

	assert(tableCount == 2);
	memset(key_columns, 0, sizeof(SqlColumn*) * MAX_KEY_COUNT);

	TRACE(ERR_ENTERING_FUNCTION, "emit_temporary_join_table");
	/// TODO: we should have a loop here to add table names to the temp name
	UNPACK_SQL_STATEMENT(value, table[0]->tableName, value);
	tableName1 = value->v.reference;
	UNPACK_SQL_STATEMENT(value, table[1]->tableName, value);
	tableName2 = value->v.reference;

	buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), "CREATE TABLE tempJoinTable%s%s (", tableName1, tableName2);

	// Add in all columns from tables
	for(i = 0; i < tableCount; i++) {
		if(i != 0) {
			buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), ", ");
		}
		UNPACK_SQL_STATEMENT(value, table[i]->tableName, value);
		tableName = value->v.reference;
		assert(table[i]->source != NULL);
		UNPACK_SQL_STATEMENT(value, table[i]->source->v.keyword->v, value);
		source_global = m_unescape_string(value->v.string_literal);
		UNPACK_SQL_STATEMENT(start_column, table[i]->columns, column);
		cur_column = start_column;
		do {
			keyword = get_keyword(cur_column, PRIMARY_KEY);
			if(keyword != NULL) {
				// Only one primary key is allowed; convert the others to a key num
				if(primary_key_seen) {
					keyword->keyword = OPTIONAL_KEY_NUM;
					keyword->v = (SqlStatement*)malloc(sizeof(SqlStatement));
					keyword->v->type = value_STATEMENT;
					MALLOC_STATEMENT(keyword->v, value, SqlValue);
					// This value gets set below
				} else {
					primary_key_seen = 1;
					key_columns[0] = cur_column;
					key_names[0] = malloc(MAX_STR_CONST);
					generate_key_name(key_names[0], MAX_STR_CONST, 0, table[i], key_columns);
				}
			}
			keyword = get_keyword(cur_column, OPTIONAL_KEY_NUM);
			if(keyword != NULL) {
				// Make sure this value is set to the next key num
				snprintf(buffer2, MAX_STR_CONST, "%d", max_key_num);
				if(keyword->v->v.value->v.string_literal != NULL)
					free(keyword->v->v.value->v.string_literal);
				keyword->v->v.value->v.string_literal = malloc(strlen(buffer2));
				// we know buffer2 is null terminated here because we set it above
				strcpy(keyword->v->v.value->v.string_literal, buffer2);
				key_columns[max_key_num] = cur_column;
				key_names[max_key_num] = malloc(MAX_STR_CONST);
				generate_key_name(key_names[max_key_num], MAX_STR_CONST, max_key_num, table[i], key_columns);
				max_key_num++;

			}
			SAFE_SNPRINTF(buff_ptr, buffer, MAX_STR_CONST, "%s.", tableName);
			buff_ptr += emit_column_specification(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), cur_column);
			// Force the SOURCE to be set so this table is a NOOP
			keyword = get_keyword(cur_column, OPTIONAL_EXTRACT);
			if(keyword == NULL) {
				UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
				columnName = value->v.reference;
				snprintf(buffer3, MAX_STR_CONST, "%s.%s", tableName, columnName);
				emit_simple_select(buffer2, table[i], buffer3, source_global);
				temp = m_escape_string(buffer2);
				buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), " EXTRACT \"%s\"", temp);
				free(temp);
			}
			// Set the ADVANCE so it is propegated to future tables
			keyword = get_keyword(cur_column, PRIMARY_KEY);
			if(keyword == NULL)
				keyword = get_keyword(cur_column, OPTIONAL_KEY_NUM);
			if(keyword != NULL) {
				keyword = get_keyword(cur_column, OPTIONAL_ADVANCE);
				if(keyword == NULL) {
					get_advance(buffer2, MAX_STR_CONST, cur_column, key_columns + tableOneKeyEnd, key_names + tableOneKeyEnd, table[i]);
					temp = m_escape_string(buffer2);
					buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), " ADVANCE \"%s\"", temp);
					free(temp);
				}
			}
			cur_column = cur_column->next;
			if(start_column != cur_column)
				buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), ", ");
		} while(cur_column != start_column);
		tableOneKeyEnd = max_key_num;
	}

	buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), ");");

	TRACE(ERR_GENERATING_TEMPORARY_TABLE, buffer);

	// Combine the ADVANCE keys; note that global should be a NOOP
	result = parse_line(buffer);
	assert(result != NULL);

	UNPACK_SQL_STATEMENT(ret_table, result, table);

	TRACE(ERR_LEAVING_FUNCTION, "emit_temporary_join_table");
	return ret_table;
}
