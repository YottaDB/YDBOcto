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

#include "octo.h"
#include "octo_types.h"

// Private funcion to simplify logic in main one
int _qualify_column_name(char **column_name, SqlJoin *tables, char *table_name_ptr, int table_name_len, char *column_name_ptr, int column_name_length);

/**
 * Tries to find the column in the list of tables
 * If the name is already qualifies, verifies the table exists.
 *
 * For the case of join tables, searches using the <tableName>.<columnName>
 *  followed by searching without seperating the two parts
 */
int qualify_column_name(char **column_name, SqlJoin *tables) {
	char *column_name_ptr = NULL, *table_name_ptr = NULL;
	int table_name_len = 0, column_name_length = 0;

	// First try searching assuming not a JOIN
	column_name_ptr = *column_name;
	// Verify that this isn't already a long name
	for(; *column_name_ptr != '.' && *column_name_ptr != '\0'; column_name_ptr++) {
		// Empty
	}
	if(*column_name_ptr == '.') {
		table_name_ptr = *column_name;
		table_name_len = column_name_ptr - *column_name;
		column_name_ptr++;
		column_name_length = strlen(column_name_ptr);
	} else {
		column_name_length = column_name_ptr - *column_name;
		column_name_ptr = *column_name;
	}
	if(_qualify_column_name(column_name, tables, table_name_ptr, table_name_len, column_name_ptr, column_name_length) == 0) {
		return 0;
	}
	// Try searching with an unmodified name
	if(_qualify_column_name(column_name, tables, NULL, 0, *column_name, strlen(*column_name)) == 0) {
		return 0;
	}
	ERROR(ERR_UNKNOWN_COLUMN_NAME, *column_name);
	return 1;
}


int _qualify_column_name(char **column_name, SqlJoin *tables, char *table_name_ptr, int table_name_len, char *column_name_ptr, int column_name_length) {
	SqlJoin *cur_join, *start_join;
	SqlColumn *cur_column, *start_column;
	SqlValue *table_column_name, *table_name;
	SqlTable *table;
	char *new_column_name;
	int column_matched = 0;

	cur_join = start_join = tables;
	column_matched = FALSE;
	do {
		UNPACK_SQL_STATEMENT(table, cur_join->value, table);
		UNPACK_SQL_STATEMENT(table_name, table->tableName, value);
		// If we have a matching table (this was already a long name) and this isn't
		//  the right table, continue
		if(table_name_ptr && (strlen(table_name->v.reference) != table_name_len
		                      || strncmp(table_name->v.reference, table_name_ptr, table_name_len) != 0)) {
			cur_join = cur_join->next;
			continue;
		}
		UNPACK_SQL_STATEMENT(start_column, table->columns, column);
		cur_column = start_column;
		do {
			UNPACK_SQL_STATEMENT(table_column_name, cur_column->columnName, value);
			if(strlen(table_column_name->v.reference) == column_name_length
			   && strncmp(table_column_name->v.reference, column_name_ptr, column_name_length) == 0) {
				column_matched = TRUE;
				if(table_name_ptr == NULL) {
					table_name_ptr = table_name->v.reference;
					table_name_len = strlen(table_name_ptr);
					new_column_name = malloc(table_name_len + column_name_length + 3);
					strncpy(new_column_name, table_name_ptr, table_name_len);
					*(new_column_name + table_name_len) = '.';
					strncpy(new_column_name + table_name_len + 1, table_column_name->v.reference, column_name_length + 2);
					free(*column_name);
					(*column_name) = new_column_name;
				}
			}
			if(column_matched)
				break;
			cur_column = cur_column->next;
		} while(cur_column != start_column);
		if(column_matched)
			break;
		cur_join = cur_join->next;
	} while(cur_join != start_join);
	if(!column_matched) {
		return 1;
	}

	return 0;
}
