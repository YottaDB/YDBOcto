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

int qualify_column_list(SqlColumnList *select_columns, SqlJoin *tables) {
  SqlJoin *cur_join, *start_join;
  SqlColumn *cur_column, *start_column;
  SqlColumnList *cur_column_list, *start_column_list;
  SqlValue *column_name, *table_name;
  SqlTable *table;
  char *column_name_ptr = NULL, *table_name_ptr = NULL, *new_column_name = NULL;
  int table_name_len = 0, column_name_length = 0, column_matched = 0, result = 0;

  cur_column_list = start_column_list = select_columns;
  do {
    result |= qualify_statement(cur_column_list->value, tables);
    cur_column_list = cur_column_list->next;
  } while(cur_column_list != start_column_list);
  return result;
}
