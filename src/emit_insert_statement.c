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
#include "template_strings.h"

SqlTable *emit_insert_statement(ydb_buffer_t *cursor_global,
    ydb_buffer_t *cursor_exe_global, struct SqlStatement *stmt)
{
  FILE *output, *insert_string;
  char *output_buffer, *insert_string_buffer;
  int status = 0, columnId=2;
  size_t output_buffer_size = 0, insert_string_buffer_size = 0;
  SqlInsertStatement *insert;
  SqlSelectStatement *select;
  SqlColumn *start_column, *cur_column;
  SqlTable *source_table;
  SqlValue *value, *value2;
  char *curse, *start, *end, *source, *formatted_start, temp_cursor_name[MAX_STR_CONST];
  char buffer[MAX_STR_CONST], buffer2[MAX_STR_CONST];
  ydb_buffer_t z_status, z_status_value;
  ydb_buffer_t schema_global, latest_schema_id;
  ydb_buffer_t m_exe_buffer_value;

  TRACE(ERR_ENTERING_FUNCTION, "emit_insert_statement");

  UNPACK_SQL_STATEMENT(insert, stmt, insert);

  if(insert->columns) {
    UNPACK_SQL_STATEMENT(start_column, insert->columns, column);
  }
  else {
    UNPACK_SQL_STATEMENT(start_column, insert->destination->columns, column);
  }

  /* Get the source table to copy from */
  output = open_memstream(&output_buffer, &output_buffer_size);
  insert_string = open_memstream(&insert_string_buffer, &insert_string_buffer_size);

  source_table = emit_select_statement(cursor_global, cursor_exe_global, insert->source, NULL);
  status = ydb_incr_s(cursor_global, 2, cursor_exe_global, NULL, &cursor_exe_global[2]);
  YDB_ERROR_CHECK(status, &z_status, &z_status_value);
  cursor_exe_global[0].buf_addr[cursor_exe_global[0].len_used] = '\0';
  get_table_parts(source_table, &curse, &start, &end, &source);
  formatted_start = malloc(MAX_STR_CONST);
  snprintf(temp_cursor_name, MAX_STR_CONST, "^cursor(%s)", cursor_exe_global[0].buf_addr);
  snprintf(formatted_start, MAX_STR_CONST, start, temp_cursor_name);
  fprintf(output, TEMPLATE_SELECT_BASIC, formatted_start, curse, end, end, end);

  fprintf(insert_string, "SET row=\"\"");
  UNPACK_SQL_STATEMENT(value2, insert->destination->tableName, value);

  cur_column = start_column;
  do {
    UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
    snprintf(buffer2, MAX_STR_CONST, "%s.%s", value2->v.reference, value->v.reference);
    emit_simple_select(buffer, insert->destination, buffer2, "row");
    fprintf(insert_string, ",%s=$P(oldRow,\"|\",%d)", buffer, columnId);
    columnId++;
    cur_column = cur_column->next;
  } while(cur_column != start_column);
  fprintf(insert_string, " SET ^%s(keys(0))=row", value2->v.reference);
  fclose(insert_string);
  fprintf(output, "SET oldRow=%s DO ^INSERT(\"%s\")", source, m_escape_string(insert_string_buffer));
  fclose(output);
  INFO(CUSTOM_ERROR, "Adding EXE to cursor: %s", output_buffer);
  m_exe_buffer_value.buf_addr = output_buffer;
  m_exe_buffer_value.len_used = m_exe_buffer_value.len_alloc = output_buffer_size;
  status = ydb_set_s(cursor_global, 3,
    cursor_exe_global,
    &m_exe_buffer_value);
  YDB_ERROR_CHECK(status, &z_status, &z_status_value);

  TRACE(ERR_LEAVING_FUNCTION, "emit_insert_statement");

  return NULL;
}
