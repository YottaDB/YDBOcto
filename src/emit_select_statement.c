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

/**
 * Emits M code for retrieving values representing this SELECT statement
 *
 * @param cursor_exe_global an array of size 3
 * Returns a table describing the temporary table containing the resulting
 *  values
 */
SqlTable *emit_select_statement(ydb_buffer_t *cursor_global,
    ydb_buffer_t *cursor_exe_global, struct SqlStatement *stmt)
{
  FILE *output, *temp_table;
  SqlSelectStatement *select;
  SqlColumnList *columns, *t_columns;
  SqlValue *value, *tmp_value;
  SqlStatement *tmp_statement;
  SqlTable *table, *cur_table, *start_table;
  SqlColumn *cur_column, *start_column;
  SqlStatement *result = 0;
  SqlJoin *join;
  char *temp_table_buffer, *output_buffer;
  size_t temp_table_buffer_size = 0, output_buffer_size = 0;
  char *tmp1, *formatted_start, *start, *end, *curse, *source;
  char temp_table_name[MAX_STR_CONST], temp_cursor_name[MAX_STR_CONST];
  int column_name_length, column_counter = 0, status, temporary_table = 0;
  ydb_buffer_t schema_global, latest_schema_id;
  ydb_buffer_t m_exe_buffer_value;
  ydb_buffer_t z_status, z_status_value;


  YDB_LITERAL_TO_BUFFER("^schema", &schema_global);
  INIT_YDB_BUFFER(&latest_schema_id, MAX_STR_CONST);
  status = ydb_incr_s(&schema_global, 0, NULL, NULL, &latest_schema_id);
  YDB_ERROR_CHECK(status, &z_status, &z_status_value);

  // Create a temporary table
  temp_table = open_memstream(&temp_table_buffer, &temp_table_buffer_size);
  latest_schema_id.buf_addr[latest_schema_id.len_used] = '\0';
  snprintf(temp_table_name, MAX_STR_CONST, "^tempTbl%s", latest_schema_id.buf_addr);
  fprintf(temp_table, TEMPLATE_CREATE_TABLE_START, temp_table_name);

  output = open_memstream(&output_buffer, &output_buffer_size);

  //fprintf(output, " WRITE ");
  assert(stmt && stmt->type == select_STATEMENT);
  UNPACK_SQL_STATEMENT(select, stmt, select);
  UNPACK_SQL_STATEMENT(join, select->table_list, join);
  switch(join->type) {
  case NO_JOIN:
    UNPACK_SQL_STATEMENT(value, join->value, value);
    table = NULL;
    start_table = cur_table = definedTables;
    do {
      UNPACK_SQL_STATEMENT(tmp_value, cur_table->tableName, value);
      if(strcmp(tmp_value->v.reference, value->v.reference) == 0) {
        table = cur_table;
        break;
      }
      cur_table = cur_table->next;
    } while(start_table != cur_table);
    if(table == NULL) {
      ERROR(ERR_UNKNOWN_TABLE, value->v.reference);
      return NULL;
    }
    break;
  case TABLE_SPEC:
    //UNPACK_SQL_STATEMENT(value, join->value, value);
    temporary_table = TRUE;
    table = emit_select_statement(cursor_global, cursor_exe_global, join->value);
    status = ydb_incr_s(cursor_global, 2, cursor_exe_global, NULL, &cursor_exe_global[2]);
    YDB_ERROR_CHECK(status, &z_status, &z_status_value);
    break;
  }
  assert(table != NULL);
  assert(table->source->type == keyword_STATEMENT && table->source->v.keyword);
  UNPACK_SQL_STATEMENT(tmp_value, table->source->v.keyword->v, value);
  source = m_unescape_string(tmp_value->v.string_literal);
  UNPACK_SQL_STATEMENT(tmp_value, table->curse->v.keyword->v, value);
  curse = m_unescape_string(tmp_value->v.string_literal);
  UNPACK_SQL_STATEMENT(tmp_value, table->start->v.keyword->v, value);
  formatted_start = malloc(MAX_STR_CONST);
  start = m_unescape_string(tmp_value->v.string_literal);
  cursor_exe_global[0].buf_addr[cursor_exe_global[0].len_used] = '\0';
  snprintf(temp_cursor_name, MAX_STR_CONST, "^cursor(%s)", cursor_exe_global[0].buf_addr);
  snprintf(formatted_start, MAX_STR_CONST, start, temp_cursor_name);
  UNPACK_SQL_STATEMENT(tmp_value, table->end->v.keyword->v, value);
  end = m_unescape_string(tmp_value->v.string_literal);
  fprintf(output, TEMPLATE_SELECT_BASIC, formatted_start, curse, end, end, end);

  UNPACK_SQL_STATEMENT(columns, select->select_list, column_list);
  if(select->where_expression) {
    tmp1 = extract_expression(select->where_expression, table, source);
    fprintf(output, "SET:%s ", tmp1);
  }
  else
    fprintf(output, "SET ");
  fprintf(output, "%s(rowId)=keys(0)_\"|\"", temp_table_name);
  if (columns == NULL) {
    /* This was a SELECT * statement; add all columns to a list */
    UNPACK_SQL_STATEMENT(start_column, table->columns, column);
    cur_column = start_column;
    columns = t_columns = (SqlColumnList*)malloc(sizeof(SqlColumnList));
    if(temporary_table)
      cur_column = cur_column->next;
    do {
      SQL_STATEMENT(tmp_statement, value_STATEMENT);
      if(select->select_list == NULL)
        assert(FALSE);
      tmp_statement->v.value = (SqlValue*)malloc(sizeof(SqlValue));
      tmp_statement->v.value->type = COLUMN_REFERENCE;
      column_name_length = strnlen(cur_column->columnName->v.value->v.reference, MAX_STR_CONST) + 1;
      tmp_statement->v.value->v.reference = malloc(column_name_length);
      strncpy(tmp_statement->v.value->v.reference, cur_column->columnName->v.value->v.reference, column_name_length);
      t_columns->value = tmp_statement;
      t_columns->next = NULL;
      cur_column = cur_column->next;
      if(cur_column != start_column) {
        SQL_STATEMENT(tmp_statement, column_list_STATEMENT);
        t_columns->next = tmp_statement;
        t_columns->next->v.column_list = (SqlColumnList*)malloc(sizeof(SqlColumnList));
        t_columns = t_columns->next->v.column_list;
        t_columns->next = NULL;
      }
    } while(cur_column != start_column);
  }
  for(; columns != 0;) {
    fprintf(temp_table, TEMPLATE_CREATE_TABLE_COLUMN, column_counter);
    column_counter++;
    tmp1 = extract_expression(columns->value, table, source);
    fprintf(output, "_%s_\"|\"", tmp1);
    free(tmp1);
    if(columns->next)
    {
      UNPACK_SQL_STATEMENT(columns, columns->next, column_list);
    }
    else
      columns = 0;
  }

  fprintf(temp_table, ");");
  fclose(temp_table);
  INFO(ERR_GENERATING_TEMPORARY_TABLE, temp_table_buffer)
  result = parse_line(temp_table_buffer);
  UNPACK_SQL_STATEMENT(table, result, table);
  free(formatted_start);
  free(start);
  free(end);
  free(curse);
  free(source);
  fclose(output);
  INFO(CUSTOM_ERROR, "Adding EXE to cursor: %s", output_buffer);
  m_exe_buffer_value.buf_addr = output_buffer;
  m_exe_buffer_value.len_used = m_exe_buffer_value.len_alloc = output_buffer_size;
  status = ydb_set_s(cursor_global, 3,
    cursor_exe_global,
    &m_exe_buffer_value);
  YDB_ERROR_CHECK(status, &z_status, &z_status_value);
  free(output_buffer);
  return table;
}
