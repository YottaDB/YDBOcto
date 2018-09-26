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
#include "template_strings.h"

/**
 * Rewrites source_select such that the returned table has a single column
 *  which is the key to search for
 */
SqlTable *emit_xref_table(ydb_buffer_t *cursor_global,
    ydb_buffer_t *cursor_exe_global, struct SqlStatement *stmt) {
  FILE *output, *temp_table;
  SqlSelectStatement *select;
  SqlTable *table;
  SqlStatement *result = NULL;
  SqlInsertStatement *insert = NULL;
  int status = 0;
  char *temp_table_buffer, *output_buffer;
  char temp_table_name[MAX_STR_CONST], temp_cursor_name[MAX_STR_CONST];
  size_t temp_table_buffer_size = 0, output_buffer_size = 0;
  ydb_buffer_t schema_global, latest_schema_id;
  ydb_buffer_t m_exe_buffer_value;
  ydb_buffer_t z_status, z_status_value;

  TRACE(ERR_ENTERING_FUNCTION, "emit_xref_table");

  UNPACK_SQL_STATEMENT(select, stmt, select);

  temp_table = open_memstream(&temp_table_buffer, &temp_table_buffer_size);
  YDB_LITERAL_TO_BUFFER("^schema", &schema_global);
  INIT_YDB_BUFFER(&latest_schema_id, MAX_STR_CONST);
  status = ydb_incr_s(&schema_global, 0, NULL, NULL, &latest_schema_id);
  YDB_ERROR_CHECK(status, &z_status, &z_status_value);
  latest_schema_id.buf_addr[latest_schema_id.len_used] = '\0';
  snprintf(temp_table_name, MAX_STR_CONST, "tempTbl%s", latest_schema_id.buf_addr);
  fprintf(temp_table, TEMPLATE_CREATE_XREF_TABLE, temp_table_name);
  fclose(temp_table);

  result = parse_line(temp_table_buffer);
  UNPACK_SQL_STATEMENT(table, result, table);
  if(definedTables == NULL) {
    definedTables = result->v.table;
    dqinit(definedTables);
  } else {
    dqinsert(definedTables, result->v.table);
  }
  free(temp_table_buffer);
  free(result);

  // Populate the table
  temp_table = open_memstream(&temp_table_buffer, &temp_table_buffer_size);
  // The "*" and second temp_table_name are place holders
  fprintf(temp_table, TEMPLATE_INSERT_XREF_TABLE, temp_table_name, "*", temp_table_name);
  fclose(temp_table);

  result = parse_line(temp_table_buffer);
  UNPACK_SQL_STATEMENT(insert, result, insert);

  // Override the insert select part
  cleanup_sql_statement(insert->source);
  insert->source = stmt;
  emit_insert_statement(cursor_global, cursor_exe_global, result);

  TRACE(ERR_LEAVING_FUNCTION, "emit_xref_table");

  return table;
}
