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

#include "octo.h"
#include "octo_types.h"

void emit_column_specification(FILE *output, SqlColumn *column);

/**
 * Emits DDL specification for the given table
 */
void emit_create_table(FILE *output, struct SqlStatement *stmt)
{
  SqlColumn *start_column, *cur_column;
  SqlConstraint *start_constraint, *cur_constraint;
  SqlTable *table, *temp;
  SqlValue *value;
  SqlOptionalKeyword *keyword, *cur_keyword, *start_keyword;
  char *column_type, *constraint_text, *primary_key_name = 0, *escaped_string;
  if(stmt == NULL)
    return;
  table = stmt->v.table;
  assert(table->tableName);
  assert(table->columns);
  UNPACK_SQL_STATEMENT(value, table->tableName, value);
  fprintf(output, "CREATE TABLE %s (", value->v.reference);
  UNPACK_SQL_STATEMENT(start_column, table->columns, column);
  cur_column = start_column;
  do {
    assert(cur_column && cur_column->columnName);
    UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
    fprintf(output, "%s", value->v.reference);
    switch(cur_column->type)
    {
    case INTEGER_TYPE:
      fprintf(output, " INTEGER");
      break;
    case CHARACTER_STRING_TYPE:
      // We should determine the actual size based on the constraint
      fprintf(output, " VARCHAR(%d)", 25);
      break;
    default:
      assert(0);
    }
    if(cur_column->constraints) {
      UNPACK_SQL_STATEMENT(start_constraint, cur_column->constraints, constraint);
      cur_constraint = start_constraint;
      do {
        switch(cur_constraint->type)
        {
        case PRIMARY_KEY:
          fprintf(output, " PRIMARY KEY");
          break;
        case NOT_NULL:
          fprintf(output, " NOT NULL");
          break;
        case UNIQUE_CONSTRAINT:
          fprintf(output, " UNIQUE");
          break;
        default:
          assert(0);
        }
        cur_constraint = cur_constraint->next;
      } while(start_constraint != cur_constraint);
    }
    UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
    cur_keyword = start_keyword;
    do {
      switch(cur_keyword->keyword)
      {
      case OPTIONAL_EXTRACT:
        UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
        fprintf(output, " EXTRACT \"%s\"", value->v.reference);
        break;
      case NO_KEYWORD:
        break;
      default:
        assert(0);
        break;
      }
    } while(cur_keyword != start_keyword);
    cur_column = cur_column->next;
    if(start_column != cur_column)
      fprintf(output, ", ");
  } while(start_column != cur_column);
  assert(table->source);
  UNPACK_SQL_STATEMENT(keyword, table->source, keyword);
  UNPACK_SQL_STATEMENT(value, keyword->v, value);
  escaped_string = m_escape_string(value->v.reference);
  fprintf(output, ") SOURCE \"%s\"", value->v.reference);
  free(escaped_string);
  assert(table->curse);
  UNPACK_SQL_STATEMENT(keyword, table->curse, keyword);
  UNPACK_SQL_STATEMENT(value, keyword->v, value);
  escaped_string = m_escape_string(value->v.reference);
  fprintf(output, " CURSE \"%s\"", value->v.reference);
  free(escaped_string);
  assert(table->start);
  UNPACK_SQL_STATEMENT(keyword, table->start, keyword);
  UNPACK_SQL_STATEMENT(value, keyword->v, value);
  escaped_string = m_escape_string(value->v.reference);
  fprintf(output, " START \"%s\"", value->v.reference);
  free(escaped_string);
  assert(table->end);
  UNPACK_SQL_STATEMENT(keyword, table->end, keyword);
  UNPACK_SQL_STATEMENT(value, keyword->v, value);
  escaped_string = m_escape_string(value->v.reference);
  fprintf(output, " END \"%s\"", value->v.reference);
  free(escaped_string);
  assert(table->delim);
  UNPACK_SQL_STATEMENT(keyword, table->delim, keyword);
  UNPACK_SQL_STATEMENT(value, keyword->v, value);
  escaped_string = m_escape_string(value->v.reference);
  fprintf(output, " DELIM \"%s\";", value->v.reference);
  free(escaped_string);
}

void emit_column_specification(FILE *output, SqlColumn *column)
{
  // pass
}
