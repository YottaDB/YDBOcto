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
  SqlTable *table;
  char *column_type, *constraint_text, *primary_key_name = 0;
  char buffer[255];
  if(stmt == NULL)
    return;
  table = stmt->v.table;
  assert(table->tableName);
  assert(table->columns);
  fprintf(output, "CREATE TABLE %s (", table->tableName);
  start_column = cur_column = table->columns;
  do {
    assert(cur_column && cur_column->columnName);
    fprintf(output, "%s", cur_column->columnName);
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
    cur_column = cur_column->next;
    if(cur_column->constraints) {
      start_constraint = cur_constraint = cur_column->constraints;
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
    if(start_column != cur_column)
      fprintf(output, ", ");
  } while(start_column != cur_column);
  assert(table->source);
  fprintf(output, ") SOURCE \"%s\";", table->source);
}

void emit_column_specification(FILE *output, SqlColumn *column)
{
  // pass
}
