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

/**
 * Emits DDL specification for the given table
 */
void emit_create_table(FILE *output, struct SqlCreateTableStatement *stmt)
{
  SqlColumn *start_column, *cur_column;
  SqlConstraint *start_constraint, *cur_constraint;
  char *column_type, *constraint_text, *primary_key_name = 0;
  if(stmt == NULL)
    return;
  fprintf(output, "%s {", stmt->tableName);
  if(stmt->columns)
  {
    cur_column = start_column = stmt->columns;
    do {
      if(cur_column != start_column) {
        fprintf(output, "}, {");
      }
      switch (cur_column->type)
      {
      case INTEGER_TYPE:
        column_type = "INTEGER";
        break;
      default:
        assert(0);
      }
      fprintf(output, "%s { TYPE = %s", cur_column->columnName, column_type);
      if(cur_column->constraints) {
        start_constraint = cur_constraint = cur_column->constraints;
        do {
          switch(cur_constraint->type)
          {
          case PRIMARY_KEY:
            constraint_text = "PRIMARY KEY";
            assert(primary_key_name == 0);
            primary_key_name = cur_column->columnName;
            break;
          default:
            assert(0);
          }
          fprintf(output, ", CONSTRAINT %s", constraint_text);
        } while(cur_constraint != start_constraint);
      }
      fprintf(output, "}");
      cur_column = cur_column->next;
    } while (cur_column != start_column);
    fprintf(output, ", ");
  }
  if (primary_key_name == 0) {
    /* No primary key was specified, error out */
    assert(0);
  }
  fprintf(output, "source = ^%s(<%s>) }", stmt->tableName, primary_key_name);
}
