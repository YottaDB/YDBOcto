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

#define MAX_EXPRESSION_LENGTH 255

/**
 * WARNING: caller is responsible for freeing the buffer
 */
char *extract_expression(SqlStatement *stmt)
{
  SqlStatement *calculated;
  SqlBinaryOperation *binary;
  SqlValue *value;
  char *buffer = malloc(MAX_EXPRESSION_LENGTH);
  char *tmp1,  *tmp2, *tmp3;

  switch(stmt->type)
  {
  case SQL_VALUE:
    value = stmt->v.value;
    switch(value->type)
    {
    case STRING_LITERAL:
      snprintf(buffer, MAX_EXPRESSION_LENGTH, "\"%s\"", value->v.string_literal);
      break;
    case NUMBER_LITERAL:
      snprintf(buffer, MAX_EXPRESSION_LENGTH, "%s", value->v.string_literal);
      break;
    case CALCULATED_VALUE:
      calculated = value->v.calculated;
      switch(calculated->type)
      {
      case SQL_VALUE:
        tmp1 = extract_expression(calculated);
        snprintf(buffer, MAX_EXPRESSION_LENGTH, "%s", tmp1);
        free(tmp1);
        break;
      case BINARY_OPERATION:
        binary = calculated->v.binary;
        tmp1 = extract_expression(binary->operands[0]);
        tmp2 = extract_expression(binary->operands[1]);
        switch(binary->operation)
        {
        case ADDITION:
          tmp3 = "+";
          break;
        case SUBTRACTION:
          tmp3 = "-";
          break;
        case DVISION:
          tmp3 = "/";
          break;
        case MULTIPLICATION:
          tmp3 = "*";
          break;
        case CONCAT:
          tmp3 = "_";
          break;
        default:
          assert(0);
        }
        snprintf(buffer, MAX_EXPRESSION_LENGTH, "(%s%s%s)", tmp1, tmp3, tmp2);
        break;
      default:
        assert(0);
      }
      break;
    default:
      assert(0);
    }
    break;
  case BINARY_OPERATION:
    binary = stmt->v.binary;
    tmp1 = extract_expression(binary->operands[0]);
    tmp2 = extract_expression(binary->operands[1]);
    switch(binary->operation)
    {
    case ADDITION:
      tmp3 = "+";
      break;
    case SUBTRACTION:
      tmp3 = "-";
      break;
    case DVISION:
      tmp3 = "/";
      break;
    case MULTIPLICATION:
      tmp3 = "*";
      break;
    case CONCAT:
      tmp3 = "_";
      break;
    default:
      assert(0);
    }
    snprintf(buffer, MAX_EXPRESSION_LENGTH, "(%s%s%s)", tmp1, tmp3, tmp2);
    break;
  default:
    assert(0);
  }
  return buffer;
}

/**
 * Emits M code for retrieving values representing this SELECT statement
 *
 * The emitted code should be formatted as follows:
 *  fprintf(buffer, output, cursor_name)
 */
void emit_select_statement(FILE *output, struct SqlStatement *stmt)
{
  SqlSelectStatement *select;
  SqlColumnList *columns;
  SqlValue *value;
  char *tmp1;

  fprintf(output, " WRITE ");

  assert(stmt && stmt->type == SELECT_STATEMENT);
  select = stmt->v.select;
  for(columns = select->select_list; columns != 0; columns = columns->next) {
    tmp1 = extract_expression(columns->value);
    fprintf(output, "%s,!", tmp1);
    free(tmp1);
  }
}
