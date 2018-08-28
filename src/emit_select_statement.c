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

/**
 * WARNING: caller is responsible for freeing the buffer
 */
char *extract_expression(SqlStatement *stmt, const SqlTable *table, char *formatted_source_begin, char *formatted_source_end)
{
  SqlStatement *calculated;
  SqlBinaryOperation *binary;
  SqlValue *value;
  char *buffer = malloc(MAX_EXPRESSION_LENGTH);
  char *tmp1,  *tmp2, *tmp3;

  switch(stmt->type)
  {
  case value_STATEMENT:
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
      case value_STATEMENT:
        tmp1 = extract_expression(calculated, table, formatted_source_begin, formatted_source_end);
        snprintf(buffer, MAX_EXPRESSION_LENGTH, "%s", tmp1);
        free(tmp1);
        break;
      case binary_STATEMENT:
        binary = calculated->v.binary;
        tmp1 = extract_expression(binary->operands[0], table, formatted_source_begin, formatted_source_end);
        tmp2 = extract_expression(binary->operands[1], table, formatted_source_begin, formatted_source_end);
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
    case COLUMN_REFERENCE:
      emit_simple_select(buffer, table, value->v.reference, formatted_source_begin, formatted_source_end);
      break;
    default:
      assert(0);
    }
    break;
  case binary_STATEMENT:
    UNPACK_SQL_STATEMENT(binary, stmt, binary);
    binary = stmt->v.binary;
    tmp1 = extract_expression(binary->operands[0], table, formatted_source_begin, formatted_source_end);
    tmp2 = extract_expression(binary->operands[1], table, formatted_source_begin, formatted_source_end);
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
  SqlColumnList *columns, *t_columns;
  SqlValue *value, *tmp_value;
  SqlStatement *tmp_statement;
  SqlTable *table, *cur_table, *start_table;
  SqlColumn *cur_column, *start_column;
  SqlJoin *join;
  char *tmp1, *formatted_source_begin, *formatted_source_end, *key, *source;
  int column_name_length;

  char *m_template = "NEW key SET key=$INCREMENT(%s) ";

  //fprintf(output, " WRITE ");
  assert(stmt && stmt->type == select_STATEMENT);
  UNPACK_SQL_STATEMENT(select, stmt, select);
  UNPACK_SQL_STATEMENT(join, select->table_list, join);
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
  assert(table != NULL);
  assert(table->source->type == keyword_STATEMENT && table->source->v.keyword);
  UNPACK_SQL_STATEMENT(tmp_value, table->source->v.keyword->v, value);
  source = tmp_value->v.string_literal;
  formatted_source_begin = malloc(MAX_STR_CONST);
  formatted_source_end = malloc(MAX_STR_CONST);
  key = malloc(MAX_STR_CONST);
  fprintf(output, m_template, "^cursor(0)");
  extract_key(source, key, formatted_source_begin, formatted_source_end);

  UNPACK_SQL_STATEMENT(columns, select->select_list, column_list);
  fprintf(output, "WRITE:$D(%skey%s) ", formatted_source_begin, formatted_source_end);
  if (columns == NULL) {
    /* This was a SELECT * statement; add all columns to a list */
    UNPACK_SQL_STATEMENT(start_column, table->columns, column);
    cur_column = start_column;
    columns = t_columns = (SqlColumnList*)malloc(sizeof(SqlColumnList));
    do {
      SQL_STATEMENT(tmp_statement, value_STATEMENT);
      tmp_statement->v.value = (SqlValue*)malloc(sizeof(SqlValue));
      tmp_statement->v.value->type = COLUMN_REFERENCE;
      column_name_length = strnlen(cur_column->columnName->v.value->v.reference, MAX_STR_CONST) + 1;
      tmp_statement->v.value->v.reference = malloc(column_name_length);
      strncpy(tmp_statement->v.value->v.reference, cur_column->columnName->v.value->v.reference, column_name_length);
      t_columns->value = tmp_statement;
      cur_column = cur_column->next;
      if(cur_column != start_column) {
        SQL_STATEMENT(tmp_statement, column_list_STATEMENT);
        t_columns->next = tmp_statement;
        t_columns->next->v.column_list = (SqlColumnList*)malloc(sizeof(SqlColumnList));
        t_columns = t_columns->next->v.column_list;
      }
    } while(cur_column != start_column);
  }
  for(; columns != 0;) {
    tmp1 = extract_expression(columns->value, table, formatted_source_begin, formatted_source_end);
    fprintf(output, "%s", tmp1);
    free(tmp1);
    if(columns->next)
    {
      UNPACK_SQL_STATEMENT(columns, columns->next, column_list);
      fprintf(output, "_$C(9)_");
    }
    else
      columns = 0;
  }
  fprintf(output, ",! KILL key");
  free(formatted_source_begin);
  free(formatted_source_end);
  free(key);
}
