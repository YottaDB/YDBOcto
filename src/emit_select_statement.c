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
char *extract_expression(SqlStatement *stmt, const SqlTable *table, char *source)
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
        tmp1 = extract_expression(calculated, table, source);
        snprintf(buffer, MAX_EXPRESSION_LENGTH, "%s", tmp1);
        free(tmp1);
        break;
      case binary_STATEMENT:
        binary = calculated->v.binary;
        tmp1 = extract_expression(binary->operands[0], table, source);
        tmp2 = extract_expression(binary->operands[1], table, source);
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
        case BOOLEAN_OR:
          tmp3 = "!";
          break;
        case BOOLEAN_AND:
          tmp3 = "&";
          break;
        case BOOLEAN_IS:
          tmp3 = "_";
          FATAL(ERR_FEATURE_NOT_IMPLEMENTED, "BOOLEAN_IS");
          break;
        case BOOLEAN_EQUALS:
          tmp3 = "=";
          break;
        case BOOLEAN_NOT_EQUALS:
          tmp3 = "'='";
          break;
        case BOOLEAN_LESS_THAN:
          tmp3 = "<";
          break;
        case BOOLEAN_GREATER_THAN:
          tmp3 = ">";
          break;
        case BOOLEAN_LESS_THAN_OR_EQUALS:
          tmp3 = "'>'";
          break;
        case BOOLEAN_GREATER_THAN_OR_EQUALS:
          tmp3 = "'<'";
          break;
        case BOOLEAN_IN:
          tmp3 = "_";
          FATAL(ERR_FEATURE_NOT_IMPLEMENTED, "BOOLEAN_IN");
          break;
        case BOOLEAN_NOT_IN:
          tmp3 = "_";
          FATAL(ERR_FEATURE_NOT_IMPLEMENTED, "BOOLEAN_NOT_IN");
          break;
        default:
          FATAL(ERR_UNKNOWN_KEYWORD_STATE);
        }
        snprintf(buffer, MAX_EXPRESSION_LENGTH, "(%s%s%s)", tmp1, tmp3, tmp2);
        break;
      default:
        FATAL(ERR_UNKNOWN_KEYWORD_STATE);
      }
      break;
    case COLUMN_REFERENCE:
      emit_simple_select(buffer, table, value->v.reference, source);
      break;
    default:
      FATAL(ERR_UNKNOWN_KEYWORD_STATE);
    }
    break;
  case binary_STATEMENT:
    UNPACK_SQL_STATEMENT(binary, stmt, binary);
    binary = stmt->v.binary;
    tmp1 = extract_expression(binary->operands[0], table, source);
    tmp2 = extract_expression(binary->operands[1], table, source);
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
    case BOOLEAN_OR:
      tmp3 = "!";
      break;
    case BOOLEAN_AND:
      tmp3 = "&";
      break;
    case BOOLEAN_IS:
      tmp3 = "_";
      FATAL(ERR_FEATURE_NOT_IMPLEMENTED, "BOOLEAN_IS");
      break;
    case BOOLEAN_EQUALS:
      tmp3 = "=";
      break;
    case BOOLEAN_NOT_EQUALS:
      tmp3 = "'='";
      break;
    case BOOLEAN_LESS_THAN:
      tmp3 = "<";
      break;
    case BOOLEAN_GREATER_THAN:
      tmp3 = ">";
      break;
    case BOOLEAN_LESS_THAN_OR_EQUALS:
      tmp3 = "'>'";
      break;
    case BOOLEAN_GREATER_THAN_OR_EQUALS:
      tmp3 = "'<'";
      break;
    case BOOLEAN_IN:
      tmp3 = "_";
      FATAL(ERR_FEATURE_NOT_IMPLEMENTED, "BOOLEAN_IN");
      break;
    case BOOLEAN_NOT_IN:
      tmp3 = "_";
      FATAL(ERR_FEATURE_NOT_IMPLEMENTED, "BOOLEAN_NOT_IN");
      break;
    default:
      FATAL(ERR_UNKNOWN_KEYWORD_STATE);
    }
    snprintf(buffer, MAX_EXPRESSION_LENGTH, "(%s%s%s)", tmp1, tmp3, tmp2);
    break;
  default:
    FATAL(ERR_UNKNOWN_KEYWORD_STATE);
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
  char *tmp1, *formatted_start, *start, *end, *curse, *source;
  int column_name_length;

  char *m_template = "%s FOR  %s USE:%s $P Q:%s  ";

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
  source = m_unescape_string(tmp_value->v.string_literal);
  UNPACK_SQL_STATEMENT(tmp_value, table->curse->v.keyword->v, value);
  curse = m_unescape_string(tmp_value->v.string_literal);
  UNPACK_SQL_STATEMENT(tmp_value, table->start->v.keyword->v, value);
  formatted_start = malloc(MAX_STR_CONST);
  start = m_unescape_string(tmp_value->v.string_literal);
  snprintf(formatted_start, MAX_STR_CONST, start, "^cursor(0)");
  UNPACK_SQL_STATEMENT(tmp_value, table->end->v.keyword->v, value);
  end = m_unescape_string(tmp_value->v.string_literal);
  fprintf(output, m_template, formatted_start, curse, end, end);

  UNPACK_SQL_STATEMENT(columns, select->select_list, column_list);
  if(select->where_expression) {
    tmp1 = extract_expression(select->where_expression, table, source);
    fprintf(output, "WRITE:%s ", tmp1);
  }
  else
    fprintf(output, "WRITE ");
  if (columns == NULL) {
    /* This was a SELECT * statement; add all columns to a list */
    UNPACK_SQL_STATEMENT(start_column, table->columns, column);
    cur_column = start_column;
    columns = t_columns = (SqlColumnList*)malloc(sizeof(SqlColumnList));
    do {
      SQL_STATEMENT(tmp_statement, value_STATEMENT);
      if(select->select_list == NULL)
        select->select_list = tmp_statement; // setting this allows it to get cleaned later
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
        t_columns->next = NULL;
      }
    } while(cur_column != start_column);
  }
  for(; columns != 0;) {
    tmp1 = extract_expression(columns->value, table, source);
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
  fprintf(output, ",!");
  free(formatted_start);
  free(start);
  free(end);
  free(curse);
  free(source);
}
