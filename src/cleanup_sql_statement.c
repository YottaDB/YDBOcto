#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

void cleanup_sql_statement(SqlStatement *stmt)
{
  SqlTable *cur_table, *start_table;
  SqlColumn *cur_column, *start_column;
  if(stmt == NULL)
    return;
  switch(stmt->type)
  {
  case table_STATEMENT:
    if(stmt->v.table) {
      cur_table = start_table = stmt->v.table;
      do {
        cleanup_sql_statement(cur_table->tableName);
        cleanup_sql_statement(cur_table->source);
        cleanup_sql_statement(cur_table->columns);
        if(cur_table->next == start_table) {
          free(cur_table);
          cur_table = start_table;
        } else {
          assert(cur_table->next->prev == cur_table);
          cur_table = cur_table->next;
          free(cur_table->prev);
        }
      } while(cur_table != start_table);
    }
    free(stmt);
    break;
  case select_STATEMENT:
    if(stmt->v.select) {
      cleanup_sql_statement(stmt->v.select->select_list);
      cleanup_sql_statement(stmt->v.select->table_list);
      free(stmt->v.select);
    }
    free(stmt);
    break;
  case drop_STATEMENT:
    if(stmt->v.drop) {
      cleanup_sql_statement(stmt->v.drop->table_name);
      cleanup_sql_statement(stmt->v.drop->optional_keyword);
      free(stmt->v.drop);
    }
    free(stmt);
    break;
  case value_STATEMENT:
    if(stmt->v.value) {
      if(stmt->v.value->type == CALCULATED_VALUE)
        cleanup_sql_statement(stmt->v.value->v.calculated);
      else
        free(stmt->v.value->v.reference);
      free(stmt->v.value);
    }
    free(stmt);
    break;
  case binary_STATEMENT:
    if(stmt->v.binary) {
      cleanup_sql_statement(stmt->v.binary->operands[0]);
      cleanup_sql_statement(stmt->v.binary->operands[1]);
      free(stmt->v.binary);
    }
    free(stmt);
    break;
  case unary_STATEMENT:
    if(stmt->v.unary) {
      cleanup_sql_statement(stmt->v.unary->operand);
      free(stmt->v.unary);
    }
    free(stmt);
    break;
  case column_list_STATEMENT:
    if(stmt->v.column_list) {
      cleanup_sql_statement(stmt->v.column_list->value);
      cleanup_sql_statement(stmt->v.column_list->next);
      free(stmt->v.column_list);
    }
    free(stmt);
    break;
  case column_STATEMENT:
    if(stmt->v.column) {
      UNPACK_SQL_STATEMENT(cur_column, stmt, column);
      start_column = cur_column;
      do {
        cleanup_sql_statement(cur_column->columnName);
        cleanup_sql_statement(cur_column->tableName);
        assert(cur_column->next == start_column ||
          cur_column->next->prev == cur_column);
        if(cur_column->next == start_column) {
          free(cur_column);
          cur_column = start_column;
        } else {
          cur_column = cur_column->next;
          free(cur_column->prev);
        }
      } while(cur_column != start_column);
    }
    free(stmt);
    break;
  case join_STATEMENT:
    if(stmt->v.join) {
      cleanup_sql_statement(stmt->v.join->next);
      cleanup_sql_statement(stmt->v.join->value);
      free(stmt->v.join);
    }
    free(stmt);
    break;
  case data_type_STATEMENT:
    /* No op */
    free(stmt);
    break;
  case constraint_type_STATEMENT:
    /* No op */
    free(stmt);
    break;
  case constraint_STATEMENT:
  case keyword_STATEMENT:
    if(stmt->v.keyword) {
      cleanup_sql_statement(stmt->v.keyword->v);
      free(stmt->v.keyword);
    }
    free(stmt);
    break;
  default:
    FATAL(ERR_UNKNOWN_KEYWORD_STATE);
  }
}
