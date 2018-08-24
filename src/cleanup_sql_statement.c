#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

void cleanup_sql_statement(SqlStatement *stmt)
{
  SqlColumn *cur_column, *start_column;
  SqlConstraint *cur_constraint, *start_constraint;
  if(stmt == NULL)
    return;
  switch(stmt->type)
  {
  case table_STATEMENT:
    /* This type of statement should be coped into the global which
        tracks all tables, and generally should not be deleted. Remove this
        if needed */
    assert(0);
    break;
  case select_STATEMENT:
    if(stmt->v.select) {
      cleanup_sql_statement(stmt->v.select->select_list);
      cleanup_sql_statement(stmt->v.select->table_list);
    }
    free(stmt);
    break;
  case drop_STATEMENT:
    if(stmt->v.drop) {
      cleanup_sql_statement(stmt->v.drop->table_name);
      cleanup_sql_statement(stmt->v.drop->optional_keyword);
    }
    free(stmt);
    break;
  case value_STATEMENT:
    if(stmt->v.value) {
      if(stmt->v.value->type == CALCULATED_VALUE)
        cleanup_sql_statement(stmt->v.value->v.calculated);
      else
        free(stmt->v.value->v.reference);
    }
    free(stmt);
    break;
  case binary_STATEMENT:
    if(stmt->v.binary) {
      cleanup_sql_statement(stmt->v.binary->operands[0]);
      cleanup_sql_statement(stmt->v.binary->operands[1]);
    }
    free(stmt);
    break;
  case unary_STATEMENT:
    if(stmt->v.unary) {
      cleanup_sql_statement(stmt->v.unary->operand);
    }
    free(stmt);
    break;
  case column_list_STATEMENT:
    if(stmt->v.column_list) {
      cleanup_sql_statement(stmt->v.column_list->value);
      cleanup_sql_statement(stmt->v.column_list->next);
    }
    free(stmt);
    break;
  case column_STATEMENT:
    if(stmt->v.column) {
      UNPACK_SQL_STATEMENT(cur_column, stmt, column);
      start_column = cur_column;
      do {
        cleanup_sql_statement(cur_column->columnName);
        cleanup_sql_statement(cur_column->constraints);
        cleanup_sql_statement(cur_column->tableName);
        assert(cur_column->next->prev == cur_column);
        cur_column = cur_column->next;
        free(cur_column->prev);
      } while(cur_column != start_column);
    } else
      free(stmt);
    break;
  case join_STATEMENT:
    if(stmt->v.join) {
      cleanup_sql_statement(stmt->v.join->next);
      cleanup_sql_statement(stmt->v.join->value);
    }
    free(stmt);
    break;
  case data_type_STATEMENT:
    /* No op */
    break;
  case constraint_STATEMENT:
    if(stmt->v.constraint) {
      UNPACK_SQL_STATEMENT(cur_constraint, stmt, constraint);
      start_constraint = cur_constraint;
      do {
        cleanup_sql_statement(stmt->v.constraint->referencesColumn);
        cleanup_sql_statement(stmt->v.constraint->check_constraint_definition);
        assert(cur_constraint->next->prev == cur_constraint);
        cur_constraint = cur_constraint->next;
        free(cur_constraint->prev);
      } while (cur_constraint != start_constraint);
    } else
      free(stmt);
    break;
  case constraint_type_STATEMENT:
    /* No op */
    break;
  case keyword_STATEMENT:
    if(stmt->v.keyword) {
      cleanup_sql_statement(stmt->v.keyword->v);
    }
    break;
  default:
    assert(0);
  }
}
