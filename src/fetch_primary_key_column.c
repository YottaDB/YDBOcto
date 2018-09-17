#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

SqlColumn *fetch_primary_key_column(const SqlTable *table) {
  SqlColumn *cur_column, *start_column, *ret_column;
  SqlOptionalKeyword *cur_constraint, *start_constraint;

  ret_column = NULL;
  UNPACK_SQL_STATEMENT(start_column, table->columns, column);
  cur_column = start_column;
  do {
    UNPACK_SQL_STATEMENT(start_constraint, cur_column->keywords, keyword);
    cur_constraint = start_constraint;
    do {
      if(cur_constraint->keyword == PRIMARY_KEY) {
        assert(ret_column == NULL); // If this triggers, it means multiple primary keys exist
        ret_column = cur_column;
        break;
      }
      cur_constraint = cur_constraint->next;
    } while(cur_constraint != start_constraint);
    cur_column = cur_column->next;
  } while(cur_column != start_column);
  return ret_column;
}
