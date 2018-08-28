#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

/**
 * Returns a buffer containing some M code which can be used to retrieve columns from
 *  the specified global
 *
 * The global should be in the form of ^someGlobal(<columnName>)
 *
 * WARNING: caller is responsible for freeing the buffer
 */
void emit_simple_select(char *output, const SqlTable *table, const char *column, char *formatted_source_begin, char *formatted_source_end)
{
  SqlValue *tmp_value;
  SqlColumn *cur_column, *start_column;
  char *global;
  char *delim="|";
  int piece_number;

  //char *m_template = "NEW temporaryVar,key SET temporaryVar=$INCREMENT(%s),key=temporaryVar";

  assert(table != NULL && table->source != NULL);

  piece_number = 1;
  UNPACK_SQL_STATEMENT(start_column, table->columns, column);
  cur_column = start_column;
  do {
    UNPACK_SQL_STATEMENT(tmp_value, cur_column->columnName, value);
    if(strcmp(column, tmp_value->v.reference) == 0)
      break;
    piece_number++;
    cur_column = cur_column->next;
  } while(cur_column != start_column);
  snprintf(output, MAX_EXPRESSION_LENGTH, "$PIECE(%skey%s,\"%s\",%d)", formatted_source_begin, formatted_source_end, delim, piece_number);
}

void extract_key(const char *source, char *key, char *formatted_source_begin, char *formatted_source_end)
{
  const char *in;
  char *out;
  int state = 0; // 0 = parsing start, 1 = parsing key, 2 = done

  for(in = source, out = formatted_source_begin; *in != '\0'; in++) {
    switch(state)
    {
    case 0:
      if(*in != '<') {
        *out++ = *in;
      } else
      {
        state = 1;
        *out = '\0';
        out = key;
      }
      break;
    case 1:
      if(*in != '>') {
        *out++ = *in;
      } else
      {
        state = 2;
        out = formatted_source_end;
      }
      break;
    case 2:
      assert(*in != '<');
      *out++ = *in;
      break;
    default:
      assert(0);
    }
  }
  *out = '\0';
}
