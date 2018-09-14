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
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

#define SOURCE 1
#define CURSE 2
#define START 4
#define END 8
#define DELIM 16

void create_table_defaults(SqlStatement *table_statement, SqlStatement *keywords_statement) {
  SqlTable *table;
  SqlOptionalKeyword *keyword, *cur_keyword, *start_keyword;
  SqlColumn *pkey;
  SqlStatement *statement;
  char buffer[MAX_STR_CONST], *out_buffer;
  size_t str_len;
  unsigned int options = 0;

  assert(keywords_statement != NULL);

  UNPACK_SQL_STATEMENT(start_keyword, keywords_statement, keyword);
  UNPACK_SQL_STATEMENT(table, table_statement, table);
  cur_keyword = start_keyword;
  do {
    switch(cur_keyword->keyword) {
    case OPTIONAL_SOURCE:
      assert(0 == (options & SOURCE));
      options |= SOURCE;
      SQL_STATEMENT(statement, keyword_STATEMENT);
      statement->v.keyword = cur_keyword;
      table->source = statement;
      break;
    case OPTIONAL_CURSE:
      assert(0 == (options & CURSE));
      options |= CURSE;
      SQL_STATEMENT(statement, keyword_STATEMENT);
      statement->v.keyword = cur_keyword;
      table->curse = statement;
      break;
    case OPTIONAL_START:
      assert(0 == (options & START));
      options |= START;
      SQL_STATEMENT(statement, keyword_STATEMENT);
      statement->v.keyword = cur_keyword;
      table->start = statement;
      break;
    case OPTIONAL_END:
      assert(0 == (options & END));
      options |= END;
      SQL_STATEMENT(statement, keyword_STATEMENT);
      statement->v.keyword = cur_keyword;
      table->end = statement;
      break;
    case OPTIONAL_DELIM:
      assert(0 == (options & DELIM));
      options |= DELIM;
      SQL_STATEMENT(statement, keyword_STATEMENT);
      statement->v.keyword = cur_keyword;
      table->delim = statement;
      break;
    case NO_KEYWORD:
      break;
    default:
      FATAL(ERR_UNKNOWN_KEYWORD_STATE);
      break;
    }
    cur_keyword = cur_keyword->next;
  } while(cur_keyword != start_keyword);
  if(options == (SOURCE | CURSE | START | END | DELIM))
    return;

  pkey = fetch_primary_key_column(table);
  assert(pkey != NULL);
  if(!(options & SOURCE)) {
    snprintf(buffer, MAX_STR_CONST, "^%s(%s)", table->tableName->v.value->v.reference,
      pkey->columnName->v.value->v.reference);
    str_len = strnlen(buffer, MAX_STR_CONST);
    out_buffer = malloc(str_len + 1);
    strncpy(out_buffer, buffer, str_len);
    out_buffer[str_len] = '\0';
    (keyword) = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
    (keyword)->keyword = OPTIONAL_SOURCE;
    SQL_STATEMENT(keyword->v, value_STATEMENT);
    keyword->v->v.value = (SqlValue*)malloc(sizeof(SqlValue));
    keyword->v->v.value->type = COLUMN_REFERENCE;
    keyword->v->v.value->v.reference = out_buffer;
    dqinsert(start_keyword, keyword);
  }
  if(!(options & CURSE)) {
    snprintf(buffer, MAX_STR_CONST, "SET %s=$O(^%s(%s))",
      pkey->columnName->v.value->v.reference,
      table->tableName->v.value->v.reference,
      pkey->columnName->v.value->v.reference);
    str_len = strnlen(buffer, MAX_STR_CONST);
    out_buffer = malloc(str_len + 1);
    strncpy(out_buffer, buffer, str_len);
    out_buffer[str_len] = '\0';
    (keyword) = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
    (keyword)->keyword = OPTIONAL_CURSE;
    SQL_STATEMENT(keyword->v, value_STATEMENT);
    keyword->v->v.value = (SqlValue*)malloc(sizeof(SqlValue));
    keyword->v->v.value->type = COLUMN_REFERENCE;
    keyword->v->v.value->v.reference = out_buffer;
    dqinsert(start_keyword, keyword);
  }
  if(!(options & START)) {
    snprintf(buffer, MAX_STR_CONST, "SET cursor=\"\"%%s\"\",%s=$P($G(@cursor),\"\"|\"\",1)",
      pkey->columnName->v.value->v.reference);
    str_len = strnlen(buffer, MAX_STR_CONST);
    out_buffer = malloc(str_len + 1);
    strncpy(out_buffer, buffer, str_len);
    out_buffer[str_len] = '\0';
    (keyword) = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
    (keyword)->keyword = OPTIONAL_START;
    SQL_STATEMENT(keyword->v, value_STATEMENT);
    keyword->v->v.value = (SqlValue*)malloc(sizeof(SqlValue));
    keyword->v->v.value->type = COLUMN_REFERENCE;
    keyword->v->v.value->v.reference = out_buffer;
    dqinsert(start_keyword, keyword);
  }
  if(!(options & END)) {
    snprintf(buffer, MAX_STR_CONST, "('%s)",
      pkey->columnName->v.value->v.reference);
    str_len = strnlen(buffer, MAX_STR_CONST);
    out_buffer = malloc(str_len + 1);
    strncpy(out_buffer, buffer, str_len);
    out_buffer[str_len] = '\0';
    (keyword) = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
    (keyword)->keyword = OPTIONAL_END;
    SQL_STATEMENT(keyword->v, value_STATEMENT);
    keyword->v->v.value = (SqlValue*)malloc(sizeof(SqlValue));
    keyword->v->v.value->type = COLUMN_REFERENCE;
    keyword->v->v.value->v.reference = out_buffer;
    dqinsert(start_keyword, keyword);
  }
  if(!(options & DELIM)) {
    snprintf(buffer, MAX_STR_CONST, "|");
    str_len = strnlen(buffer, MAX_STR_CONST);
    out_buffer = malloc(str_len + 1);
    strncpy(out_buffer, buffer, str_len);
    out_buffer[str_len] = '\0';
    (keyword) = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
    (keyword)->keyword = OPTIONAL_DELIM;
    SQL_STATEMENT(keyword->v, value_STATEMENT);
    keyword->v->v.value = (SqlValue*)malloc(sizeof(SqlValue));
    keyword->v->v.value->type = COLUMN_REFERENCE;
    keyword->v->v.value->v.reference = out_buffer;
    dqinsert(start_keyword, keyword);
  }
  cur_keyword = start_keyword;
  do {
    switch(cur_keyword->keyword) {
    case OPTIONAL_SOURCE:
      if(table->source != NULL && table->source->v.keyword == cur_keyword)
        break;
      assert(table->source == NULL);
      SQL_STATEMENT(statement, keyword_STATEMENT);
      statement->v.keyword = cur_keyword;
      table->source = statement;
      break;
    case OPTIONAL_CURSE:
      if(table->curse != NULL && table->curse->v.keyword == cur_keyword)
        break;
      assert(table->curse == NULL);
      SQL_STATEMENT(statement, keyword_STATEMENT);
      statement->v.keyword = cur_keyword;
      table->curse = statement;
      break;
    case OPTIONAL_START:
      if(table->start != NULL && table->start->v.keyword == cur_keyword)
        break;
      assert(table->start == NULL);
      SQL_STATEMENT(statement, keyword_STATEMENT);
      statement->v.keyword = cur_keyword;
      table->start = statement;
      break;
    case OPTIONAL_END:
      if(table->end != NULL && table->end->v.keyword == cur_keyword)
        break;
      assert(table->end == NULL);
      SQL_STATEMENT(statement, keyword_STATEMENT);
      statement->v.keyword = cur_keyword;
      table->end = statement;
      break;
    case OPTIONAL_DELIM:
      if(table->delim != NULL && table->delim->v.keyword == cur_keyword)
        break;
      assert(table->delim == NULL);
      SQL_STATEMENT(statement, keyword_STATEMENT);
      statement->v.keyword = cur_keyword;
      table->delim = statement;
      break;
    case NO_KEYWORD:
      break;
    default:
      FATAL(ERR_UNKNOWN_KEYWORD_STATE);
      break;
    }
    cur_keyword = cur_keyword->next;
  } while(cur_keyword != start_keyword);
}
