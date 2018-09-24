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
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

void get_table_parts(SqlTable *table, char **curse, char **start, char **end,
    char **source) {
  SqlValue *value;
  SqlOptionalKeyword *keyword;

  UNPACK_SQL_STATEMENT(keyword, table->curse, keyword);
  UNPACK_SQL_STATEMENT(value, keyword->v, value);
  (*curse) = m_unescape_string(value->v.string_literal);
  UNPACK_SQL_STATEMENT(keyword, table->source, keyword);
  UNPACK_SQL_STATEMENT(value, keyword->v, value);
  (*source) = m_unescape_string(value->v.string_literal);
  UNPACK_SQL_STATEMENT(keyword, table->start, keyword);
  UNPACK_SQL_STATEMENT(value, keyword->v, value);
  (*start) = m_unescape_string(value->v.string_literal);
  UNPACK_SQL_STATEMENT(keyword, table->end, keyword);
  UNPACK_SQL_STATEMENT(value, keyword->v, value);
  (*end) = m_unescape_string(value->v.string_literal);
}
