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

SqlOptionalKeyword *get_keyword(SqlColumn *column, enum OptionalKeyword keyword) {
	SqlOptionalKeyword *cur_keyword, *start_keyword;

	UNPACK_SQL_STATEMENT(start_keyword, column->keywords, keyword);
	cur_keyword = start_keyword;
	do {
		if(cur_keyword->keyword == keyword)
			return cur_keyword;
		cur_keyword = cur_keyword->next;
	} while(cur_keyword != start_keyword);
	return NULL;
}
