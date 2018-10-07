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
#include "template_strings.h"

#define SOURCE (1 << 0)
#define CURSE (1 << 1)
#define START (1 << 2)
#define END (1 << 3)
#define DELIM (1 << 4)
#define PACK (1 << 5)
#define UNPACK (1 << 6)

int create_table_defaults(SqlStatement *table_statement, SqlStatement *keywords_statement) {
	SqlTable *table;
	SqlOptionalKeyword *keyword, *cur_keyword, *start_keyword, *t_keyword;
	SqlColumn *pkey, *key_columns[MAX_KEY_COUNT];
	SqlStatement *statement;
	SqlValue *value;
	char buffer[MAX_STR_CONST], buffer2[MAX_STR_CONST], *out_buffer;
	char *buff_ptr;
	size_t str_len;
	int max_key = 0, i;
	unsigned int options = 0;

	assert(keywords_statement != NULL);

	UNPACK_SQL_STATEMENT(start_keyword, keywords_statement, keyword);
	UNPACK_SQL_STATEMENT(table, table_statement, table);

	memset(key_columns, 0, MAX_KEY_COUNT * sizeof(SqlColumn*));
	max_key = get_key_columns(table, key_columns);

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
		case OPTIONAL_PACK:
			assert(0 == (options & PACK));
			options |= PACK;
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->pack = statement;
			break;
		case NO_KEYWORD:
			break;
		default:
			FATAL(ERR_UNKNOWN_KEYWORD_STATE);
			break;
		}
		cur_keyword = cur_keyword->next;
	} while(cur_keyword != start_keyword);
	if(options == (SOURCE | CURSE | START | END | DELIM | PACK))
		return 0;

	pkey = fetch_primary_key_column(table);
	if(pkey == NULL) {
		ERROR(ERR_PRIMARY_KEY_NOT_FOUND, table->tableName->v.value->v.reference);
		return 1;
	}
	assert(pkey != NULL);
	/// TODO: if CURSOR is set, make sure there are no KEY NUMs on the keys
	if(!(options & SOURCE)) {
		UNPACK_SQL_STATEMENT(value, table->tableName, value);
		buff_ptr = buffer;
		buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), "^%s(",
				     value->v.reference);
		for(i = 0; i <= max_key; i++) {
			generate_key_name(buffer2, MAX_STR_CONST, i, table, key_columns);
			if(i != 0)
				buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), ",");
			buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), "%s", buffer2);
		}
		buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), ")");
		*buff_ptr++ = '\0';
		out_buffer = m_escape_string(buffer);
		(keyword) = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
		(keyword)->keyword = OPTIONAL_SOURCE;
		SQL_STATEMENT(keyword->v, value_STATEMENT);
		keyword->v->v.value = (SqlValue*)malloc(sizeof(SqlValue));
		keyword->v->v.value->type = COLUMN_REFERENCE;
		keyword->v->v.value->v.reference = out_buffer;
		dqinit(keyword);
		dqinsert(start_keyword, keyword, t_keyword);
	}
	if(!(options & CURSE)) {
		generate_cursor(buffer, MAX_STR_CONST, table);
		out_buffer = m_escape_string(buffer);
		(keyword) = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
		(keyword)->keyword = OPTIONAL_CURSE;
		SQL_STATEMENT(keyword->v, value_STATEMENT);
		keyword->v->v.value = (SqlValue*)malloc(sizeof(SqlValue));
		keyword->v->v.value->type = COLUMN_REFERENCE;
		keyword->v->v.value->v.reference = out_buffer;
		dqinit(keyword);
		dqinsert(start_keyword, keyword, t_keyword);
	}
	if(!(options & START)) {
		UNPACK_SQL_STATEMENT(value, table->tableName, value);
		buff_ptr = buffer;
		buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), "SET ",
				     value->v.reference);
		for(i = 0; i <= max_key; i++) {
			generate_key_name(buffer2, MAX_STR_CONST, i, table, key_columns);
			if(i != 0)
				buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), ",");
			buff_ptr += snprintf(buff_ptr, MAX_STR_CONST - (buff_ptr - buffer), "%s=\"\"", buffer2);
		}
		*buff_ptr++ = '\0';
		out_buffer = m_escape_string(buffer);
		(keyword) = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
		(keyword)->keyword = OPTIONAL_START;
		SQL_STATEMENT(keyword->v, value_STATEMENT);
		keyword->v->v.value = (SqlValue*)malloc(sizeof(SqlValue));
		keyword->v->v.value->type = COLUMN_REFERENCE;
		keyword->v->v.value->v.reference = out_buffer;
		dqinit(keyword);
		dqinsert(start_keyword, keyword, t_keyword);
	}
	if(!(options & END)) {
		generate_end(buffer, MAX_STR_CONST, table);
		out_buffer = m_escape_string(buffer);
		(keyword) = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
		(keyword)->keyword = OPTIONAL_END;
		SQL_STATEMENT(keyword->v, value_STATEMENT);
		keyword->v->v.value = (SqlValue*)malloc(sizeof(SqlValue));
		keyword->v->v.value->type = COLUMN_REFERENCE;
		keyword->v->v.value->v.reference = out_buffer;
		dqinit(keyword);
		dqinsert(start_keyword, keyword, t_keyword);
	}
	if(!(options & DELIM)) {
		snprintf(buffer, MAX_STR_CONST, TEMPLATE_TABLE_DEFAULT_DELIM);
		str_len = strnlen(buffer, MAX_STR_CONST);
		out_buffer = malloc(str_len + 1);
		strncpy(out_buffer, buffer, str_len);
		out_buffer[str_len] = '\0';
		(keyword) = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
		(keyword)->keyword = OPTIONAL_DELIM;
		SQL_STATEMENT(keyword->v, value_STATEMENT);
		keyword->v->v.value = (SqlValue*)malloc(sizeof(SqlValue));
		keyword->v->v.value->type = STRING_LITERAL;
		keyword->v->v.value->v.reference = out_buffer;
		dqinit(keyword);
		dqinsert(start_keyword, keyword, t_keyword);
	}
	if(!(options & PACK)) {
		snprintf(buffer, MAX_STR_CONST, TEMPLATE_TABLE_DEFAULT_PACK);
		str_len = strnlen(buffer, MAX_STR_CONST);
		out_buffer = malloc(str_len + 1);
		strncpy(out_buffer, buffer, str_len);
		out_buffer[str_len] = '\0';
		(keyword) = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
		(keyword)->keyword = OPTIONAL_PACK;
		SQL_STATEMENT(keyword->v, value_STATEMENT);
		keyword->v->v.value = (SqlValue*)malloc(sizeof(SqlValue));
		keyword->v->v.value->type = COLUMN_REFERENCE;
		keyword->v->v.value->v.reference = out_buffer;
		dqinit(keyword);
		dqinsert(start_keyword, keyword, t_keyword);
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
		case OPTIONAL_PACK:
			if(table->pack != NULL && table->pack->v.keyword == cur_keyword)
				break;
			assert(table->pack == NULL);
			SQL_STATEMENT(statement, keyword_STATEMENT);
			statement->v.keyword = cur_keyword;
			table->pack = statement;
			break;
		default:
			FATAL(ERR_UNKNOWN_KEYWORD_STATE);
			break;
		}
		cur_keyword = cur_keyword->next;
	} while(cur_keyword != start_keyword);
	return 0;
}
