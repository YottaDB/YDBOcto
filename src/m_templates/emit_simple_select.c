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
void emit_simple_select(char *output, const SqlTable *table, const char *column, char *source)
{
	SqlValue *tmp_value;
	SqlColumn *cur_column, *start_column;
	SqlOptionalKeyword *start_keyword, *cur_keyword;
	char *global, *temp;
	const char *c;
	char *delim="|", *piece_string = NULL;
	int piece_number;

	/* Assert that this is a qualified references for this table */
	for(c = column; *c != '.' && *c != '\0'; c++) {
		// Empty
	}
	assert(*c == '.');
	assert(strncmp(table->tableName->v.value->v.reference, column, c - column) == 0);
	column = (c+1);

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
	UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
	cur_keyword = start_keyword;
	do {
		switch(cur_keyword->keyword) {
		case OPTIONAL_EXTRACT:
			UNPACK_SQL_STATEMENT(tmp_value, cur_keyword->v, value);
			temp = m_unescape_string(tmp_value->v.string_literal);
			snprintf(output, MAX_EXPRESSION_LENGTH, "%s", temp);
			free(temp);
			return;
			break;
		case OPTIONAL_PIECE:
			UNPACK_SQL_STATEMENT(tmp_value, cur_keyword->v, value);
			piece_string = m_unescape_string(tmp_value->v.string_literal);
			break;
		case OPTIONAL_SOURCE:
			UNPACK_SQL_STATEMENT(tmp_value, cur_keyword->v, value);
			source = m_unescape_string(tmp_value->v.string_literal);
			break;
		case PRIMARY_KEY:
			snprintf(output, MAX_EXPRESSION_LENGTH, "keys(0)");
			return;
			break;
		case NOT_NULL:
		case UNIQUE_CONSTRAINT:
		case NO_KEYWORD:
			/* These are known states that have nothing to do with rendering the SELECT code */
			break;
		default:
			FATAL(ERR_UNKNOWN_KEYWORD_STATE);
			break;
		}
		cur_keyword = cur_keyword->next;
	} while(cur_keyword != start_keyword);
	if(piece_string)
		snprintf(output, MAX_EXPRESSION_LENGTH, "$PIECE(%s,\"%s\",%s)", source, delim, piece_string);
	else
		snprintf(output, MAX_EXPRESSION_LENGTH, "$PIECE(%s,\"%s\",%d)", source, delim, piece_number);
}
