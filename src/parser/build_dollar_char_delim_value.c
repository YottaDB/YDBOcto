/****************************************************************
 *								*
 * Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

/* Builds a DELIM value_STATEMENT from a list of integer character codes (a "delim_char_list").
 *
 * The returned value's string is the M expression "$CHAR(c1,c2,...)" prefixed with a one-byte flag
 * (DELIM_IS_DOLLAR_CHAR) that marks this delimiter as a $CHAR expression (as opposed to a plain literal
 * delimiter, which is flagged DELIM_IS_LITERAL). Downstream code (codegen, SHOW) looks at this first byte
 * to decide whether to emit the delimiter verbatim (a $CHAR call) or wrapped in double quotes (a literal).
 *
 * For example the char list "63,64" produces the string:  <DELIM_IS_DOLLAR_CHAR>"$CHAR(63,64)"
 *
 * This builder is shared by:
 *	1) the table/column-level "DELIM (c1,c2,...)" form (which assembles a SINGLE delimiter from the codes), and
 *	2) the chained "DELIMS (.. $C(c1,c2,..) ..)" form (YDBOcto#1108), where one level's delimiter is a $CHAR.
 *
 * Returns the value_STATEMENT on success, or NULL on error (after issuing ERR_TOO_MANY_DELIM_CHARS).
 * Invoked by rules in src/parser.y.
 *
 * A note on the snprintf() calls below: the 2nd argument is the maximum number of bytes snprintf will write
 * INCLUDING the terminating '\0', and the return value ("copied") is the number of bytes written EXCLUDING
 * that '\0'. We advance the write cursor "c" by "copied" each time so the next write begins exactly where the
 * previous text ended (overwriting the '\0', which the next write replaces).
 */
SqlStatement *build_dollar_char_delim_value(SqlStatement *delim_char_list_stmt) {
	SqlStatement		  *char_list_literal;
	SqlDelimiterCharacterList *start_delim_char_list, *cur_delim_char_list;
	char			  *c;	      /* write cursor into "str_lit" (always points at the next free byte) */
	char			  *str_lit;   /* the growing output buffer: flag byte + "$CHAR(...)" + '\0' */
	char			  *temp;      /* holds the old buffer while we copy it into a larger one */
	int			   copied;    /* bytes the most recent snprintf wrote, excluding its '\0' */
	int			   len_used;  /* bytes of "str_lit" used so far (flag byte + text written) */
	int			   len_alloc; /* bytes currently allocated for "str_lit" */
	int			   num_args;  /* number of character codes seen (validated against DOLLAR_CHAR_MAX_ARGS) */

	/* Start with a generously sized buffer; it is grown on demand inside the loop below if a long char list
	 * overruns it. INT8_TO_STRING_MAX is the max width of one decimal char code, so 8x that is plenty to begin.
	 */
	len_alloc = INT8_TO_STRING_MAX * 8;
	str_lit = octo_cmalloc(memory_chunks, len_alloc);

	/* Byte 0 is the flag byte; the actual delimiter text starts at byte 1. */
	str_lit[0] = DELIM_IS_DOLLAR_CHAR;
	len_used = 1;
	SQL_VALUE_STATEMENT(char_list_literal, DELIM_VALUE, str_lit);
	c = &str_lit[1]; /* begin writing the "$CHAR(...)" text just after the flag byte */

	/* Emit the opening "$CHAR(". */
	UNPACK_SQL_STATEMENT(start_delim_char_list, delim_char_list_stmt, delim_char_list);
	cur_delim_char_list = start_delim_char_list;
	copied = snprintf(c, INT16_TO_STRING_MAX, "$CHAR(");
	len_used += copied;
	assert(INT16_TO_STRING_MAX > copied);
	c += copied;

	num_args = 0;
	do {
		/* Grow the buffer if there is not enough room for another char code. We copy the "len_used" bytes
		 * written so far into the larger buffer and re-point the write cursor "c".
		 * Note: it is acceptable here to allocate without freeing the previous buffer, as octo_cmalloc
		 * memory is automatically freed once query execution completes (or is cancelled).
		 */
		if (INT8_TO_STRING_MAX > len_alloc - len_used) {
			len_alloc *= 2;
			temp = str_lit;
			str_lit = octo_cmalloc(memory_chunks, len_alloc);
			memcpy(str_lit, temp, len_used);
			c = str_lit;
			c += len_used; /* re-point cursor to the same logical position in the new buffer */
		}

		/* Write this character code as decimal text, e.g. "63". */
		copied = snprintf(c, INT8_TO_STRING_MAX, "%d", cur_delim_char_list->character);
		assert(INT8_TO_STRING_MAX > copied);
		c += copied;
		len_used += copied;

		cur_delim_char_list = cur_delim_char_list->next;
		if (start_delim_char_list != cur_delim_char_list) {
			/* More codes follow, so append a comma separator. "," plus its '\0' is 2 bytes. */
			copied = snprintf(c, 2, ",");
			assert(2 > copied);
			c += copied;
			len_used += copied;
		}
		num_args++;
	} while (cur_delim_char_list != start_delim_char_list);

	/* $CHAR accepts at most DOLLAR_CHAR_MAX_ARGS arguments; reject lists longer than that. */
	if (DOLLAR_CHAR_MAX_ARGS < num_args) {
		ERROR(ERR_TOO_MANY_DELIM_CHARS, num_args, DOLLAR_CHAR_MAX_ARGS);
		return NULL;
	}

	/* Append the closing ")". ")" plus its '\0' is 2 bytes. */
	copied = snprintf(c, 2, ")");
	assert(2 > copied);
	c += copied;
	*c = '\0';
	char_list_literal->v.value->v.string_literal = str_lit;
	return char_list_literal;
}
