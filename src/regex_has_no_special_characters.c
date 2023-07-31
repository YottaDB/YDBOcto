/****************************************************************
 *								*
 * Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"
#include "octo_types.h"

/* Returns :  1 if "stmt" points to a literal that is not a regex (meaning varies depending on LIKE or SIMILAR TO).
 *	     -1 on error.
 *            0 otherwise.
 * If it returns 1, it also modifies "stmt" to point to a version of the literal minus escape sequences (if any were present).
 * If it returns 0 or -1, "stmt" is untouched.
 * Used for optimizing the parse tree.
 */
int regex_has_no_special_characters(SqlStatement *stmt, enum RegexType regex_type, ParseContext *parse_context) {
	boolean_t has_regex_chars;
	boolean_t esc_state;
	char *	  ptr, *ptr_start, *ptr_top, *ptr_minus_escape_start, *ptr_minus_escape;
	size_t	  len;

	if ((value_STATEMENT != stmt->type) || !IS_STRING_TYPE(stmt->v.value->type)) {
		/* Cannot do any checks if the `stmt` is not of STRING type. Return FALSE to indicate it might have special
		 * characters. */
		return 0;
	}
	/* Note: The regex matching logic is currently in $$pattransform^%ydboctoplanhelpers, an M program.
	 * What we want is related but different enough (we don't want a match, just to know if a regex is a regex or not.
	 * Some of the code below (like which characters have special meaning for LIKE and SIMILAR TO) would be duplicated
	 * but it is small enough that it is considered okay for now.
	 */
	has_regex_chars = FALSE;
	esc_state = FALSE;
	ptr_start = ptr = stmt->v.value->v.string_literal;
	len = strlen(ptr);
	ptr_top = ptr + len;
	ptr_minus_escape_start = ptr_minus_escape = NULL;
	assert((REGEX_LIKE == regex_type) || (REGEX_SIMILARTO == regex_type));
	for (; ptr < ptr_top; ptr++) {
		char ch;

		ch = *ptr;
		if (esc_state) {
			/* Preceding character was an escape character. Ignore this character. */
			esc_state = FALSE;
			if (NULL == ptr_minus_escape) {
				/* Allocate a buffer of same length to store the string minus escape characters
				 * in case we need it at the end (if we are returning TRUE).
				 */
				size_t tmplen;

				ptr_minus_escape = (char *)octo_cmalloc(memory_chunks, len + 1);
				ptr_minus_escape_start = ptr_minus_escape;
				assert(ptr >= (ptr_start + 1));
				tmplen = ptr - 1 - ptr_start;
				if (tmplen) {
					memcpy(ptr_minus_escape, ptr_start, tmplen);
					ptr_minus_escape += tmplen;
				}
			}
			*ptr_minus_escape++ = ch;
			continue;
		}
		switch (ch) {
		case '\\':
			assert(!esc_state);
			esc_state = TRUE;
			break;
		/* LIKE operator. The following characters have a special meaning. \ prefix disables that special meaning.
		 * '%'
		 * '_'
		 *
		 * SIMILAR TO : The following characters have a special meaning. \ prefix disables that special meaning.
		 * '%'
		 * '_'
		 * '|'
		 * '*'
		 * '+'
		 * '('
		 * ')'
		 * '['
		 * ']'
		 * '{'
		 * '}'
		 * '?'
		 */
		case '%':
		case '_':
			has_regex_chars = TRUE;
			break;
		case '|':
		case '*':
		case '+':
		case '(':
		case '[':
		case '{':
		case '?':
			/* The below cases imply regex usage too but they are commented out because they require one of
			 * the above case choices to occur before them (e.g. ')' requires '(' before it etc.).
			 * The moment we see the corresponding prior case, we would have set "has_regex_chars" to TRUE
			 * and stopped processing.
			 *
			 * case ']':
			 * case ')':
			 * case '}':
			 *
			 */
			if (REGEX_SIMILARTO == regex_type) {
				has_regex_chars = TRUE;
				break;
			}
			/* Else: LIKE : Fall through as these characters have no special meaning for LIKE. */
			/* intentional fall through */
		default:
			if (NULL != ptr_minus_escape) {
				*ptr_minus_escape++ = ch;
			}
			break;
		}
		if (has_regex_chars) {
			break;
		}
	}
	if (esc_state) {
		/* Dangling escape sequence. Treat it as a regex as this will signal an error later. */
		has_regex_chars = TRUE;
	}
	if (!has_regex_chars) {
		/* About to return TRUE. Check if "stmt" needs to be repointed to a literal minus escape sequences. */
		if (NULL != ptr_minus_escape_start) {
			int status;

			assert(NULL != ptr_minus_escape);
			assert((ptr_minus_escape_start + len) > ptr_minus_escape);
			*ptr_minus_escape = '\0';
			stmt->v.value->v.string_literal = ptr_minus_escape_start;
			/* Update stored parameter corresponding to this literal to reflect the modified version.
			 * It is possible though that this literal does not have a stored parameter (i.e. parameter_index == 0)
			 * in case this literal is for example inside a CHECK constraint. In that case, skip the update.
			 */
			if (0 != stmt->v.value->parameter_index) {
				status = parse_literal_to_parameter(parse_context, stmt->v.value, TRUE);
				if (status) {
					/* Error encountered. Return error to caller so they can do YYABORT as appropriate. */
					return -1;
				}
			}
		}
	}
	return !has_regex_chars;
}
