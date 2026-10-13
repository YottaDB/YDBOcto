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
#include <stdlib.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

/* Looks at the literal prefix of a LIKE (or ILIKE if "case_insensitive" is TRUE) pattern, i.e. the text up to the first
 * unescaped '%' or '_' (YDBOcto#1158). The pattern is the "pattern_len" bytes at "pattern" (a bound value is not null
 * terminated).
 *
 * Returns one of the following operations for the caller to use in place of the LIKE or ILIKE.
 *	BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX_ONLY : the prefix is not empty and only '%' follows it (e.g. 'PAT%', 'PAT\%%').
 *	BOOLEAN_EQUALS				 : no wildcard, no escape and (ILIKE) no character with case, so the
 *						   pattern is the whole value (e.g. a bound 'PAT', or ILIKE '12'). As in
 *						   PostgreSQL ("Pattern_Prefix_Exact" in "like_fixed_prefix()"), use "=".
 *						   A LIKE literal never gets here, "regex_has_no_special_characters()"
 *						   already turned it into an EQUALS.
 *	BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX      : LIKE only. The prefix is not empty and more follows it (e.g. 'PAT%ENT%',
 *						   'PAT_'), or there is no wildcard but an escape (e.g. a bound 'PA\%T').
 *	BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX    : ILIKE only. The prefix is not empty and more follows it (e.g. '12A%').
 *	BOOLEAN_REGEX_SENSITIVE_LIKE             : LIKE only. No usable prefix (e.g. '%PAT', '_AT%', '').
 *	BOOLEAN_REGEX_INSENSITIVE_LIKE           : ILIKE only. No usable prefix (e.g. '%PAT', 'pat%').
 *
 * All of these match exactly what the LIKE or ILIKE matches. The prefix ones let "lp_optimize_where_multi_equals_ands_helper()"
 * walk the column's cross reference over the prefix only, instead of checking every row of the table. With ..._PREFIX_ONLY,
 * the walk finds exactly the matching rows, so the LIKE itself is dropped. With the other two, each row found is still
 * checked. A pattern with no wildcard ('PAT') is not ..._PREFIX_ONLY: the walk over 'PAT' also finds 'PATIENT'.
 *
 * ILIKE: the cross reference is in case-sensitive order, so 'pat', 'PAT' and 'Pat' are in different places. A character
 * that has case therefore ends the prefix, as it can match either case: 'pat%' has no prefix, '12A%' has the prefix '12'.
 * This is what PostgreSQL does. Its "like_fixed_prefix()" (src/backend/utils/adt/like_support.c) stops at a "case-varying
 * character (it's sort of a wildcard)", which "pattern_char_isalpha()" decides. In a multi-byte encoding, that treats any
 * byte with the high bit set as having case ("char_is_cased_libc()" in src/backend/utils/adt/pg_locale_libc.c). Octo does
 * the same for both M and UTF-8 mode: an ASCII letter or any byte above 127 ends the prefix. So a prefix only holds
 * characters that are the same in either case, and an ILIKE whose pattern is such a prefix followed by only '%' (e.g.
 * '12%') matches exactly what the LIKE does. It returns BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX_ONLY, so it shares that plan.
 *
 * Note: The escape and case handling below must stay in sync with "likeprefix^%ydboctoplanhelpers" and the escape
 * handling with "pattransform^%ydboctoplanhelpers".
 */
BinaryOperations get_like_prefix_operation_of_pattern(const char *pattern, size_t pattern_len, boolean_t case_insensitive) {
	boolean_t   esc_state, escape_seen, wildcard_seen, prefix_done, only_percent_after_prefix;
	const char *ptr, *ptr_top;
	size_t	    prefix_len;

	esc_state = FALSE;
	escape_seen = FALSE;
	wildcard_seen = FALSE;
	prefix_done = FALSE;
	only_percent_after_prefix = TRUE;
	prefix_len = 0;
	ptr = pattern;
	ptr_top = ptr + pattern_len;
	for (; ptr < ptr_top; ptr++) {
		unsigned char ch;
		boolean_t     is_wildcard;

		ch = (unsigned char)*ptr;
		is_wildcard = FALSE;
		if (esc_state) {
			/* Preceding character was an escape character. This character is a literal. */
			esc_state = FALSE;
		} else if ('\\' == ch) {
			esc_state = TRUE;
			escape_seen = TRUE;
			continue;
		} else if (('%' == ch) || ('_' == ch)) {
			is_wildcard = TRUE;
			wildcard_seen = TRUE;
		}
		if (!prefix_done && !is_wildcard && case_insensitive
		    && ((127 < ch) || (('A' <= ch) && ('Z' >= ch)) || (('a' <= ch) && ('z' >= ch)))) {
			/* ILIKE: a character with case can match either case, so it ends the prefix (see comment above) */
			prefix_done = TRUE;
		}
		if (is_wildcard) {
			prefix_done = TRUE;
		}
		if (!prefix_done) {
			prefix_len++;
		} else if ('%' != ch) {
			/* Something other than '%' after the prefix (the character that ended it counts too, unless it is '%') */
			only_percent_after_prefix = FALSE;
		}
	}

	if (esc_state) {
		/* Dangling escape character at the end of the pattern. This is an error that "regexmatch^%ydboctoplanhelpers"
		 * issues when it checks a row. Keep a plain LIKE so every row is still checked and the error is still issued
		 * regardless of which rows the prefix would have selected.
		 */
		return (case_insensitive ? BOOLEAN_REGEX_INSENSITIVE_LIKE : BOOLEAN_REGEX_SENSITIVE_LIKE);
	}
	if (0 == prefix_len) {
		/* The pattern starts with a wildcard (e.g. '%PAT', '_AT%'), is empty, or (ILIKE) starts with a character that has
		 * case (e.g. 'pat%'). Every value in the cross reference would have to be walked, which is no better than walking
		 * the table. Worse, it would build a cross reference (and its triggers on every later write) for no benefit.
		 * Keep the LIKE as is.
		 */
		return (case_insensitive ? BOOLEAN_REGEX_INSENSITIVE_LIKE : BOOLEAN_REGEX_SENSITIVE_LIKE);
	}
	if (!wildcard_seen && !escape_seen && !prefix_done) {
		/* No wildcard, no escape, and (ILIKE) no character with case: the pattern can only match itself. "=" can look the
		 * value up directly, instead of walking every value that starts with it ('PAT' would also walk 'PATIENT').
		 * An escape is excluded as "=" would compare it literally ('PA\%T' matches 'PA%T').
		 */
		return BOOLEAN_EQUALS;
	}
	if (wildcard_seen && only_percent_after_prefix) {
		/* The prefix followed by only '%'. For ILIKE, the prefix has no character with case, so the ILIKE matches
		 * exactly what the LIKE does.
		 */
		return BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX_ONLY;
	}
	/* More after the prefix, or no wildcard but an escape (e.g. a bound value 'PA\%T': the walk over the prefix 'PA%T'
	 * also finds 'PA%TIENT'). Each row found needs the full check.
	 */
	return (case_insensitive ? BOOLEAN_REGEX_INSENSITIVE_LIKE_PREFIX : BOOLEAN_REGEX_SENSITIVE_LIKE_PREFIX);
}

/* Called while a query is parsed, by "binary_operation_data_type_check()", for a LIKE (or ILIKE if "case_insensitive" is
 * TRUE) with a column on the left. That happens in three places:
 *	- octo, and rocto for a simple query (literal patterns only),
 *	- rocto, for the Parse message of a prepared statement (the values of bind parameters are not known yet),
 *	- rocto, for the Bind message: "get_bind_replan_routine()" parses the query of the prepared statement again, this
 *	  time with the bound values in "parse_context->bind_parm_values".
 * "stmt" is the pattern. It is not known to be free of special characters (a LIKE literal with none is turned into an
 * EQUALS by "regex_has_no_special_characters()"). Returns the operation to use in place of the LIKE or ILIKE (see
 * "get_like_prefix_operation_of_pattern()").
 *
 * For a literal, the pattern becomes a plan parameter, so all queries that differ only in that literal share the plan.
 * The generated M code therefore derives the prefix from the pattern at runtime ("likeprefix^%ydboctoplanhelpers").
 * Deciding the operation here (and not at runtime) puts the shape of the pattern into the plan hash, so 'PAT%' and '%PAT'
 * get different plans.
 *
 * For a bind parameter (e.g. "col LIKE $1" from a JDBC prepared statement), the value is not known at Parse. So the plain
 * LIKE or ILIKE plan (a table scan, which builds no cross reference) is used there, and
 * "parse_context->plan_depends_on_bind_values" is set. At Bind, the bound value is classified here exactly like a
 * literal. The value itself stays a plan parameter, so all values of the same shape share the plan.
 */
BinaryOperations get_like_prefix_operation(SqlStatement *stmt, ParseContext *parse_context, boolean_t case_insensitive) {
	SqlValue	*value;
	BinaryOperations no_prefix_operation;

	no_prefix_operation = (case_insensitive ? BOOLEAN_REGEX_INSENSITIVE_LIKE : BOOLEAN_REGEX_SENSITIVE_LIKE);
	if (value_STATEMENT != stmt->type) {
		/* The pattern is not a literal or a parameter (e.g. a column or an expression). Keep the LIKE as is. */
		return no_prefix_operation;
	}
	value = stmt->v.value;
	if (PARAMETER_VALUE == value->type) {
		BindParmValue *bind_parm_value;
		int	       bind_parm_index;

		if ((NULL == parse_context) || !parse_context->is_extended_query) {
			/* Not a rocto prepared statement, so no Bind message will ever supply a value to plan for */
			return no_prefix_operation;
		}
		if (NULL == parse_context->bind_parm_values) {
			/* Parse time: the plan depends on the value of this parameter. Note that, and use the plan without a
			 * prefix for now.
			 */
			parse_context->plan_depends_on_bind_values = TRUE;
			return no_prefix_operation;
		}
		/* Bind time: classify the bound value. As in PostgreSQL, $n gets the n-th value of the Bind message, whatever
		 * the order of the bind parameters in the query (YDBOcto#1165). The parser already checked that n is from 1 to
		 * INT16_MAX, and "handle_bind()" that the Bind message has one value per parameter ($1 to the highest $n).
		 */
		bind_parm_index = (int)strtol(value->v.string_literal + 1, NULL, 10) - 1; /* + 1 to skip the '$' */
		assert((0 <= bind_parm_index) && (bind_parm_index < parse_context->num_bind_parm_values));
		bind_parm_value = &parse_context->bind_parm_values[bind_parm_index];
		if (NULL == bind_parm_value->value) {
			/* SQL NULL (LIKE NULL matches no row) or a binary format value (whose bytes may not be the text of the
			 * pattern). The plan without a prefix is right for any value and builds no cross reference.
			 */
			return no_prefix_operation;
		}
		return get_like_prefix_operation_of_pattern(bind_parm_value->value, (size_t)bind_parm_value->length,
							    case_insensitive);
	}
	if (!IS_STRING_TYPE(value->type)) {
		/* Not a string literal (e.g. a cast such as 'PAT%'::VARCHAR). Keep the LIKE as is. */
		return no_prefix_operation;
	}
	/* A string literal of a query is a plan parameter, so all queries that differ only in it share the plan. A literal
	 * that is not (e.g. in a CHECK constraint) does not get here, as there the left operand is not a column alias.
	 */
	assert(0 != value->parameter_index);
	return get_like_prefix_operation_of_pattern(value->v.string_literal, strlen(value->v.string_literal), case_insensitive);
}
