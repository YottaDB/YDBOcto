/****************************************************************
 *								*
 * Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

#include "parser.h"

#define QUERY_EXCERPT_BASE_LEN 64 // Length of query excerpt, omitting "..." and null terminator
#define QUERY_EXCERPT_MAX      (QUERY_EXCERPT_BASE_LEN + sizeof("...") + sizeof("...") + 1) // Null terminator
#define LINE_LIT	       "LINE: "

/* When running rocto the error must be printed via octo_log,
 * which will log the error locally as well as propagate it
 * to the client. Otherwise print via stderr.
 */
#define PRINT_YYERROR(...)                              \
	if (config->is_rocto) {                         \
		ERROR(CUSTOM_ERROR, ##__VA_ARGS__);     \
	} else {                                        \
		fprintf(stderr, ##__VA_ARGS__);         \
		fprintf(stderr, "\n"); /* formatting */ \
	}

#define SNPRINTF_ERR_BUFF(ERR_PTR, ERR_OUT_LEN, ERR_OUT, ...)                                          \
	{                                                                                              \
		int written;                                                                           \
                                                                                                       \
		do {                                                                                   \
			written = snprintf(ERR_PTR, ERR_OUT_LEN - (ERR_PTR - ERR_OUT), ##__VA_ARGS__); \
			if (written < (ERR_OUT_LEN - (ERR_PTR - ERR_OUT))) {                           \
				ERR_PTR += written;                                                    \
				break;                                                                 \
			} else {                                                                       \
				char *tmp;                                                             \
                                                                                                       \
				ERR_OUT_LEN *= 2;                                                      \
				tmp = (char *)malloc(sizeof(char) * ERR_OUT_LEN);                      \
				memcpy(tmp, ERR_OUT, ERR_PTR - ERR_OUT);                               \
				ERR_PTR = tmp + (ERR_PTR - ERR_OUT);                                   \
				free(ERR_OUT);                                                         \
				ERR_OUT = tmp;                                                         \
			}                                                                              \
		} while (TRUE);                                                                        \
	}

void print_yyloc(YYLTYPE *llocp); /* A helper function internal to this file */

void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, int *plan_id, ParseContext *parse_context, char const *s) {
	UNUSED(plan_id);
	UNUSED(parse_context);
	if ((NULL == scan) && (NULL != out)) {
		/* This is a "yyerror" call from outside the parser (e.g. "populate_data_type.c").
		 * In this case, compute "llocp" from "out".
		 */
		SqlSetOperation *set_operation;
		SqlStatement *	 sql_stmt, *stmt;

		assert(NULL == llocp);
		stmt = *out;
		switch (stmt->type) {
		case set_operation_STATEMENT:
		case table_alias_STATEMENT:
			if (set_operation_STATEMENT == stmt->type) {
				UNPACK_SQL_STATEMENT(set_operation, stmt, set_operation);
				sql_stmt = drill_to_table_alias(set_operation->operand[0]);
			} else {
				sql_stmt = stmt;
			}
			llocp = &sql_stmt->loc;
			break;
		default:
			llocp = &stmt->loc;
			break;
		}
	}
	if (llocp->first_line || llocp->first_column || llocp->last_column) {
		print_yyloc(llocp);
		if (NULL != s) {
			PRINT_YYERROR("%s", s);
		}
	} else {
		assert(NULL == s);
	}
}

void print_yyloc(YYLTYPE *llocp) {
	int   cur_line, cur_column, err_out_len;
	int   prefix_len, line_len, highlight_len, i, newline_offset;
	char *c, *line_begin, *line_end, old_terminator, placeholder;
	char *highlight_begin, *highlight_end, *excerpt_begin, *excerpt_end;
	char *left_ellipsis, *right_ellipsis;
	char *err_out, *err_ptr;
	char  prefix[sizeof(LINE_LIT) + INT32_TO_STRING_MAX];

	// Initialize variables used throughout print_yyloc
	cur_line = 0;	// llocp is 0 based
	cur_column = 0; // llocp is 0 based
	// Allocate error buffer
	err_out_len = YDB_MAX_ERRORMSG + 1;
	err_out = malloc(err_out_len);
	err_ptr = err_out;

	/* Find the beginning of the query. Start at the beginning of the input buffer
	 * and iterate up to the previous input index, `old_input_index`, which is set
	 * to the index where the previous query ended.
	 *
	 * If a newline is found before `old_input_index`, the previous query was a multiline
	 * query. In that case, move the start of the string to the start of the new line
	 * to account for the characters from previous lines that are
	 * now ignored.
	 */
	line_begin = input_buffer_combined;
	c = line_begin;
	for (; ('\0' != *c) && (cur_column < old_input_index); c++, cur_column++) {
		if ('\n' == *c) {
			cur_line++;
			line_begin = c + 1;
		}
	}
	/* Newlines before the start of the query artificially increase the column number by 1. In that case, the additional column
	 * must be ignored during later string manipulations to prevent off-by-one issues in output strings. So, check if there
	 * were any newlines, i.e. cur_line > 0, and if so set the newline offset to 1.
	 */
	newline_offset = ((0 < cur_line) ? 1 : 0);
	/* If the previous query was a multiline query and the current query is on the same line as the last line of the previous
	 * query, then `line_begin` will point to a location in the previous query. In that case, we should
	 * treat the start of the current query as the beginning of the line to prevent including a portion of the previous query in
	 * the syntax excerpt.
	 *
	 * The above condition is signaled by `c > line_begin`. However, this condition is also satisfied if there is whitespace at
	 * the beginning of the line and not a portion of a previous multiline query. So, we must distinguish these two cases by
	 * also checking `cur_line` for a value greater than 0, in which case a portion of the previous query is on the current
	 * line.
	 *
	 * So, when both of these conditions are satisfied, set both `line_begin` and `excerpt_begin` to the value of `c` instead of
	 * overwriting `c` with the value of `line_begin`, which would set the line to start in a previous query.
	 */
	if ((c > line_begin) && (0 < cur_line)) {
		line_begin = c;
	}
	c = line_begin;
	// Reset the line and column number for scan for offending line of multiline query
	cur_line = 0;
	cur_column = 0;
	/* Find the line with the offending syntax so that we know what line number to point to in the error message,
	 * as well as where to begin syntax highlighting.
	 */
	for (; ('\0' != *c) && (cur_line < llocp->first_line); c++) {
		if ('\n' == *c) {
			line_begin = c + 1;
			cur_line++;
		}
	}
	newline_offset = (((0 == newline_offset) && (0 < cur_line)) ? 1 : newline_offset);
	/* Find the next newline or null terminator. This will signal the end of the current line. Store this line terminator for
	 * later as it must be restored at the end so that other commands on the same line will run properly.
	 */
	for (; ('\0' != *c) && ('\n' != *c); c++) {
		// We are just scanning for the end of the line, so do nothing here.
	}
	old_terminator = *c;
	line_end = c;

	// Get start and end points of the highlighted area
	highlight_len = (llocp->last_column - llocp->first_column);
	if (0 == llocp->first_line) {
		// Include any leading spaces if this is the first line of the query
		highlight_begin = &line_begin[llocp->first_column + leading_spaces - newline_offset];
	} else {
		highlight_begin = &line_begin[llocp->first_column - newline_offset];
	}
	highlight_end = highlight_begin + highlight_len;
	// Extract QUERY_EXCERPT_BASE_LEN length excerpt from query for later syntax highlighting
	line_len = line_end - line_begin;
	if (QUERY_EXCERPT_BASE_LEN < line_len) {
		int left_len, right_len, context_len;
		int right_excess, left_excess;

		// Get default length of each side of highlighted section of query
		context_len = (QUERY_EXCERPT_BASE_LEN - highlight_len) / 2;
		// Get initial length of substrings on each side of the highlighted section of query
		left_len = highlight_begin - line_begin;
		right_len = line_end - highlight_end;
		assert(right_len + left_len == line_len - highlight_len);

		/* Check whether either side of the highlighted area is less than the max length permitted, i.e.
		 * `context_len`. If so, there will be extra characters available for use on the opposite side of the
		 * highlighted area. So, check for any extra characters and assign them to the appropriate side of the
		 * context.
		 */
		right_excess = context_len - right_len;
		left_excess = context_len - left_len;
		if (0 < right_excess) {
			left_len = context_len + right_excess;
		} else if (0 < left_excess) {
			right_len = context_len + left_excess;
		} else {
			left_len = right_len = context_len;
		}
		if (QUERY_EXCERPT_BASE_LEN < highlight_len) {
			/* If the highlighted area won't fit in the excerpted area, truncate the highlighted area to fit, placing
			 * the start of the highlighted area at the start of the excerpt.
			 */
			excerpt_begin = highlight_begin;
			excerpt_end = highlight_begin + QUERY_EXCERPT_BASE_LEN;
			highlight_len = QUERY_EXCERPT_BASE_LEN;
		} else {
			// Use the left and right side length assignments to determine the beginning and end of the excerpt buffer
			excerpt_begin = highlight_begin - left_len;
			excerpt_end = highlight_end + right_len;
		}
		placeholder = *excerpt_end;
		*excerpt_end = '\0';
	} else {
		/* Initialize placeholder to prevent clang-tidy warning:
		 *   yyerror.c:warning: 'placeholder' may be used uninitialized in this function [-Wmaybe-uninitialized]
		 */
		placeholder = '\0';
		excerpt_end = line_end;
		excerpt_begin = line_begin;
	}
	/* Print an error message indicating the location of the error before highlighting
	 * the offending syntax inline.
	 */
	*line_end = '\0';
	left_ellipsis = ((excerpt_begin > line_begin) ? "..." : "");
	right_ellipsis = ((excerpt_end < line_end) ? "..." : "");
	prefix_len = snprintf(prefix, sizeof(LINE_LIT) + INT32_TO_STRING_MAX, "LINE %d: ", cur_line + 1); // Line #s are 1-indexed
	if (0 == llocp->first_line) {
		PRINT_YYERROR("%s%s%.*s%s", prefix, left_ellipsis, QUERY_EXCERPT_BASE_LEN, excerpt_begin, right_ellipsis);
	} else {
		PRINT_YYERROR("%s%s%.*s%s", prefix, left_ellipsis, QUERY_EXCERPT_BASE_LEN, excerpt_begin, right_ellipsis);
	}

	// Underline the issue
	c = excerpt_begin;
	while (cur_column < (highlight_begin - excerpt_begin)) {
		if ('\0' != *c) {
			if ('\t' == *c) {
				SNPRINTF_ERR_BUFF(err_ptr, err_out_len, err_out, "\t");
			} else {
				SNPRINTF_ERR_BUFF(err_ptr, err_out_len, err_out, " ");
			}
			c++;
		} else {
			SNPRINTF_ERR_BUFF(err_ptr, err_out_len, err_out, " ");
		}
		cur_column++;
	}
	while (cur_column < ((highlight_begin - excerpt_begin) + prefix_len + (int)strlen(left_ellipsis))) {
		SNPRINTF_ERR_BUFF(err_ptr, err_out_len, err_out, " ");
		if ('\0' != *c) {
			c++;
		}
		cur_column++;
	}
	for (i = 0; i < highlight_len; i++) {
		SNPRINTF_ERR_BUFF(err_ptr, err_out_len, err_out, "^");
	}
	PRINT_YYERROR("%s", err_out);
	free(err_out);
	// Restore line terminator
	*line_end = old_terminator;
	/* Restore character at end of excerpt section, now that we no longer need the excerpt substring.
	 * This is necessary to prevent premature termination of the query string if there are more queries
	 * in the input buffer.
	 */
	if (QUERY_EXCERPT_BASE_LEN < line_len) {
		*excerpt_end = placeholder;
	}
}
