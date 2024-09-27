/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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
		SqlStatement	*sql_stmt, *stmt;

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
		// At the time of this writing `s` is only set to non-NULL value when the bison calls yyerror() directly to notify a
		// parse failure
		if (NULL != s) {
			// This will log locally as well as propagate it to the client
			ERROR(ERR_PARSE_FAILED, s);
		}
		print_yyloc(llocp);
	} else {
		assert(NULL == s);
	}
}

void print_yyloc(YYLTYPE *llocp) {
	int	  cur_line, line_in_file, cur_column, err_out_len, prev_query_len;
	int	  prefix_len, line_len, highlight_len, i, newline_offset;
	char	 *c, *line_end, old_terminator, placeholder;
	char	 *highlight_begin, *highlight_end, *excerpt_begin, *excerpt_end, *prev_query_end;
	char	 *left_ellipsis, *right_ellipsis;
	char	 *err_out, *err_ptr;
	char	  prefix[sizeof(LINE_LIT) + INT32_TO_STRING_MAX];
	boolean_t is_multiquery_line, line_begin_was_reset;

	// Initialize variables used throughout print_yyloc
	// Allocate error buffer
	err_out_len = YDB_MAX_ERRORMSG + 1;
	err_out = malloc(err_out_len);
	err_ptr = err_out;

	/* Find the beginning of the query. If there is no `old_input_line_number`
	 * from a previous pass through this function, start at the beginning of the
	 * input buffer. Otherwise, begin from the start of the line at `old_input_line_begin`.
	 *
	 * In either case, we then iterate up to the previous input index, `old_input_index`,
	 * which is set to the index where the previous query ended.
	 *
	 * If a newline is found before `old_input_index`, the previous query was a multiline
	 * query. In that case, move the start of the string to the start of the new line
	 * to account for the characters from previous lines that are now ignored.
	 */
	assert(0 <= old_input_line_num);
	if (0 == old_input_line_num) { // Initialized by INIT_INPUT_BUFFER() prior to reading query
		old_input_line_begin = input_buffer_combined;
		cur_column = 0; // llocp is 0 based
	} else {
		if (&input_buffer_combined[old_input_index] >= old_input_line_begin) {
			cur_column = old_input_index
				     - (&input_buffer_combined[old_input_index] - old_input_line_begin); // llocp is 0 based
		} else {
			/* There are multiple errors within a single query line, and this error is not the first. In this case, the
			 * previous error will have updated several variables to point to the next query and/or query line. However,
			 * since there is still at least one more error to issue for the current line, these updates need to be
			 * reverted so that the error message points to the current line, not the next one.
			 *
			 * This reversion is necessary because this function does not have access to information about subsequent
			 * errors, and so cannot anticipate the scenario where there are multiple errors within a single query line.
			 * Accordingly, we must wait until the second or later error, if present, and then revert the changes made
			 * blindly on the previous pass through this function.
			 */
			old_input_line_begin = &input_buffer_combined[old_input_index];
			cur_column = old_input_index;
			old_input_line_num = prev_input_line_num;
		}
	}

	/* Check for blank newlines and count them toward the input line number, but don't update `old_input_line_begin` on their
	 * account, since they contain no query input. This is necessary to prevent erroneous omission of query strings in error
	 * messages when:
	 *   1. There are multiple whole queries on a single line (the multiple partial queries case is handled below)
	 *   2. There is one or more error in each of these queries
	 *
	 * Specifically, we need to count the newlines to get the correct input line number where the error occurs for the error
	 * message output. The number of newlines (`old_input_line_num`) is used to determine the start of the previous query, per
	 * the `if (0 == old_input_line_num) ...` block above.
	 *
	 * In most cases, this safe to do naively. However, in the case described above, `old_input_line_num` will be greater than
	 * 0, but there will in fact be 0 input lines, since all the previous lines contained no query input, only newline
	 * characters. Accordingly, we must account for this edge case by NOT counting them toward `old_input_line_begin`, as is
	 * done with other newlines in the loop below.
	 */
	prev_query_end = NULL;
	c = old_input_line_begin;
	is_multiquery_line = FALSE;
	for (; ('\0' != *c) && (cur_column < old_input_index); c++, cur_column++) {
		if ('\n' == *c) {
			old_input_line_num++;
			old_input_line_begin = c + 1;
			is_multiquery_line = FALSE;
		} else if (';' == *c) {
			/* It is safe to treat the most recently encountered `;` character as the end of the previous query, since
			 * `old_input_index` points to the start of the current query. This means that
			 *  1. This loop iterates over characters in a previous query or queries
			 *  2. This loop terminates at the start of the current query.
			 *
			 * Moreover, queries must be terminated either by a `;` or by an EOF. However, in the case of an
			 * EOF, there cannot be an subsequent queries and so this loop would not execute.
			 *
			 * It follows that the `old_input_index` variable checked in the loop condition indexes a string location
			 * that is preceded by a valid query string that ended with a `;` character.
			 *
			 * Consequently, any `;` encountered here must:
			 *   1. Belong to the previous query
			 *   2. Have been handled as a `SEMICOLON` token and not belong to a SQL string literal
			 *
			 * This means that any final `;` encountered in this loop has in fact been parsed by the parser, such that
			 * it has already been used as the terminus of a query. As a result, looking for the last `;` here does not
			 * constitute a parsing operation, but rather the unpacking of implicit information from the parse stage.
			 *
			 * Thus, it is safe to consider such a `;` as the end of any query preceding the one that is currently
			 * erroring out.
			 */
			prev_query_end = c;
			/* If the line number pointed to by `old_input_index` (i.e. `old_input_line_num`) matches that line number
			 * pointed to by the previous value of `old_input_index` (i.e. `prev_input_line_num`), then we know that
			 * these indices point to multiple queries on the same line. So, we note this down here so that we can
			 * correctly set `old_input_line_begin` below.
			 */
			is_multiquery_line = ((old_input_line_num == prev_input_line_num) ? TRUE : FALSE);
		}
	}
	if (prev_query_end < old_input_line_begin) {
		prev_query_end = old_input_line_begin;
	}
	assert(NULL != prev_query_end);
	prev_query_len = prev_query_end - old_input_line_begin;
	prev_query_len += ((0 < prev_query_len) ? 1 : 0); // llocp is 0-indexed
	prev_input_line_num = old_input_line_num;
	/* Store the number of the line in the file where the query is located before resetting cur_line to 0 to get the line number
	 * of the error within the query itself. This will allow the error message to include both the location of the
	 * failing query in the file and of the error within that query, making it easier to identify the precise location of
	 * the error in both multi-line queries and multi-query files.
	 */
	line_in_file = old_input_line_num;
	/* Newlines before the start of the query artificially increase the column number by 1. In that case, the additional column
	 * must be ignored during later string manipulations to prevent off-by-one issues in output strings. So, check if there
	 * were any newlines, i.e. cur_line > 0, and if so set the newline offset to 1.
	 */
	newline_offset = ((0 < old_input_line_num) ? 1 : 0);
	/* If the previous query was a multiline query and the current query is on the same line as the last line of the previous
	 * query, then `line_begin` will point to a location in the previous query. In that case, we should
	 * treat the start of the current query as the beginning of the line to prevent including a portion of the previous query in
	 * the syntax excerpt.
	 *
	 * The above condition is signaled by `c > old_input_line_begin`. However, this condition is also satisfied if there is
	 * whitespace at the beginning of the line and not a portion of a previous multiline query. So, we must distinguish these
	 * two cases by also checking `old_input_line_num` for a value greater than 0, in which case a portion of the previous query
	 * is on the current line.
	 *
	 * So, when both of these conditions are satisfied, set both `old_input_line_begin` and `excerpt_begin` to the value of `c`
	 * instead of overwriting `c` with the value of `old_input_line_begin`, which would set the line to start in a previous
	 * query.
	 *
	 */
	line_begin_was_reset = FALSE;

	if ((c > old_input_line_begin) && ((0 < old_input_line_num) || is_multiquery_line)) {
		old_input_line_begin = c;
		if (0 == old_input_line_num) {
			assert(is_multiquery_line);
			line_begin_was_reset = TRUE;
		}
	}
	c = old_input_line_begin;
	// Reset the line and column number for scan for offending line of multiline query
	cur_line = 0; // Number of lines in multiline query
	cur_column = 0;
	/* Find the line with the offending syntax so that we know what line number to point to in the error message,
	 * as well as where to begin syntax highlighting.
	 */
	for (; ('\0' != *c) && (cur_line < llocp->first_line); c++) {
		if ('\n' == *c) {
			old_input_line_begin = c + 1;
			cur_line++;
		}
	}
	if (old_input_line_begin > prev_query_end) {
		/* Reset the length of the previous query to 0 when the beginning of the offending query line is beyond the end of
		 * the previous query, since in that case none of the contents of that query should be output.
		 */
		if (line_begin_was_reset) {
			prev_query_len = -1;
		} else {
			prev_query_len = 0;
		}
	}
	old_input_line_num += cur_line; // Include the lines from multiline query in total input line number
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
		highlight_begin = &old_input_line_begin[llocp->first_column + leading_spaces - newline_offset];
	} else {
		highlight_begin = &old_input_line_begin[llocp->first_column - newline_offset];
	}
	highlight_end = highlight_begin + highlight_len;
	// Extract QUERY_EXCERPT_BASE_LEN length excerpt from query for later syntax highlighting
	line_len = line_end - old_input_line_begin;
	if (QUERY_EXCERPT_BASE_LEN < line_len) {
		int left_len, right_len, context_len;
		int right_excess, left_excess;

		// Get default length of each side of highlighted section of query
		context_len = (QUERY_EXCERPT_BASE_LEN - highlight_len) / 2;
		// Get initial length of substrings on each side of the highlighted section of query
		left_len = highlight_begin - old_input_line_begin;
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
		excerpt_begin = old_input_line_begin;
	}
	/* Print an error message indicating the location of the error before highlighting
	 * the offending syntax inline.
	 */
	*line_end = '\0';
	left_ellipsis = ((excerpt_begin > old_input_line_begin) ? "..." : "");
	right_ellipsis = ((excerpt_end < line_end) ? "..." : "");
	prefix_len = snprintf(prefix, sizeof(LINE_LIT) + INT32_TO_STRING_MAX, "LINE %d:%d: ", line_in_file + cur_line + 1,
			      cur_line + 1); // Line #s are 1-indexed
	if (0 == llocp->first_line) {
		PRINT_YYERROR("%s%s%.*s%s", prefix, left_ellipsis, QUERY_EXCERPT_BASE_LEN, excerpt_begin, right_ellipsis);
	} else {
		PRINT_YYERROR("%s%s%.*s%s", prefix, left_ellipsis, QUERY_EXCERPT_BASE_LEN, excerpt_begin, right_ellipsis);
	}

	// Underline the issue
	c = excerpt_begin;
	// Add spaces for `LINE %d:%d: ...` part of the error message
	int left_ellipsis_len = (int)strlen(left_ellipsis);
	SNPRINTF_ERR_BUFF(err_ptr, err_out_len, err_out, "%*s", prefix_len + left_ellipsis_len, "");
	// Add spaces for query characters that are not being highlighted
	while (cur_column < (highlight_begin - excerpt_begin) + prev_query_len) {
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
	// Add highlighting characters
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
