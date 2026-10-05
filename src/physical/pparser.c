/****************************************************************
 *								*
 * Copyright (c) 2019-2026 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>

#include "config.h"
#include "errors.h"
#include "physical-parser.h"

extern struct Expr *parser_value;
extern FILE	   *yyin;

int  print_active;
int  linestart_prefix_firsttime_use;
char linestart_prefix[64];

/* Path of the .ctemplate file being converted, used in the "#line" directives */
char *template_path;

/* The last character written to stdout. A "#line" directive must start on a line of its own. */
int last_char_printed = '\n';

Expr *print_template(Expr *expr, Expr *prev);
void  store_linestart_prefix(char *str);
void  emit(const char *format, ...);
void  emit_line_directive(int line);

/* Converts the .ctemplate file whose path is argv[1] into C on stdout.
 * The generated C carries "#line" directives that credit each piece of it to the template line it came from, so
 * compiler messages, assert() failures and code coverage (gcov, and the Cobertura report built from it) point at the
 * .ctemplate file and not the generated C file.
 */
int main(int argc, char **argv) {
	if (2 != argc) {
		/* src/CMakeLists.txt is the only caller, and always passes the template path */
		fprintf(stderr, "Usage: %s <path of .ctemplate file>\n", argv[0]);
		return 1;
	}
	template_path = argv[1];

	yyin = fopen(template_path, "r");
	if (NULL == yyin) {
		/* The build cannot generate this template's C without the template */
		perror(template_path);
		return 1;
	}

	if (yyparse()) {
		ERROR(ERR_PARSING_COMMAND, "Trouble parsing input");
		return 1;
	}
	fclose(yyin);

	print_active = 0;
	print_template(parser_value, NULL);
	return 0;
}

/* Writes to stdout like printf(), and remembers the last character written */
void emit(const char *format, ...) {
	va_list args, args_copy;
	int	length;
	char   *buffer;

	va_start(args, format);
	va_copy(args_copy, args);
	length = vsnprintf(NULL, 0, format, args);
	va_end(args);

	buffer = malloc(length + 1);
	vsnprintf(buffer, length + 1, format, args_copy);
	va_end(args_copy);

	fputs(buffer, stdout);
	if (0 < length) {
		last_char_printed = buffer[length - 1];
	}
	free(buffer);
}

/* Emits a "#line" directive, so that the C emitted next is credited to the given line of the template */
void emit_line_directive(int line) {
	if ('\n' != last_char_printed) {
		/* The directive must start a line. Ending the current line early is harmless, since it only holds
		 * whitespace or a complete statement at the points this is called from.
		 */
		emit("\n");
		/* The indentation already written belonged to the line just ended, so the next TEMPLATE_SNPRINTF()
		 * needs its own.
		 */
		linestart_prefix_firsttime_use = FALSE;
	}
	emit("#line %d \"%s\"\n", line, template_path);
}

void safe_print_string(char *s) {
	while (*s != '\0') {
		switch (*s) {
		case '\n':
			emit("\\n");
			break;
		case '\\':
			emit("\\\\");
			break;
		case '"':
			emit("\\\"");
			break;
		case '`':
			emit("\\");
			break;
		default:
			emit("%c", *s);
			break;
		}
		s++;
	}
}

void store_linestart_prefix(char *str) {
	int   len;
	char *ptr, *dst;

	len = strlen(str);
	ptr = str + len - 1;
	while ('\n' != *ptr) {
		ptr--;
		if (ptr < str)
			return;
	}
	ptr++; /* go past newline */
	dst = &linestart_prefix[0];
	while ((' ' == *ptr) || ('\t' == *ptr))
		*dst++ = *ptr++;
	*dst = '\0';
	assert(dst < linestart_prefix + sizeof(linestart_prefix));
	linestart_prefix_firsttime_use = TRUE;
	return;
}

char *get_linestart_prefix(void) {
	if (linestart_prefix_firsttime_use) {
		linestart_prefix_firsttime_use = FALSE;
		return linestart_prefix + strlen(linestart_prefix); /* effectively return an empty string */
	}
	return linestart_prefix;
}

Expr *print_template(Expr *expr, Expr *prev) {
	Expr *next, *t;
	char *format = "%s", *c, *rformat = format;
	next = expr->next;
	switch (expr->type) {
	case LITERAL_TYPE:
		if (print_active) {
			safe_print_string(expr->value);
			break;
		}
		if (next == NULL)
			break;
		print_active = 1;
		emit_line_directive(expr->line);
		emit("%sTEMPLATE_SNPRINTF(\"", get_linestart_prefix());
		safe_print_string(expr->value);
		if (next && next->type == VALUE_TYPE) {
			next = print_template(next, expr);
		}
		emit("\"");
		t = expr;
		while (t->next != next) {
			t = t->next;
			if (t == NULL)
				break;
			if (t->type == VALUE_TYPE && t->value) {
				emit(",");
				safe_print_string(t->value);
			}
		}
		emit(");\n");
		print_active = 0;
		break;
	case EXPR_TYPE:
		// Return self so we can finish string literal
		if (print_active)
			return expr;
		emit_line_directive(expr->line);
		emit("%s", expr->value);
		store_linestart_prefix(expr->value);
		break;
	case VALUE_TYPE:
		// If this literal has a "|", the ending is a different format
		//  Use that instead of '%s'
		c = expr->value;
		for (; *c != '\0'; c++) {
			if (*c == '|') {
				*c++ = '\0';
				rformat = c;
			}
			if (rformat != format && (*c == ' ' || *c == '\t' || *c == '\n')) {
				*c = '\0';
				break;
			}
		}
		// If the preceding one was a EXPR_TYPE, there is no
		//  middle literal, so print it
		if (prev && prev->type == EXPR_TYPE && !print_active) {
			emit_line_directive(expr->line);
			emit("%sTEMPLATE_SNPRINTF(\"", get_linestart_prefix());
		}
		emit("%s", rformat);
		if (prev && prev->type == EXPR_TYPE && !print_active)
			emit("\", %s);\n", expr->value);
		break;
	};
	if (next)
		return print_template(next, expr);
	return NULL;
}
