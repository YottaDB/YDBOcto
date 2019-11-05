/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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

#include "config.h"
#include "errors.h"
#include "physical-parser.h"

extern struct Expr	*parser_value;

int	print_active;
int	linestart_prefix_firsttime_use;
char	linestart_prefix[64];

Expr	*print_template(Expr *expr, Expr *prev);
void	store_linestart_prefix(char *str);

int main() {
	if(yyparse()) {
		ERROR(ERR_PARSING_COMMAND, "Trouble parsing input");
		return 1;
	}
	print_active = 0;
	print_template(parser_value, NULL);
	return 0;
}

void safe_print_string(char *s) {
	while(*s != '\0') {
		switch(*s) {
		case '\n':
			printf("\\n");
			break;
		case '\\':
			printf("\\\\");
			break;
		case '"':
			printf("\\\"");
			break;
		case '`':
			printf("\\");
			break;
		default:
			printf("%c", *s);
			break;
		}
		s++;
	}
}

void	store_linestart_prefix(char *str)
{
	int	len;
	char 	*ptr, *dst;

	len = strlen(str);
	ptr = str + len - 1;
	while ('\n' != *ptr)
	{
		ptr--;
		if (ptr < str)
			return;
	}
	ptr++;	/* go past newline */
	dst = &linestart_prefix[0];
	while ((' ' == *ptr) || ('\t' == *ptr))
		*dst++ = *ptr++;
	*dst = '\0';
	assert(dst < linestart_prefix + sizeof(linestart_prefix));
	linestart_prefix_firsttime_use = TRUE;
	return;
}

char	*get_linestart_prefix()
{
	if (linestart_prefix_firsttime_use)
	{
		linestart_prefix_firsttime_use = FALSE;
		return linestart_prefix + strlen(linestart_prefix);	/* effectively return an empty string */
	}
	return linestart_prefix;
}

Expr *print_template(Expr *expr, Expr *prev) {
	Expr *next, *t;
	char *format = "%s", *c, *rformat = format;
	next = expr->next;
	switch(expr->type) {
	case LITERAL_TYPE:
		if(print_active) {
			safe_print_string(expr->value);
			break;
		}
		if(next == NULL)
			break;
		print_active = 1;
		printf("%sTEMPLATE_SNPRINTF(\"",get_linestart_prefix());
		safe_print_string(expr->value);
		if(next && next->type == VALUE_TYPE) {
			next = print_template(next, expr);
		}
		printf("\"");
		t = expr;
		while(t->next != next) {
			t = t->next;
			if(t == NULL)
				break;
			if(t->type == VALUE_TYPE && t->value) {
				printf(",");
				safe_print_string(t->value);
			}
		}
		printf(");\n");
		print_active = 0;
		break;
	case EXPR_TYPE:
		// Return self so we can finish string literal
		if(print_active)
			return expr;
		printf("%s", expr->value);
		store_linestart_prefix(expr->value);
		break;
	case VALUE_TYPE:
		// If this literal has a "|", the ending is a different format
		//  Use that instead of '%s'
		c = expr->value;
		for(; *c != '\0'; c++) {
			if(*c == '|') {
				*c++ = '\0';
				rformat = c;
			}
			if(rformat != format &&
			   (*c == ' ' || *c == '\t' || *c == '\n')) {
				*c = '\0';
				break;
			}

		}
		// If the preceding one was a EXPR_TYPE, there is no
		//  middle literal, so print it
		if(prev && prev->type == EXPR_TYPE && !print_active)
			printf("%sTEMPLATE_SNPRINTF(\"",get_linestart_prefix());
		printf("%s", rformat);
		if(prev && prev->type == EXPR_TYPE && !print_active)
			printf("\", %s);\n", expr->value);
		break;
	};
	if(next)
		return print_template(next, expr);
	return NULL;
}
