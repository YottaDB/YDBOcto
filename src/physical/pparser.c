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

#include <stdio.h>

#include "config.h"
#include "errors.h"
#include "physical-parser.h"

extern struct Expr *parser_value;

Expr *print_template(Expr *expr, Expr *prev);
int print_active;

int main(int argc, char **argv) {
	//yyscan_t scanner;
	//YY_BUFFER_STATE state;
	char *test = "{% int myfunc() { %}ok";

	octo_init();

	//	yydebug = TRUE;

	/*if(yylex_init(&scanner))
		FATAL(ERR_INIT_SCANNER);
		//state = yy_scan_string(test, scanner);*/
	if(yyparse()) {
		ERROR(ERR_PARSING_COMMAND, test);
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
		default:
			printf("%c", *s);
		}
		s++;
	}
}

#define SNPRINT_HEADER \
	"buff_ptr += snprintf(buff_ptr, buffer_len - (buff_ptr - buffer), \""

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
		print_active = 1;
		if(next == NULL)
			return NULL;
		printf(SNPRINT_HEADER);
		safe_print_string(expr->value);
		if(next && next->type == VALUE_TYPE)
			next = print_template(next, expr);
		printf("\"");
		t = expr;
		if(t->next && t->next->type == VALUE_TYPE) {
			do {
				t = t->next;
				if(t->type == VALUE_TYPE && t->value) {
					printf(",");
					safe_print_string(t->value);
				}
			} while(t && t->next && t != next);
		}
		printf(");\n");
		print_active = 0;
		break;
	case EXPR_TYPE:
		// Return self so we can finish string literal
		if(print_active)
			return expr;
		printf("%s\n", expr->value);
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
			printf(SNPRINT_HEADER);
		printf("%s", rformat);
		if(prev && prev->type == EXPR_TYPE && !print_active)
			printf("\", %s);", expr->value);
		break;
	};
	if(next)
		return print_template(next, expr);
	return NULL;
}
