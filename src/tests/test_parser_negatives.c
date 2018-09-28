#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdlib.h>

#include "octo.h"
#include "octo_types.h"
#include "parser.h"
#include "lexer.h"

/**
 * Test creating the DDL for a simple examle; CREATE TABLE myTable (id INTEGER PRIMARY KEY)
 */
static void test_bad_create_statement(void **state) {
	yyscan_t scanner;
	YY_BUFFER_STATE parser_state;
	SqlStatement *result;

	octo_init();
	if (yylex_init(&scanner)) {
		fprintf(stderr, "Error initializing the scanner\n");
		return;
	}
	parser_state = yy_scan_string("CREATE TBLE abc (id INTEGER);", scanner);
	assert_true(yyparse(scanner, &result) == 1);
	parser_state = yy_scan_string("CREATE TABLE abc (id HOTDOG);", scanner);
	assert_true(yyparse(scanner, &result) == 1);
	parser_state = yy_scan_string("CREATE TABLE abc (id HOTDOG));", scanner);
	assert_true(yyparse(scanner, &result) == 1);
	parser_state = yy_scan_string("CREATE TABLE -- abc (id HOTDOG);\n", scanner);
	assert_true(yyparse(scanner, &result) == 1);
	parser_state = yy_scan_string("CREATE TABLE abc (id INTEGER, name);", scanner);
	assert_true(yyparse(scanner, &result) == 1);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_bad_create_statement)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
