#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "parser.h"
#include "lexer.h"

static void test_primary_key_cursor(void **state) {
	yyscan_t scanner;
	YY_BUFFER_STATE parser_state;
	char buffer[MAX_STR_CONST];
	SqlStatement *result;
	SqlTable *table;

	octo_init();
	if (yylex_init(&scanner)) {
		fprintf(stderr, "Error initializing the scanner\n");
		return;
	}
	parser_state = yy_scan_string("CREATE TABLE myTable (id INTEGER PRIMARY KEY, name VARCHAR(20), age INTEGER);",
	                              scanner);
	assert_true(yyparse(scanner, &result) == 0);
	UNPACK_SQL_STATEMENT(table, result, table);
	assert_true(generate_cursor(buffer, MAX_STR_CONST, table) == 0);
	assert_true(strcmp("SET keys(0)=$S($G(keys(1))=\"\"\"\":$O(^myTable(keys(0))),1:keys(0))", buffer) == 0);

	cleanup_sql_statement(result);
}

static void test_two_key_cursor(void **state) {
	yyscan_t scanner;
	YY_BUFFER_STATE parser_state;
	char buffer[MAX_STR_CONST];
	SqlStatement *result;
	SqlTable *table;

	octo_init();
	if (yylex_init(&scanner)) {
		fprintf(stderr, "Error initializing the scanner\n");
		return;
	}
	parser_state = yy_scan_string("CREATE TABLE myTable (id INTEGER PRIMARY KEY, oid INTEGER KEY NUM 1, name VARCHAR(20), age INTEGER);",
	                              scanner);
	assert_true(yyparse(scanner, &result) == 0);
	UNPACK_SQL_STATEMENT(table, result, table);
	assert_true(generate_cursor(buffer, MAX_STR_CONST, table) == 0);
	assert_true(strcmp("SET keys(1)=$S((keys(0)=\"\"\"\"):\"\"\"\",$G(keys(2))=\"\"\"\":$O(^myTable(keys(0),"
		"keys(1))),1:keys(1)),keys(0)=$S($G(keys(1))=\"\"\"\":$O(^myTable(keys(0))),1:keys(0))", buffer) == 0);
	cleanup_sql_statement(result);
}

static void test_three_key_cursor(void **state) {
	yyscan_t scanner;
	YY_BUFFER_STATE parser_state;
	char buffer[MAX_STR_CONST];
	SqlStatement *result;
	SqlTable *table;

	octo_init();
	if (yylex_init(&scanner)) {
		fprintf(stderr, "Error initializing the scanner\n");
		return;
	}
	parser_state = yy_scan_string("CREATE TABLE myTable (id INTEGER PRIMARY KEY, oid INTEGER KEY NUM 1, oid2 INTEGER KEY NUM 2, name VARCHAR(20), age INTEGER);",
	                              scanner);
	assert_true(yyparse(scanner, &result) == 0);
	UNPACK_SQL_STATEMENT(table, result, table);
	assert_true(generate_cursor(buffer, MAX_STR_CONST, table) == 0);
	assert_true(strcmp("SET keys(2)=$S((keys(0)=\"\"\"\")!(keys(1)=\"\"\"\"):\"\"\"\",$G(keys(3))=\"\"\"\":$O(^myTable(keys(0),keys(1),keys(2))),1:keys(2)),"
			"keys(1)=$S((keys(0)=\"\"\"\"):\"\"\"\",$G(keys(2))=\"\"\"\":$O(^myTable(keys(0),keys(1))),1:keys(1)),"
			"keys(0)=$S($G(keys(1))=\"\"\"\":$O(^myTable(keys(0))),1:keys(0))", buffer) == 0);

	cleanup_sql_statement(result);
}static void test_two_key_cursor_with_advance(void **state) {
	yyscan_t scanner;
	YY_BUFFER_STATE parser_state;
	char buffer[MAX_STR_CONST];
	SqlStatement *result;
	SqlTable *table;

	octo_init();
	if (yylex_init(&scanner)) {
		fprintf(stderr, "Error initializing the scanner\n");
		return;
	}
	parser_state = yy_scan_string("CREATE TABLE myTable ("
		"id INTEGER PRIMARY KEY ADVANCE \"$O(^va(200,keys(0)))\", "
		"oid INTEGER KEY NUM 1 ADVANCE \"$O(^va(200,keys(0),53,keys(1)))\","
		"name VARCHAR(20), age INTEGER);",
	                              scanner);
	assert_true(yyparse(scanner, &result) == 0);
	UNPACK_SQL_STATEMENT(table, result, table);
	assert_true(generate_cursor(buffer, MAX_STR_CONST, table) == 0);
	assert_true(strcmp("SET keys(1)=$S((keys(0)=\"\"\"\"):\"\"\"\",$G(keys(2))=\"\"\"\":$O(^va(200,keys(0),53,keys(1))),1:keys(1)),"
		"keys(0)=$S($G(keys(1))=\"\"\"\":$O(^va(200,keys(0))),1:keys(0))", buffer) == 0);

	cleanup_sql_statement(result);
}

int main(void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_primary_key_cursor),
		cmocka_unit_test(test_two_key_cursor),
		cmocka_unit_test(test_three_key_cursor),
		cmocka_unit_test(test_two_key_cursor_with_advance)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
