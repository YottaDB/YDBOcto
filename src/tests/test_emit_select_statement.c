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

static int setup_myTable(void **state) {
  yyscan_t scanner;
  YY_BUFFER_STATE parser_state;
  SqlStatement *result;
  SqlTable *table;

  octo_init();
  if (yylex_init(&scanner)) {
    fprintf(stderr, "Error initializing the scanner\n");
    return 1;
  }
  parser_state = yy_scan_string("CREATE TABLE myTable (id INTEGER PRIMARY KEY, name VARCHAR(255));", scanner);
  assert_true(yyparse(scanner, &result) == 0);

  UNPACK_SQL_STATEMENT(table, result, table);
  if(definedTables == NULL) {
    definedTables = table;
    dqinit(definedTables);
  } else {
    dqinsert(definedTables, table);
  }
  return 0;
}

/**
 * Test emitting a string as a column
 */
static void test_hello_world_expression(void **state) {
  yyscan_t scanner;
  YY_BUFFER_STATE parser_state;
  SqlStatement *result;
  char *buffer;
  size_t buffer_size = 0;
  FILE *out;

  octo_init();
  if (yylex_init(&scanner)) {
    fprintf(stderr, "Error initializing the scanner\n");
    return;
  }
  parser_state = yy_scan_string("CREATE TABLE myTable (id INTEGER PRIMARY KEY);", scanner);
  assert_true(yyparse(scanner, &result) == 0);
  UNPACK_SQL_STATEMENT(definedTables, result, table);
  parser_state = yy_scan_string("SELECT \"Hello world!\" FROM myTable;", scanner);
  assert_true(yyparse(scanner, &result) == 0);
}

/**
 * Test emitting a numeric expression as a column
 */
static void test_numeric_expression(void **state) {
  yyscan_t scanner;
  YY_BUFFER_STATE parser_state;
  SqlStatement *result;
  char *buffer, *ptr, *e_ptr;
  char *expected = "(5+(5/5))";
  int matching = 0, max_match = 0;
  size_t buffer_size = 0;
  FILE *out;

  octo_init();
  if (yylex_init(&scanner)) {
    fprintf(stderr, "Error initializing the scanner\n");
    return;
  }
  parser_state = yy_scan_string("CREATE TABLE myTable (id INTEGER PRIMARY KEY);", scanner);
  assert_true(yyparse(scanner, &result) == 0);
  UNPACK_SQL_STATEMENT(definedTables, result, table);
  parser_state = yy_scan_string("SELECT 5+5/5 FROM myTable;", scanner);
  assert_true(yyparse(scanner, &result) == 0);
}

int main(void) {
    const struct CMUnitTest tests[] = {
        cmocka_unit_test_setup(test_hello_world_expression, setup_myTable),
        cmocka_unit_test_setup(test_numeric_expression, setup_myTable),
    };
    return cmocka_run_group_tests(tests, NULL, NULL);
}
