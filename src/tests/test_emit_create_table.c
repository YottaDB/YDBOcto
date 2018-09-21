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

/* A test case that does nothing and succeeds. */
static void null_test_success(void **state) {
    (void) state; /* unused */
}

/**
 * Test creating the DDL for a simple examle; CREATE TABLE myTable (id INTEGER PRIMARY KEY)
 */
static void test_simple_table_definition(void **state) {
  yyscan_t scanner;
  YY_BUFFER_STATE parser_state;
  char *buffer;
  size_t buffer_size = 0;
  SqlStatement *result;
  FILE *out;

  octo_init();
  if (yylex_init(&scanner)) {
    fprintf(stderr, "Error initializing the scanner\n");
    return;
  }
  parser_state = yy_scan_string("CREATE TABLE myTable (id INTEGER PRIMARY KEY, name VARCHAR(20), age INTEGER) GLOBAL \"^myTable(id)\";",
    scanner);
  assert_true(yyparse(scanner, &result) == 0);

  out = open_memstream(&buffer, &buffer_size);
  assert_non_null(out);
  emit_create_table(out, result);
  fclose(out);
  assert_true(buffer_size > 0);
  printf("%s\n", buffer);
  free(buffer);
}

/**
 * Test emitting a table with multiple columns
 */
static void test_multiple_columns(void **state) {
  yyscan_t scanner;
  YY_BUFFER_STATE parser_state;
  SqlStatement *result;
  char *buffer, *ptr, *e_ptr;
  char *expected = "name VARCHAR";
  int matching = 0;
  size_t buffer_size = 0;
  FILE *out;

  octo_init();
  if (yylex_init(&scanner)) {
    fprintf(stderr, "Error initializing the scanner\n");
    return;
  }
  parser_state = yy_scan_string("CREATE TABLE myTable (id INTEGER PRIMARY KEY, name VARCHAR(20), age INTEGER) GLOBAL \"^myTable(id)\";",
    scanner);
  assert_true(yyparse(scanner, &result) == 0);

  out = open_memstream(&buffer, &buffer_size);
  assert_non_null(out);
  emit_create_table(out, result);
  fclose(out);
  assert_true(buffer_size > 0);
  for(ptr = buffer, e_ptr = expected; *ptr != '\0'; ptr++) {
    if(*ptr == *e_ptr)
    {
      e_ptr++;
      matching++;
      if(matching == strlen(expected))
        break;
    } else
      assert_true(matching == 0);
  }
  free(buffer);
}

int main(void) {
    const struct CMUnitTest tests[] = {
        cmocka_unit_test(null_test_success),
        cmocka_unit_test(test_simple_table_definition),
        cmocka_unit_test(test_multiple_columns),
    };
    return cmocka_run_group_tests(tests, NULL, NULL);
}
