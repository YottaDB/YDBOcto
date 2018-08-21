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
 * Test emitting a string as a column
 */
static void test_hello_world_expression(void **state) {
  yyscan_t scanner;
  YY_BUFFER_STATE parser_state;
  SqlStatement *result;
  char *buffer;
  size_t buffer_size = 0;
  FILE *out;

  if (yylex_init(&scanner)) {
    fprintf(stderr, "Error initializing the scanner\n");
    return;
  }
  parser_state = yy_scan_string("SELECT \"Hello world!\" FROM helloWorld;", scanner);
  assert_true(yyparse(scanner, &result) == 0);

  out = open_memstream(&buffer, &buffer_size);
  assert_non_null(out);
  emit_select_statement(out, result);
  fclose(out);
  assert_true(buffer_size > 0);
  free(buffer);
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
  int matching = 0;
  size_t buffer_size = 0;
  FILE *out;

  if (yylex_init(&scanner)) {
    fprintf(stderr, "Error initializing the scanner\n");
    return;
  }
  parser_state = yy_scan_string("SELECT 5+5/5 FROM helloWorld;", scanner);
  assert_true(yyparse(scanner, &result) == 0);

  out = open_memstream(&buffer, &buffer_size);
  assert_non_null(out);
  emit_select_statement(out, result);
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
        cmocka_unit_test(test_hello_world_expression),
        cmocka_unit_test(test_numeric_expression),
    };
    return cmocka_run_group_tests(tests, NULL, NULL);
}
