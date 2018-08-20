#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdlib.h>

#include "octo.h"
#include "octo_types.h"

/* A test case that does nothing and succeeds. */
static void null_test_success(void **state) {
    (void) state; /* unused */
}

/**
 * Test creating the DDL for a simple examle; CREATE TABLE myTable (id INTEGER PRIMARY KEY)
 */
static void test_simple_table_definition(void **state) {
  char *buffer;
  size_t buffer_size = 0;
  FILE *out;

  SqlConstraint primary_key_constraint = {PRIMARY_KEY, 0, 0, 0, 0};
  dqinit(&primary_key_constraint);
  SqlColumn column = {"id", INTEGER_TYPE, &primary_key_constraint, 0, 0};
  dqinit(&column);
  SqlCreateTableStatement table = {"myTable", &column};

  out = open_memstream(&buffer, &buffer_size);
  assert_non_null(out);
  emit_create_table(out, &table);
  fclose(out);
  assert_true(buffer_size > 0);
  printf("%s\n", buffer);
  free(buffer);
}

int main(void) {
    const struct CMUnitTest tests[] = {
        cmocka_unit_test(null_test_success),
        cmocka_unit_test(test_simple_table_definition),
    };
    return cmocka_run_group_tests(tests, NULL, NULL);
}
