#!/bin/bash

cp build/compile_commands.json .

oclint-json-compilation-database -e build/parser.c -e build/src/lexer.c -e src/tests/test_emit_create_table.c -e src/tests/test_emit_select_statement.c -e src/tests/test_parser_negatives.c

sed -i "s%$(pwd)/%%g" ./build/report.txt
diff build/report.txt tools/oclint_allowed.txt > report_diff.txt
exit $(cat report_diff.txt | wc -l)
