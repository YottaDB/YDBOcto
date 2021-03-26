#!/bin/bash
#################################################################
#								#
# Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

cp build/compile_commands.json .

oclint-json-compilation-database -e build/parser.c -e build/src/lexer.c -e src/tests/test_emit_create_table.c -e src/tests/test_emit_select_statement.c -e src/tests/test_parser_negatives.c

sed -i "s%$(pwd)/%%g" ./build/report.txt
diff build/report.txt tools/oclint_allowed.ref > report_diff.txt
exit "$(wc -l report_diff.txt)"
