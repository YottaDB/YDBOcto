#!/bin/sh

#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

ignored_warnings="\
-clang-analyzer-security.insecureAPI.DeprecatedOrUnsafeBufferHandling,\
-clang-diagnostic-gnu-zero-variadic-macro-arguments,\
-clang-analyzer-security.insecureAPI.strcpy"

find ../src -name '*.c' | grep -v '/test_.*\.c' \
	| xargs clang-tidy --checks="$ignored_warnings" "$@"
