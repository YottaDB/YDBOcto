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

if ! clang_tidy=$($(git rev-parse --show-toplevel)/tools/ci/find-llvm-tool.sh clang-tidy 8); then
	echo "error: clang-tidy-8 or greater is required"
	exit 1
fi

find ../src -name '*.c' | grep -v '/test_.*\.c' \
	| xargs $clang_tidy --checks="$ignored_warnings" "$@"
