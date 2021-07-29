#!/bin/sh
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

if [ -x "$(command -v "$1")" ]; then
	CLANG_FORMAT="$1"
else
	CLANG_FORMAT=clang-format
fi
# NOTE: does not format .ctemplate files, because clang-format will change `%{}` to `% {}`, breaking the preprocessor.
find ../src \( -name '*.c' -o -name '*.h' \) -print0 | xargs -0 "$CLANG_FORMAT" -i
if ! [ "$(git diff --stat | wc -l)" = 0 ]; then
  echo " -> Formatting differences found!"
  git diff
  exit 1
fi
