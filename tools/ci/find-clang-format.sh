#!/bin/sh
#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
# Find clang-format in the environment
#
# Takes no arguments and outputs the name of the clang-format executable.
# If a recent enough clang-format was not found, outputs nothing.
set -e

# Ubuntu likes to name the tools after the version
exists() {
	[ -x "$(which $1)" ]
}
# We require at least clang-format-9 (for `AlignConsecutiveMacros`)
# NOTE: should be updated when later versions of LLVM are released
for version in 11 10 9; do
	if exists clang-format-$version; then
		CLANG_FORMAT=clang-format-$version
		break
	fi
done

# No version suffix, we get what we get.
if [ "" = "$CLANG_FORMAT" ]; then
	# We didn't find it at all.
	if ! exists clang-format; then
		exit 1
	fi
	CLANG_FORMAT=clang-format
fi

# Make sure we have a recent enough version.
if [ "$($CLANG_FORMAT --version | cut -d ' ' -f 3 | cut -d '.' -f 1)" -ge 9 ]; then
	echo "$CLANG_FORMAT"
else
	exit 1
fi
