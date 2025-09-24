#!/bin/sh
#################################################################
#								#
# Copyright (c) 2019-2025 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
# Find an LLVM tool in the environment
#
# Takes 2 arguments ("name" and "minimum version" of tool) and outputs the name of the executable.
# If a recent enough version was not found, outputs nothing.
set -eu

tool="$1"
version="$2"

exists() {
	[ -x "$(command -v "$1")" ]
}

# Ubuntu likes to name the tools after the version
# NOTE: should be updated when later versions of LLVM are released
for version in $(seq 15 -1 "$version"); do
	if exists "$tool-$version"; then
		# We found a recent enough version so return success
		echo "$tool-$version"
		exit 0
	else
		echo "Version not found: $tool-$version"
		exit 1
	fi
done

# We didn't find a version specific tool. Exit with error.
echo "No LLVM tool found >= version 15. Please install LLVM tools >= 15 to run this script."
exit 1
