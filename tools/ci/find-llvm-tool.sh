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
# Find an LLVM tool in the environment
#
# Takes 2 arguments ("name" and "minimum version" of tool) and outputs the name of the executable.
# If a recent enough version was not found, outputs nothing.
set -eu

tool="$1"
version="$2"
FOUND=

exists() {
	[ -x "$(command -v "$1")" ]
}

# Ubuntu likes to name the tools after the version
# NOTE: should be updated when later versions of LLVM are released
for version in $(seq 12 -1 "$version"); do
	if exists "$tool-$version"; then
		FOUND="$tool-$version"
		# We found a recent enough version so return success
		echo "$FOUND"
		exit 0
	fi
done

# We didn't find a version specific tool. Check if tool without a specific version exists.
if ! exists "$tool"; then
	# We did not find generic tool. Exit with error.
	exit 1
fi

# We found generic tool. Check if its version is at least the minimum we want.
FOUND=$tool

# Make sure we have a recent enough version.
# "clang-format --version" output is different depending on the versions.
# Example outputs on various linux distributions
#	RHEL 8       : clang-format version 11.0.0 (Red Hat 11.0.0-1.module+el8.4.0+8598+a071fcd5)
#	Ubuntu 20.04 : clang-format version 10.0.0-4ubuntu1
#	Arch Linux   : clang-format version 12.0.1
#	Ubuntu 21.04 : Ubuntu clang-format version 12.0.0-3ubuntu1~21.04.1
# We want to extract the major version from the above (11, 10, 12, 12 respectively). Hence the gsub() function usage below.
majorver=$($FOUND --version | awk '/version/ {gsub(".*clang-format version", ""); print $1}' | cut -d '.' -f 1)
if [ "$majorver" -ge "$version" ]; then
	echo "$FOUND"
else
	exit 1
fi
