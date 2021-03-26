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
# Takes no arguments and outputs the name of the executable.
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
for version in $(seq 11 -1 "$version"); do
	if exists "$tool-$version"; then
		FOUND="$tool-$version"
		break
	fi
done

# No version suffix, we get what we get.
if [ "" = "$FOUND" ]; then
	# We didn't find it at all.
	if ! exists "$tool"; then
		exit 1
	fi
	FOUND=$tool
fi

# Make sure we have a recent enough version.
if [ "$($FOUND --version | grep version | awk '{print $3}' | cut -d '.' -f 1)" -ge "$version" ]; then
	echo "$FOUND"
else
	exit 1
fi
