#!/bin/bash
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
# If a recent enough version was not found, outputs nothing to stdout and returns false
set -eu

tool="$1"
version="$2"

# First check if exact version exists and if so print it and exit true
command -v $tool-$version && exit

# Failing that, check whether the tool with the generic name (without the -version suffix exists)
if ! command -v $tool >/dev/null; then
	echo >/dev/stderr "No LLVM tool '$tool' found. Please install LLVM tools >= $version to run this script."
	exit 1
fi

# If so, get its version and check that it is at least as recent as the required version
tool_version=$($tool --version | grep -oP '(?<=version )[0-9.]+')

if [[ $(sort -V <(echo $tool_version) <(echo $version) | head -n1) != "$version" ]]; then
	echo >/dev/stderr "LLVM tool '$tool' is version $tool_version but must be at least version $version"
	exit 1
fi

echo $tool
