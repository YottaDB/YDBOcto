#!/bin/bash
#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

# This script ensures all assertions in the code base continue to be valid even after a commit.
# Examples of these assertions are checking that changes to one module also change a related module if necessary etc.

set -e	# Ensure any errors in this script cause it to exit with a non-zero status right away
set -u	# Enable detection of uninitialized variables.
set -o pipefail

topleveldir=$(git rev-parse --show-toplevel)
cwd=$(pwd)
if [[ $topleveldir != "$cwd" ]]; then
	echo "ERROR: check_code_assertions.sh run from $cwd (expected: $topleveldir)"
	exit 1
fi

# -------------------------------------------------------------------------------------------------------
# Verify that "case" blocks in src/qualify_statement.c and src/qualify_check_constraint.c are in sync
# If not, it is likely an update to one forgot to update the other. Catch that situation.
# The interesting "case" blocks are identified by having just one or more tabs after the start of the line.
# -------------------------------------------------------------------------------------------------------
caselist () {
	grep -E '^[	]*case|^[	]*default:' $1 | sort | sed 's/;.*//g'
}

mytmpdir=$(mktemp -d "${TMPDIR:-/tmp/}$(basename $0).XXXXXXXXXXXX")

for file in qualify_statement qualify_check_constraint
do
	caselist src/$file.c > $mytmpdir/$file.caselist
done

if ! diff -q $mytmpdir/qualify_statement.caselist $mytmpdir/qualify_check_constraint.caselist > /dev/null; then
	echo "ERROR: src/qualify_statement.c and src/qualify_check_constraint.c list of case blocks are not in sync"
	echo "INFO: Differing case blocks are listed below"
	diff $mytmpdir/qualify_statement.caselist $mytmpdir/qualify_check_constraint.caselist
	rm -rf $mytmpdir
	exit 1
fi

rm -rf $mytmpdir
exit 0
