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

actual_release=$($ydb_dist/yottadb -version | grep "YottaDB release:" | awk '{print $3}' | tr -d r)

echo -n $actual_release

# Require an expected version
if [ -z $1 ]; then
	exit 1
fi
# https://stackoverflow.com/a/25731924/7669110
# Split by `.`, then compare each field numerically.
later_version=$(printf "$actual_release\n$1\n" | sort -t '.' -k 1,1 -k 2,2 -n | tail -n1)
if [[ "$actual_release" == $later_version ]]; then
	exit 0
else
	exit 1
fi
