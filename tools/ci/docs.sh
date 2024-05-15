#!/bin/bash -v
#################################################################
#								#
# Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

set -e
cd doc

if [ -e Makefile ]; then
    make html
else
    ninja html
fi
mv _build/html ../public

# The below code is similar to that in YDBDoc/buildall.sh. See comment there for more details on what this does (YDBDoc#397).
cd ..
outfile="public/duplicate_reference.out"
find public -name index.html -exec grep "href.*#id[0-9]" /dev/null {} \; >& $outfile
if [ -s $outfile ]; then
	echo "------------------------------------------------------------------------------"
	echo "Duplicate references found by tools/ci/docs.sh. List follows. Fix those first."
	echo "------------------------------------------------------------------------------"
	cat $outfile
	echo "--------------------------------------------------------------------------"
	exit 1
fi
rm -f $outfile

