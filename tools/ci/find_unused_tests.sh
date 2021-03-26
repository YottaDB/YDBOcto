#!/bin/bash
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

for i in $(cd ../tests && find . -type f -maxdepth 1 | sed -E 's#^./##; s/\.bats\.in//'); do
  if [ 0 -eq "$(grep -c "$i" bats-tests.cmake)" ]
  then
    echo "$i.bats.in"
  fi
done
