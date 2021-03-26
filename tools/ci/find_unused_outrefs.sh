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

# NOTE: this must be run from tests/.
for i in $(find outref -type f -printf "%f\n" | sed 's/\.ref//'); do
  if [ 0 -eq "$(grep -rw "$i" | wc -l)" ]
  then
    echo "$i.ref"
  fi
done
