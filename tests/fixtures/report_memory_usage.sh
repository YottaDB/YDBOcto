#!/bin/bash
#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

while [[ $(grep -c "with pid" rocto.log) -lt 1 ]]; do
    sleep .1
done

grep "with pid" rocto.log | sed 's/^.*rocto server process forked with pid \([0-9]*\)/\1/' | while read pid; do
    ps -p $pid -o vsize | cut -d ' ' -f 2 | tr -d '\n' >> mem_usage.log
done
echo >> mem_usage.log
