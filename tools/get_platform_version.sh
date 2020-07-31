#!/bin/bash
#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

platform_version="$(cat /etc/os-release | grep "VERSION_ID" | cut -f 2 -d '=' | sed 's/"//g')"
echo -n $platform_version
