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

-- TERR033 : OCTO597 : Error scenarios in SET/SHOW statements

-- SET statement for invalid run-time parameter issues error
SET nonexistent_parameter = 'invalid';

-- SHOW statement for invalid run-time parameter issues error
SHOW nonexistent_parameter;

