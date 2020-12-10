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

-- TOB14 : OCTO643 : ORDER BY returns incorrect results when used on a value returned by the ROUND function

SELECT id FROM (VALUES (ROUND(2.1234567890,10)), (ROUND(1,10))) AS n1(id) ORDER BY id;
SELECT id FROM (VALUES (ROUND(2.1234567891,10)), (ROUND(1,10))) AS n1(id) ORDER BY id;

