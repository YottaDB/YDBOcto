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

-- TCSP05 : OCTO560 : Support TRUNC() function
SELECT TRUNC(1.525, 0); -- same as one-argument version
SELECT TRUNC(1.525, 1);
SELECT TRUNC(1, 0); -- should work for integers as well as floats
SELECT TRUNC(1, 1);
SELECT TRUNC(1.575, 2); -- should truncate down, not round
SELECT TRUNC(-1.525, 2); -- should truncate up, not round
SELECT TRUNC(-1.575, 2); -- should truncate up, not round
SELECT TRUNC(155, -1);
SELECT TRUNC(155, -2);
SELECT TRUNC(155, -3);
SELECT TRUNC(155, -43);
SELECT TRUNC(155, 44);
