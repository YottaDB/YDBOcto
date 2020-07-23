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

-- Octo only supports numbers with a precision of up to 18 significant digits.
SELECT TRUNC(1.01234567890123456789012345, 20);
-- 10**(-44) == 0 in YDB, so will give a divide by zero error
SELECT TRUNC(155, -44);
-- Octo does not enforce that arguments are integers and not floats.
-- Instead, YDB will coerce the second argument to an integer by truncating.
SELECT TRUNC(155, 1.5);
SELECT TRUNC(155, -1.5);
SELECT TRUNC(155, 45);
