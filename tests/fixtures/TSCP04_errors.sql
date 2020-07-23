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
SELECT ROUND(1.01234567890123456789012345, 20);
-- Octo does not enforce that arguments are integers and not floats.
-- Instead, YDB will coerce the second argument to an integer by truncating.
SELECT ROUND(155, 1.5);
SELECT ROUND(155, -1.5);
-- Numeric overflow
SELECT ROUND(155, -47);
