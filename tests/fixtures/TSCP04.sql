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

-- TSCP04 : OCTO561 : Support ROUND() function
SELECT ROUND(1.525, 0); -- same as one-argument version
SELECT ROUND(1.525, 1);
SELECT ROUND(1, 0); -- should work for integers as well as floats
SELECT ROUND(1, 1);
SELECT ROUND(1.575, 2); -- should round up, not truncate
-- negative numbers
SELECT ROUND(-1.525, 2);
SELECT ROUND(-1.575, 2);
select ROUND(-1.5, 0);
-- negative precision
SELECT ROUND(155, -1);
SELECT ROUND(155, -2);
SELECT ROUND(155, -3);
SELECT ROUND(155, -46);
SELECT ROUND(-1.5, -1);
SELECT ROUND(155, 46);
SELECT ROUND(155, 100);
SELECT ROUND(155, 1000);
