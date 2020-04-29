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

-- TCF020 : OCTO345 : Nested functions work as expected

SELECT ABS(-ABS(-id)) from names;
SELECT ABS(ABS(-ABS(-id)-3)) from names;
SELECT * from names where id = ABS(ABS(-1)+2);
SELECT * from names where id = ABS(ABS(ABS(-1)+2)-1);
