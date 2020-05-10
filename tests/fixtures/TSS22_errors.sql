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

-- All queries in this query file are invalid queries and generate an error.

-- TSS22 : OCTO504 : Error not issued in some cases when sub-query returns more than one row when used as an expression

SELECT (SELECT * FROM (SELECT n3.id FROM names n1) n2) FROM names n3;
SELECT (SELECT n2.id FROM (SELECT n3.id FROM names n4) n2 INNER JOIN names n1 ON n1.id = n3.id + 1) FROM names n3;
SELECT (SELECT n2.id FROM (SELECT n1.id FROM names n1) n2) FROM names n3;

