#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/OR its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSD03 : OCTO434 : DISTINCT in sub-query and outer-query returns duplicate rows if outer query WHERE clause has OR conditions that match the same row

SELECT DISTINCT firstname,(SELECT DISTINCT n1.lastname FROM names n1 ORDER BY n1.lastname DESC LIMIT 1) FROM names WHERE id = 0 OR id < 2;
SELECT DISTINCT firstname,(SELECT n1.lastname FROM names n1 ORDER BY n1.lastname DESC LIMIT 1) FROM names WHERE id = 0 OR id < 2;
SELECT DISTINCT firstname,(SELECT DISTINCT n1.lastname FROM names n1 ORDER BY n1.lastname DESC LIMIT 1) FROM names WHERE id < 3 OR id > 2;
SELECT DISTINCT firstname,(SELECT n1.lastname FROM names n1 ORDER BY n1.lastname DESC LIMIT 1) FROM names WHERE id < 3 OR id > 2;

