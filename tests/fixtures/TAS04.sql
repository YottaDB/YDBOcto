#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

SELECT ARRAY(SELECT 1 UNION SELECT 2) FROM names GROUP BY firstname;
SELECT ARRAY(SELECT 1 UNION ALL SELECT 1) FROM names GROUP BY firstname;
SELECT ARRAY(SELECT 1 UNION SELECT 2 UNION ALL SELECT 1) FROM names GROUP BY firstname;
SELECT ARRAY(VALUES(1)) FROM names GROUP BY firstname;
SELECT ARRAY(VALUES(1), (2), (3)) FROM names;

