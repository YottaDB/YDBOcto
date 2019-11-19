#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TOB04 : OCTO322 : ORDER BY derived column where sub-query does UNION ALL

SELECT * from (select * from names) ORDER BY id;
SELECT * from (select * from names UNION ALL select * from names) ORDER BY id;
SELECT * from (select firstname from names UNION ALL select lastname from names) ORDER BY firstname;
SELECT firstname from (select firstname from names UNION ALL select lastname from names) ORDER BY firstname;
SELECT * from ((select * from names) UNION ALL (select * from names)) namesunion ORDER BY firstname;
SELECT * from ((select * from names) UNION ALL (select * from names)) namesunion ORDER BY namesunion.firstname;
SELECT namesunion.firstname from ((select * from names) UNION ALL (select * from names)) namesunion ORDER BY firstname;
SELECT NULL as computed_id from (select * from names UNION ALL select * from names) ORDER BY computed_id;
SELECT 1 as computed_id from (select * from names UNION ALL select * from names) ORDER BY computed_id;
SELECT 1+id*2 as computed_id from (select * from names UNION ALL select * from names) ORDER BY computed_id;
