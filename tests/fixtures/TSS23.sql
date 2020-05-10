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

SELECT (SELECT firstname FROM (SELECT id,firstname FROM names n1) n2 WHERE n2.id = n3.id) FROM names n3;
SELECT (SELECT firstname FROM (SELECT id,firstname FROM names n1) n2 ORDER BY 1 DESC LIMIT 1) FROM names n3;
SELECT (SELECT firstname FROM (SELECT n3.id,firstname FROM names n1) n2 ORDER BY 1 DESC LIMIT 1) FROM names n3;

