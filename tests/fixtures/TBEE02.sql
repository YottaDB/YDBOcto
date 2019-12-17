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

-- TBEE02 : OCTO330 : OR usage processes left operand ahead of right operand

SELECT name FROM easynames WHERE id < 3 OR id > 12 LIMIT 3;

(SELECT name FROM easynames WHERE id < 3 ORDER BY name)
	INTERSECT ALL (select name FROM easynames WHERE id < 3 OR id > 12)
EXCEPT (select name FROM easynames WHERE id < 2 ORDER BY name)
	INTERSECT ALL (select name FROM easynames WHERE id = 4);

(SELECT name FROM easynames WHERE id < 3 ORDER BY name)
	INTERSECT ALL (select name FROM easynames WHERE id < 3 OR id > 12 LIMIT 3)
EXCEPT (select name FROM easynames WHERE id < 2 ORDER BY name)
	INTERSECT ALL (select name FROM easynames WHERE id = 4 LIMIT 5);
