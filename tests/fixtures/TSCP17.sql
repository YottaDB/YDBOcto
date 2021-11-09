#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSCP17 : OCTO779 : Incorrect ERR_UNKNOWN_COLUMN_NAME error when GROUP BY is used on a VALUES table in the JOIN list

SELECT 1 FROM names, (values ('Joey')) AS alias1(name) GROUP BY alias1.name;
SELECT COUNT(*) FROM names, (values ('Joey')) AS alias1(name) GROUP BY alias1.name;
SELECT COUNT(DISTINCT alias1.*) FROM names, (values ('Joey')) AS alias1(name) GROUP BY alias1.name;
SELECT COUNT(alias1.name) FROM names, (values ('Joey')) AS alias1(name) GROUP BY alias1.name;
SELECT COUNT(alias1.name) FROM names, (values ('Joey')) AS alias1(name) GROUP BY name;
SELECT name FROM names, (values ('Joey')) AS alias1(name) GROUP BY name;

