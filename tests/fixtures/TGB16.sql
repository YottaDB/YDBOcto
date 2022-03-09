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

-- TGB16: OCTO812 : Test sub-query in WHERE clause doing GROUP BY on parent query column

-- Below is a query from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/812#note_869019770

SELECT n1.lastname FROM names n1 WHERE EXISTS (SELECT COUNT(*) FROM names n2 WHERE FALSE GROUP BY n1.lastname);

