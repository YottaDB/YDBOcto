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

-- TWI03 : where IN references grandparent query

SELECT *
FROM names a
WHERE a.firstName IN (
    SELECT b.firstName
    FROM names b
    WHERE b.id IN (SELECT c.id
        FROM names c
        WHERE c.id = a.id)
);

