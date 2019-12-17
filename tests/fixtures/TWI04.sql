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

-- TWI04 : where IN references parent from SET expression

SELECT *
FROM names a
WHERE a.firstName IN (
    SELECT b.firstName
    FROM names b
    WHERE b.id = a.id

    UNION ALL

    SELECT c.firstName
    FROM names c
    WHERE c.id = a.id
);

