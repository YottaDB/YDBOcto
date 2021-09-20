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

-- TBT05 : OCTO747 : Test WHERE/HAVING/ON clause with non-boolean expressions issues ERR_TYPE_NOT_COMPATIBLE error

SELECT * from names WHERE firstname;
SELECT * from names WHERE id;
SELECT COUNT(*) from names GROUP BY firstname HAVING firstname;
SELECT COUNT(*) from names GROUP BY id HAVING id;
SELECT * from names n1 INNER JOIN names n2 ON n2.firstname;
SELECT * from names n1 INNER JOIN names n2 ON n2.id

