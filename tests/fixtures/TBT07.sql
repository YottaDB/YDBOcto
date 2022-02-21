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

-- TBT07 : OCTO498 : Test of various error scenarios of IS (NOT) TRUE, IS (NOT) FALSE, and IS (NOT) UNKNOWN

-- Test comparison of non-BOOLEAN field against each IS and IS NOT scenario, i.e. TRUE, FALSE, UNKNOWN
SELECT * FROM names WHERE id IS TRUE;
SELECT * FROM names WHERE id IS NOT TRUE;
SELECT * FROM names WHERE id IS FALSE;
SELECT * FROM names WHERE id IS NOT FALSE;
SELECT * FROM names WHERE id IS UNKNOWN;
SELECT * FROM names WHERE id IS NOT UNKNOWN;
SELECT * FROM names a1 WHERE a1.id IS TRUE;
SELECT * FROM names a1 WHERE a1.id IS NOT TRUE;
SELECT * FROM names a1 WHERE a1.id IS FALSE;
SELECT * FROM names a1 WHERE a1.id IS NOT FALSE;
SELECT * FROM names a1 WHERE a1.id IS UNKNOWN;
SELECT * FROM names a1 WHERE a1.id IS NOT UNKNOWN;
SELECT (SELECT a2.id from names a2 where a1.id = a2.id) IS TRUE FROM names a1;
SELECT (SELECT a2.id from names a2 where a1.id = a2.id) IS NOT TRUE FROM names a1;
SELECT (SELECT a2.id from names a2 where a1.id != a2.id ORDER BY a2.id LIMIT 1) IS TRUE FROM names a1;
SELECT (SELECT a2.id from names a2 where a1.id != a2.id ORDER BY a2.id LIMIT 1) IS NOT TRUE FROM names a1;
SELECT (SELECT a2.id from names a2 where a1.id = a2.id) IS FALSE FROM names a1;
SELECT (SELECT a2.id from names a2 where a1.id = a2.id) IS NOT FALSE FROM names a1;
SELECT (SELECT a2.id from names a2 where a1.id != a2.id ORDER BY a2.id LIMIT 1) IS FALSE FROM names a1;
SELECT (SELECT a2.id from names a2 where a1.id != a2.id ORDER BY a2.id LIMIT 1) IS NOT FALSE FROM names a1;
SELECT (SELECT a2.id from names a2 where a1.id = a2.id) IS UNKNOWN FROM names a1;
SELECT (SELECT a2.id from names a2 where a1.id = a2.id) IS NOT UNKNOWN FROM names a1;
SELECT (SELECT a2.id from names a2 where a1.id != a2.id ORDER BY a2.id LIMIT 1) IS UNKNOWN FROM names a1;
SELECT (SELECT a2.id from names a2 where a1.id != a2.id ORDER BY a2.id LIMIT 1) IS NOT UNKNOWN FROM names a1;
