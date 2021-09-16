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

-- TDFT06 : OCTO54 : Test TP wrapping of M plan for DELETE FROM, INSERT INTO, SELECT, VALUES and UNION/INTERSECT/EXCEPT

CREATE FUNCTION DISPLAYTLEVEL() RETURNS VARCHAR AS $$DISPLAYTLEVEL^TDFT06;

SELECT '-- Test of $TLEVEL for SELECT : Expect 0';
SELECT * FROM names WHERE DISPLAYTLEVEL() is NULL LIMIT 1;

SELECT '-- Test of $TLEVEL for VALUES : Expect 0';
VALUES (1, DISPLAYTLEVEL());

SELECT '-- Test of $TLEVEL for UNION : Expect 0';
SELECT DISPLAYTLEVEL() UNION SELECT DISPLAYTLEVEL();

SELECT '-- Test of $TLEVEL for INTERSECT : Expect 0';
SELECT DISPLAYTLEVEL() INTERSECT SELECT DISPLAYTLEVEL();

SELECT '-- Test of $TLEVEL for EXCEPT : Expect 0';
SELECT DISPLAYTLEVEL() EXCEPT SELECT DISPLAYTLEVEL();

SELECT '-- Test of $TLEVEL for UNION ALL : Expect 0';
SELECT DISPLAYTLEVEL() UNION ALL SELECT DISPLAYTLEVEL();

SELECT '-- Test of $TLEVEL for INTERSECT ALL : Expect 0';
SELECT DISPLAYTLEVEL() INTERSECT ALL SELECT DISPLAYTLEVEL();

SELECT '-- Test of $TLEVEL for EXCEPT ALL : Expect 0';
SELECT DISPLAYTLEVEL() EXCEPT ALL SELECT DISPLAYTLEVEL();

SELECT '-- Test of $TLEVEL for INSERT INTO : Expect 1';
INSERT INTO names SELECT id+1,firstname,lastname FROM names WHERE DISPLAYTLEVEL() is NULL ORDER BY id DESC LIMIT 1;

SELECT '-- Test of $TLEVEL for DELETE FROM : Expect 1';
DELETE FROM names WHERE DISPLAYTLEVEL() is NULL AND id = 6;

