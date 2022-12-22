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

-- TII10 : OCTO502 : Test that INSERT INTO removes unnecessary trailing zeros in NUMERIC(PRECISION,SCALE)

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1203#note_1219440100

DROP TABLE IF EXISTS tmp;
CREATE TABLE tmp (id INTEGER PRIMARY KEY, salary NUMERIC(20,2));
INSERT INTO tmp VALUES (1, 2.1);
INSERT INTO tmp VALUES (2, 2.123);
INSERT INTO tmp VALUES (3, -0.0);
SELECT * FROM tmp;
SELECT '-- Below SELECT query for SALARY = 2.123 should return 0 rows since only 2.12 got stored (SCALE = 2)';
SELECT * FROM tmp WHERE salary = 2.123;
SELECT '-- Below SELECT query for SALARY = 0 should return 1 row even though we stored -0.0 as they are the same value';
SELECT * FROM tmp WHERE salary = 0;
SELECT '-- Below SELECT query for SALARY = -0 should return 1 row even though we stored -0.0 as they are the same value';
SELECT * FROM tmp WHERE salary = -0;
SELECT '-- Below SELECT query for SALARY = 2.1 should return 1 row since we stored a row with 2.1 SALARY';
SELECT * FROM tmp WHERE salary = 2.1;

