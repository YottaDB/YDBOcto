#################################################################
#								#
# Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TII09 : OCTO502 : Test that INSERT INTO correctly adds duplicate rows on table with no primary key columns

CREATE TABLE tmp1 (id INTEGER, firstname VARCHAR);
INSERT INTO tmp1 VALUES (1234, 'First'), (1234, 'First');
INSERT INTO tmp1 SELECT * FROM tmp1;
-- In addition to testing that duplicate rows are printed below, also test that hidden key column %YO_KEYCOL
-- is not printed in case of a SELECT *. Only user specified columns are selected in case * was specified.
SELECT * FROM tmp1;
-- Test that tablename.* syntax also skips hidden key column %YO_KEYCOL like * did above.
SELECT t1.* from tmp1 t1;

-- Test that "%YO_KEYCOL" column is hidden from the user (lower case or upper case names) if it was implicitly inserted by Octo.
-- Test of ERR_UNKNOWN_COLUMN_NAME
SELECT `%yo_keycol` FROM tmp1;
SELECT `%YO_KEYCOL` FROM tmp1;

-- Test that INSERT INTO cannot specify the hidden column by name in the list of columns that it is allowed to optionally specify.
-- Test of ERR_TABLE_UNKNOWN_COLUMN_NAME error
INSERT INTO tmp1(`%yo_keycol`, id) SELECT id, id from tmp1;
INSERT INTO tmp1(id, `%yo_keycol`) SELECT id, id from tmp1;

-- Test that "%YO_KEYCOL" is a column name that can be used in a CREATE TABLE as well as a SELECT if it was NOT an implicitly inserted column.
-- Create a READONLY table tmp2 that uses the same gvn created by the READWRITE table tmp1 so we can see the actual hidden column.
CREATE TABLE tmp2 (`%yo_keycol` INTEGER PRIMARY KEY, id INTEGER, firstname VARCHAR) READONLY GLOBAL "^%ydboctoDAgeMzpAgs346YlVTTNcBB6(keys(""%yo_keycol"")";
SELECT `%yo_keycol` from tmp2;
SELECT * from tmp2;
-- Test that tablename.* syntax produces same output as * did above;
-- i.e. tablename.* syntax includes key column %YO_KEYCOL since it is not a hidden column.
SELECT t2.* from tmp2 t2;

-- Test that cross references get built automatically using the hidden key column for queries that involve key fixing optimization.
CREATE TABLE tmp3 (firstName VARCHAR, lastName varchar) READWRITE;
INSERT INTO tmp3 VALUES (NULL, NULL);
INSERT INTO tmp3 VALUES (NULL, 'last1');
INSERT INTO tmp3 VALUES (NULL, 'last2');
INSERT INTO tmp3 VALUES ('first1', NULL);
INSERT INTO tmp3 VALUES ('first1', 'last1');
INSERT INTO tmp3 VALUES ('first1', 'last2');
INSERT INTO tmp3 VALUES ('first2', NULL);
INSERT INTO tmp3 VALUES ('first2', 'last1');
INSERT INTO tmp3 VALUES ('first2', 'last2');
SELECT * FROM tmp3 WHERE firstname = 'first1';
SELECT * FROM tmp3 WHERE lastname = 'last2';
SELECT * FROM tmp3 WHERE firstname IS NULL;
SELECT * FROM tmp3 WHERE lastname IS NULL;
-- Test that tablename.* syntax uses hidden key column for key fixing optimization where possible just like * did above.
SELECT t3.* FROM tmp3 t3 WHERE firstname = 'first1';
SELECT t3.* FROM tmp3 t3 WHERE lastname = 'last2';
SELECT t3.* FROM tmp3 t3 WHERE firstname IS NULL;
SELECT t3.* FROM tmp3 t3 WHERE lastname IS NULL;
