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

-- TUT013 : OCTO579 : Test ERR_CHECK_CONSTRAINT_VIOLATION is not incorrectly issued by UPDATE

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1203#note_1219385042

DROP TABLE IF EXISTS namesTQG03;
CREATE TABLE namesTQG03 (id INTEGER PRIMARY KEY,firstName VARCHAR,lastName VARCHAR, CHECK(lastName>firstName));
INSERT INTO namesTQG03 VALUES(0,'Cereal','Cool');
UPDATE namesTQG03 SET lastName=NULL;
SELECT * from namesTQG03;
SELECT '-- Below UPDATE query should run fine (previously it used to incorrectly issue a ERR_CHECK_CONSTRAINT_VIOLATION error)';
UPDATE namesTQG03 SET firstName=firstName;

