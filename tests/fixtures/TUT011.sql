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

-- TUT011 : OCTO579 : Correct results from UPDATE even if previous UPDATE issued a ERR_DUPLICATE_KEY_VALUE error

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/579#note_1098951468

CREATE TABLE tmp (id1 INTEGER PRIMARY KEY, id2 INTEGER KEY NUM 1);
INSERT INTO tmp VALUES (3, 2);
INSERT INTO tmp VALUES (3, 5);
UPDATE tmp SET id2 = 4;
DROP TABLE tmp;

CREATE TABLE tmp (id1 INTEGER PRIMARY KEY, id2 INTEGER KEY NUM 1, CHECK (id1 > 2));
INSERT INTO tmp VALUES (3, 4);
INSERT INTO tmp VALUES (5, 4);
UPDATE tmp SET id1 = 4;
DROP TABLE tmp;

