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

-- TCTC014 : OCTO770 : Test minimal use of gvns in M plans for UNIQUE/PRIMARY KEY constraints in INSERT/UPDATE/DELETE

DROP TABLE IF EXISTS tmp;
CREATE TABLE tmp (id1 INTEGER, id2 INTEGER, PRIMARY KEY(id1, id2), UNIQUE (id2));
INSERT INTO tmp VALUES (3, 2);
UPDATE TMP SET id2 = 4;
DELETE FROM TMP;

