-- ######################################################################
-- #									#
-- # Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.		#
-- # All rights reserved.						#
-- #									#
-- #	This source code contains the intellectual property		#
-- #	of its copyright holder(s), and is made available		#
-- #	under a license.  If you do not know the terms of		#
-- #	the license, please stop and do not read further.		#
-- #									#
-- ######################################################################

CREATE TABLE composite (
	id0 INTEGER,
	id1 INTEGER,
	id2 INTEGER,
	id3 INTEGER,
	id4 INTEGER,
	id5 INTEGER,
	id6 INTEGER,
	id7 INTEGER,
	name VARCHAR(20),
	UNIQUE(id0, id1, id2, id3, id4, id5, id6, id7)
);

INSERT INTO composite VALUES (0,1,2,3,4,5,6,7,'Name1');
INSERT INTO composite VALUES (0,1,2,3,4,5,6,8,'Name2');
INSERT INTO composite VALUES (0,1,2,3,4,5,7,7,'Name3');
INSERT INTO composite VALUES (0,1,2,3,4,5,8,7,'Name4');
INSERT INTO composite VALUES (0,1,2,3,4,6,8,7,'Name5');
INSERT INTO composite VALUES (0,1,2,3,5,6,8,7,'Name6');
INSERT INTO composite VALUES (0,1,2,4,5,6,8,7,'Name7');
INSERT INTO composite VALUES (0,1,3,4,5,6,8,7,'Name8');
INSERT INTO composite VALUES (0,2,3,4,5,6,8,7,'Name9');
INSERT INTO composite VALUES (1,2,3,4,5,6,8,7,'Name10');

