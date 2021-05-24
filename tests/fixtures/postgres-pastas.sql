-- ######################################################################
-- #									#
-- # Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	#
-- # All rights reserved.						#
-- #									#
-- #	This source code contains the intellectual property		#
-- #	of its copyright holder(s), and is made available		#
-- #	under a license.  If you do not know the terms of		#
-- #	the license, please stop and do not read further.		#
-- #									#
-- ######################################################################

CREATE TABLE names4 (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), favoritePasta VARCHAR(30));
CREATE TABLE pastas (id INTEGER PRIMARY KEY, pastaName VARCHAR(30));

INSERT INTO names4 VALUES (0,'Zero','Cool','Pappardelle');
INSERT INTO names4 VALUES (1,'Acid','Burn','Cavatelli');
INSERT INTO names4 VALUES (2,'Cereal','Killer','Penne');
INSERT INTO names4 VALUES (3,'Lord','Nikon','Rotini');
INSERT INTO names4 VALUES (4,'Joey',NULL,'Pappardelle');
INSERT INTO names4 VALUES (5,'Zero','Lame','Lasagna');
INSERT INTO names4 VALUES (6,'Bobby','Buttons','Cavatelli');
INSERT INTO names4 VALUES (7,'Acid','House','Orechiette');
INSERT INTO names4 VALUES (8,'Zero','Day','Rotini');
INSERT INTO names4 VALUES (9,'Betty','Buttons','Cavatelli');
INSERT INTO names4 VALUES (10,'Carol',NULL,NULL);

INSERT INTO pastas VALUES (0,'Fettucine');
INSERT INTO pastas VALUES (1,'Cavatelli');
INSERT INTO pastas VALUES (2,'Shells');
INSERT INTO pastas VALUES (3,'Linguini');
INSERT INTO pastas VALUES (4,'Spaghetti');
INSERT INTO pastas VALUES (5,'Rotini');
INSERT INTO pastas VALUES (6,'Penne');
INSERT INTO pastas VALUES (7,'Orechiette');
INSERT INTO pastas VALUES (8,'Lasagna');
INSERT INTO pastas VALUES (9,'Pappardelle');

