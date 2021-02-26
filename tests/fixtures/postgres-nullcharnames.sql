-- #################################################################
-- #								#
-- # Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
-- # All rights reserved.						#
-- #								#
-- #	This source code contains the intellectual property	#
-- #	of its copyright holder(s), and is made available	#
-- #	under a license.  If you do not know the terms of	#
-- #	the license, please stop and do not read further.	#
-- #								#
-- #################################################################

-- Below is to skip the INSERT commands if the table already exists (CREATE TABLE will cause an error and script will exit)
\set ON_ERROR_STOP on

CREATE TABLE nullcharnames (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30));

INSERT INTO nullcharnames VALUES (0,'Zero','Cool');
INSERT INTO nullcharnames VALUES (1,'Acid','Burn');
INSERT INTO nullcharnames VALUES (2,'Cereal','Killer');
INSERT INTO nullcharnames VALUES (3,'Lord','Nikon');
INSERT INTO nullcharnames VALUES (4,'Joey',NULL);
INSERT INTO nullcharnames VALUES (5,'Zero','Cool');
INSERT INTO nullcharnames VALUES (6,NULL,NULL);
INSERT INTO nullcharnames VALUES (7,NULL,NULL);
INSERT INTO nullcharnames VALUES (8,'Suzy',NULL);
