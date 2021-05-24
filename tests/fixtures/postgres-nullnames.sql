-- ######################################################################
-- #									#
-- # Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
-- # All rights reserved.						#
-- #									#
-- #	This source code contains the intellectual property		#
-- #	of its copyright holder(s), and is made available		#
-- #	under a license.  If you do not know the terms of		#
-- #	the license, please stop and do not read further.		#
-- #									#
-- ######################################################################

CREATE TABLE nullnames (id INTEGER PRIMARY KEY NOT NULL, firstName VARCHAR(30), lastName VARCHAR(30), salary NUMERIC NOT NULL, exempt BOOLEAN NOT NULL, yearsTenured INTEGER);
CREATE TABLE nullnamesb (id INTEGER PRIMARY KEY NOT NULL, firstName VARCHAR(30), lastName VARCHAR(30), salary NUMERIC NOT NULL, exempt BOOLEAN NOT NULL, yearsTenured INTEGER NOT NULL);

INSERT INTO nullnames (id, firstName, lastName, salary, exempt, yearsTenured)
VALUES
	(0, 'Zero', 'Cool', 25000.01, TRUE, 10),
	(1, 'Acid', 'Burn', 62530.56, FALSE, 5),
	(2, 'Cereal', 'Killer', 0, FALSE, NULL),
	(3, 'Lord', 'Nikon', 2000567.49, TRUE, 2),
	(4, 'Joey', NULL, 0, FALSE, NULL),
	(5, 'Zero', 'Cool', 25000.01, TRUE, 10);

INSERT INTO nullnamesb (id, firstName, lastName, salary, exempt, yearsTenured)
VALUES
	(0, 'Zero', 'Cool', 25000.01, FALSE, 20),
	(1, 'Acid', 'Burn', 62530.56, FALSE, 5),
	(2, 'Cereal', 'Killer', 0, TRUE, 0),
	(3, 'Lord', 'Nikon', 2000567.49, FALSE, 2),
	(4, 'Joey', NULL, 0, FALSE, 0),
	(5, 'Zero', 'Cool', 25000.01, FALSE, 20);
