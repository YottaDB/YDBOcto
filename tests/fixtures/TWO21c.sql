#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- ############################################################################################################
-- Part N: a READWRITE table, whose rows Octo writes. INSERT, UPDATE and DELETE go through the triggers, so the cross
-- reference of b stays right. A DELETE or UPDATE with a WHERE on b walks it too.
CREATE TABLE rwk (a INTEGER, b INTEGER, v VARCHAR, PRIMARY KEY (a, b)) GLOBAL "^RWK(keys(""a""),keys(""b""))" READWRITE;
-- Three rows.
INSERT INTO rwk VALUES (1, 10, 'x'), (2, 10, 'y'), (2, 20, 'z');
-- [no key column xref] the rows of the table: 1|10|x, 2|10|y and 2|20|z.
SELECT * FROM rwk;
-- [xref on rwk.b] 1|10|x and 2|10|y.
SELECT * FROM rwk WHERE b = 10 ORDER BY a;
-- An INSERT adds 3|10|w to the cross reference.
INSERT INTO rwk VALUES (3, 10, 'w');
-- [xref on rwk.b] 1|10|x, 2|10|y and 3|10|w.
SELECT * FROM rwk WHERE b = 10 ORDER BY a;
-- [no key column xref] a DELETE on the first key column removes 1|10|x from the cross reference.
DELETE FROM rwk WHERE a = 1;
-- [xref on rwk.b] 2|10|y and 3|10|w.
SELECT * FROM rwk WHERE b = 10 ORDER BY a;
-- [no key column xref] an UPDATE of the key column b, with both key columns fixed in place: 2|10|y moves to b = 30.
UPDATE rwk SET b = 30 WHERE a = 2 AND b = 10;
-- [xref on rwk.b] 3|10|w.
SELECT * FROM rwk WHERE b = 10 ORDER BY a;
-- [xref on rwk.b] 2|30|y.
SELECT * FROM rwk WHERE b = 30 ORDER BY a;
-- [xref on rwk.b] a DELETE with a WHERE on b: 2|20|z goes.
DELETE FROM rwk WHERE b = 20;
-- [no key column xref] the rows of the table: 2|30|y and 3|10|w.
SELECT * FROM rwk;
-- Three more rows.
INSERT INTO rwk VALUES (4, 40, 'a'), (5, 40, 'b'), (6, 50, 'c');
-- [xref on rwk.b] an UPDATE of a non-key column: 4|40|a and 5|40|b get v = 'B'.
UPDATE rwk SET v = 'B' WHERE b = 40;
-- [xref on rwk.b] 4|40|B and 5|40|B.
SELECT * FROM rwk WHERE b = 40 ORDER BY a;
-- [xref on rwk.b] an UPDATE of the key column itself: the rows move to b = 41, each once.
UPDATE rwk SET b = 41 WHERE b = 40;
-- [xref on rwk.b] no row.
SELECT * FROM rwk WHERE b = 40 ORDER BY a;
-- [xref on rwk.b] 4|41|B and 5|41|B.
SELECT * FROM rwk WHERE b = 41 ORDER BY a;
-- [xref on rwk.b] a DELETE with a WHERE on b: 4|41|B and 5|41|B go.
DELETE FROM rwk WHERE b = 41;
-- [no key column xref] the rows of the table: 2|30|y, 3|10|w and 6|50|c.
SELECT * FROM rwk;
-- [xref on rwk.b] an UPDATE of the key column b whose WHERE has an OR. It runs one plan per alternative: the first walks
-- the cross reference of b and updates 3|10|w; the second walks the table's global and updates 2|30|y and 6|50|c, but
-- not 3|-54|w again, which the first noted as updated. UPDATE 3.
UPDATE rwk SET b = b - 64 WHERE b IN (10, 99) OR a < 7;
-- [no key column xref] the rows of the table: 2|-34|y, 3|-54|w and 6|-14|c.
SELECT * FROM rwk;
