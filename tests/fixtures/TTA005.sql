#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TTA005 : OCTO759 : TABLENAME.ASTERISK produces incorrect results when used with GROUP BY and LEFT JOIN : names database

-- Below are simple queries using the "names" database with sub-queries
-- These were derived from the fancier queries generated by the TQG03 subtest.
SELECT COUNT(n1.*),COUNT(n2.*) FROM (SELECT lastname from names) n1 LEFT JOIN (SELECT lastName FROM names) n2 ON (n1.lastName = 'Burn') GROUP BY n2.*;
SELECT COUNT(n1.*),COUNT(n2.*) FROM (SELECT firstname,lastname from names) n1 LEFT JOIN (SELECT lastName FROM names) n2 ON (n1.firstName <= 'Acid') GROUP BY n2.* ORDER BY 1,2;
SELECT COUNT(n1.*),COUNT(n2.*),COUNT(n3.*) FROM (SELECT firstname,lastname FROM names) n1 LEFT JOIN (SELECT lastname FROM names) n2 ON n1.firstname <= 'Acid' LEFT JOIN (SELECT lastname FROM names) n3 ON n1.firstname <= 'Acid' GROUP BY n2.* ORDER BY 1,2,3;
SELECT COUNT(n1.*),COUNT(n2.*),COUNT(n3.*) FROM (SELECT firstname,lastname FROM names) n1 LEFT JOIN (SELECT lastname FROM names) n2 ON n1.firstname <= 'Acid' LEFT JOIN (SELECT lastname FROM names) n3 ON n1.firstname <= 'Acid' GROUP BY n3.* ORDER BY 1,2,3;
SELECT COUNT(n1.*),COUNT(n2.*),COUNT(n3.*) FROM (SELECT firstname,lastname FROM names) n1 LEFT JOIN (SELECT lastname FROM names) n2 ON n1.firstname <= 'Acid' LEFT JOIN (SELECT lastname FROM names) n3 ON n1.firstname <= 'Acid' GROUP BY n2.*,n3.* ORDER BY 1,2,3;
SELECT COUNT(n1.*),COUNT(n2.*),COUNT(n3.*) FROM (SELECT firstname,lastname FROM names) n1 LEFT JOIN (SELECT lastname FROM names) n2 ON n1.firstname <= 'Acid' cross join (SELECT lastname FROM names) n3 group by n2.* ORDER BY 1,2,3;
SELECT COUNT(n1.*),COUNT(n2.*),COUNT(n3.*) FROM (SELECT firstname,lastname FROM names) n1 LEFT JOIN (SELECT lastname FROM names) n2 ON n1.firstname <= 'Acid' cross join (SELECT lastname FROM names) n3 group by n2.*,n3.* ORDER BY 1,2,3;
SELECT COUNT(n1.*),COUNT(n2.*) FROM (SELECT firstname,lastname FROM names) n1 LEFT JOIN (SELECT lastname FROM names) n2 ON n1.firstname <= 'Acid' CROSS JOIN (SELECT lastname FROM names) n3 GROUP BY n2.* ORDER BY 1,2;

-- Below are queries similar to the above using the "names" database but without sub-queries
-- While the above queries test that LP_DERIVED_COLUMN code path in tmpl_print_expression handles TABLENAME.ASTERISK correctly,
-- the below queries test that tmpl_column_reference handles TABLENAME.ASTERISK correctly.
DROP TABLE IF EXISTS TTA005A;
CREATE TABLE TTA005A (firstName VARCHAR, lastName VARCHAR);
INSERT INTO TTA005A (SELECT firstName,lastName from names);
DROP TABLE IF EXISTS TTA005B;
CREATE TABLE TTA005B (lastName VARCHAR);
INSERT INTO TTA005B (SELECT lastName from names);
SELECT COUNT(n1.*),COUNT(n2.*) FROM TTA005B n1 LEFT JOIN TTA005B n2 ON (n1.lastName = 'Burn') GROUP BY n2.*;
SELECT COUNT(n1.*),COUNT(n2.*) FROM TTA005A n1 LEFT JOIN TTA005B n2 ON (n1.firstName <= 'Acid') GROUP BY n2.* ORDER BY 1,2;
SELECT COUNT(n1.*),COUNT(n2.*),COUNT(n3.*) FROM TTA005A n1 LEFT JOIN TTA005B n2 ON n1.firstname <= 'Acid' LEFT JOIN TTA005B n3 ON n1.firstname <= 'Acid' GROUP BY n2.* ORDER BY 1,2,3;
SELECT COUNT(n1.*),COUNT(n2.*),COUNT(n3.*) FROM TTA005A n1 LEFT JOIN TTA005B n2 ON n1.firstname <= 'Acid' LEFT JOIN TTA005B n3 ON n1.firstname <= 'Acid' GROUP BY n3.* ORDER BY 1,2,3;
SELECT COUNT(n1.*),COUNT(n2.*),COUNT(n3.*) FROM TTA005A n1 LEFT JOIN TTA005B n2 ON n1.firstname <= 'Acid' LEFT JOIN TTA005B n3 ON n1.firstname <= 'Acid' GROUP BY n2.*,n3.* ORDER BY 1,2,3;
SELECT COUNT(n1.*),COUNT(n2.*),COUNT(n3.*) FROM TTA005A n1 LEFT JOIN TTA005B n2 ON n1.firstname <= 'Acid' cross join TTA005B n3 group by n2.* ORDER BY 1,2,3;
SELECT COUNT(n1.*),COUNT(n2.*),COUNT(n3.*) FROM TTA005A n1 LEFT JOIN TTA005B n2 ON n1.firstname <= 'Acid' cross join TTA005B n3 group by n2.*,n3.* ORDER BY 1,2,3;
SELECT COUNT(n1.*),COUNT(n2.*) FROM TTA005A n1 LEFT JOIN TTA005B n2 ON n1.firstname <= 'Acid' CROSS JOIN TTA005B n3 GROUP BY n2.* ORDER BY 1,2;

-- Below are queries from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/759#note_731897567 using the "names" database.
-- These are fancier queries generated by the TQG03 subtest that fail due to YDBOcto#759.
SELECT alias4.lastName,COUNT(alias2.*) FROM (select * from (values (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', NULL), (5, 'Zero', 'Cool')) as names(id,firstname,lastname)) AS names  LEFT JOIN names AS alias1 ON ((names.firstName IS NOT NULL)) LEFT JOIN (SELECT alias2.lastName FROM names alias2 ORDER BY alias2.lastName) AS alias2 ON ((alias1.firstName IS NULL) OR (alias1.lastName IS NULL)) RIGHT OUTER JOIN names AS alias4 ON (((alias2.lastName IS NOT NULL)) OR NOT ((alias2.lastName IS NOT NULL))) WHERE ((names.id IS NOT NULL) OR (names.firstName IS NULL) AND (NOT (names.firstName IS NULL))) GROUP BY alias2.*, alias4.lastName ORDER BY COUNT(alias2.*), alias4.lastName;

-- Below is a query from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/759#note_736104049
-- This tests ORDER BY with TABLENAME.ASTERISK where we expect 'Burn' to show up as the first row. Not 'Cool'.
SELECT t1.id, t1.lastName, t2.* FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL ORDER BY t2.*,t1.id;

-- Few more tests of ORDER BY with TABLENAME.ASTERISK when ASC/DESC are also involved.
SELECT t1.id, t1.lastName, t2.* FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') ORDER BY t2.* asc,t1.id asc;
SELECT t1.id, t1.lastName, t2.* FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') ORDER BY t2.* asc,t1.id desc;
SELECT t1.id, t1.lastName, t2.* FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') ORDER BY t2.* desc,t1.id asc;
SELECT t1.id, t1.lastName, t2.* FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') ORDER BY t2.* desc,t1.id desc;
SELECT t1.id, t1.lastName, t2.* FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') ORDER BY t1.id asc,t2.* asc;
SELECT t1.id, t1.lastName, t2.* FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') ORDER BY t1.id desc,t2.* asc;
SELECT t1.id, t1.lastName, t2.* FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') ORDER BY t1.id asc,t2.* desc;
SELECT t1.id, t1.lastName, t2.* FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') ORDER BY t1.id desc,t2.* desc;

-- Below is a query from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/759#note_742645558
-- This tests ORDER BY when IS NOT NULL or IS NULL usages are done on TABLENAME.ASTERISK
SELECT 1 FROM names n1 GROUP BY n1.* ORDER BY n1.* IS NOT NULL;
SELECT 1 FROM names n1 GROUP BY n1.* ORDER BY n1.* IS NULL;
SELECT 1 FROM names n1 ORDER BY n1.* IS NOT NULL;
SELECT 1 FROM names n1 ORDER BY n1.* IS NULL;
SELECT 1 FROM (SELECT * FROM names) n1 GROUP BY n1.* ORDER BY n1.* IS NOT NULL;
SELECT 1 FROM (SELECT * FROM names) n1 GROUP BY n1.* ORDER BY n1.* IS NULL;
SELECT 1 FROM (SELECT * FROM names) n1 ORDER BY n1.* IS NOT NULL;
SELECT 1 FROM (SELECT * FROM names) n1 ORDER BY n1.* IS NULL;

