#################################################################
#								#
# Copyright (c) 2019-2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select * from names where firstname BETWEEN 'Acid' and 'Lord';
select * from names where firstname BETWEEN 'Acid' and 'Lorc';
select * from names where firstname BETWEEN 'Acic' and 'Lorc';
select * from names where firstname BETWEEN 'Acie' and 'Lorc';
select * from names where firstname BETWEEN 'Acie' and 'Lord';
select * from names where firstname NOT BETWEEN 'Acid' and 'Lord';
select * from names where firstname NOT BETWEEN 'Acid' and 'Lorc';
select * from names where firstname NOT BETWEEN 'Acic' and 'Lorc';
select * from names where firstname NOT BETWEEN 'Acie' and 'Lorc';
select * from names where firstname NOT BETWEEN 'Acie' and 'Lord';
select n1.id,n2.id from names n1 inner join names n2 on n1.id = n2.id AND n1.id BETWEEN 2 and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id = n2.id AND n1.id NOT BETWEEN 2 and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id = n2.id OR n1.id BETWEEN 2 and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id = n2.id OR n1.id NOT BETWEEN 2 and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id BETWEEN 2 and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id NOT BETWEEN 2 and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id BETWEEN 2 and n2.id;
select n1.id,n2.id from names n1 inner join names n2 on n1.id NOT BETWEEN 2 and n2.id;
select n1.id,n2.id from names n1 inner join names n2 on n1.id BETWEEN n2.id and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id NOT BETWEEN n2.id and 4;
select * from names n1 where n1.id BETWEEN 2 and 4;
select * from names n1 where n1.id NOT BETWEEN 2 and 4;
select * from names n1 where n1.id > 2  OR n1.id BETWEEN 1 and 4;
select * from names n1 where n1.id > 2  OR n1.id NOT BETWEEN 1 and 4;
select * from names n1 where n1.id > 2  AND n1.id BETWEEN 1 and 4;
select * from names n1 where n1.id > 2  AND n1.id NOT BETWEEN 1 and 4;
select * from (select * from (select * from names n1 where EXISTS (select n2.id from names n2 where n2.id BETWEEN n1.id and n1.id + 2)) n3) n4;
select * from (select * from (select * from names n1 where EXISTS (select n2.id from names n2 where n2.id NOT BETWEEN n1.id and n1.id + 2)) n3) n4;

-- OCTO843 : Below tests https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/843#note_967932702 (produces zero output though)
select n1.firstname,n2.lastname from names n1 inner join names n2 on (n1.firstname = n2.lastname) where n2.lastname between 'a' and 'b';

-- OCTO843 : Below also tests YDBOcto#843 (but produces some non-zero output)
 select n1.firstname,n2.lastname from names n1 inner join names n2 on (n1.firstname < n2.lastname) where n2.lastname between 'Cereal' and 'Joey';


-- ############################################################################################################
-- YDBOcto#1020: key fixing walks a range from one bound to the other. The rows of names: 0 Zero Cool, 1 Acid Burn,
-- 2 Cereal Killer, 3 Lord Nikon, 4 Joey (no lastname), 5 Zero Cool.
-- [walk 3 to 4] the range has 2 rows, so LIMIT 2 gives both
SELECT * FROM names WHERE id BETWEEN 3 AND 4 LIMIT 2;
-- [walk 1 to 3] 1 Acid Burn and 2 Cereal Killer
SELECT * FROM names WHERE id >= 1 AND id < 3;
-- [walk 3 down to 1] 3 Lord Nikon and 2 Cereal Killer
SELECT * FROM names WHERE id > 1 AND id <= 3 ORDER BY id DESC;
-- [walk 1 to 4] 1 Acid Burn and 2 Cereal Killer
SELECT * FROM names WHERE id BETWEEN 1 AND 4 ORDER BY id LIMIT 2;
-- [walk 4 down to 1] 4 Joey and 3 Lord Nikon
SELECT * FROM names WHERE id BETWEEN 1 AND 4 ORDER BY id DESC LIMIT 2;
-- [walk 4 to 1] no row
SELECT * FROM names WHERE id BETWEEN 4 AND 1;
-- [walk 'Joey' to 'Lord' of the cross reference of firstname] the range has 2 rows, so LIMIT 2 gives both
SELECT * FROM names WHERE firstname BETWEEN 'Joey' AND 'Lord' LIMIT 2;
-- [walk 'Acid' to 'Zero' of the cross reference of firstname] both bounds not included: Cereal, Joey and Lord
SELECT * FROM names WHERE firstname > 'Acid' AND firstname < 'Zero';
-- [walk 'Acid' to 'Lord' of the cross reference of firstname] Acid, Cereal, Joey and Lord
SELECT * FROM names WHERE firstname BETWEEN 'Acid' AND 'Lord' ORDER BY firstname;
-- [walk 'Lord' down to 'Acid' of the cross reference of firstname] Lord and Joey
SELECT * FROM names WHERE firstname BETWEEN 'Acid' AND 'Lord' ORDER BY firstname DESC LIMIT 2;
-- [walk 'Lord' to 'Joey' of the cross reference of firstname] no row
SELECT * FROM names WHERE firstname BETWEEN 'Lord' AND 'Joey';
-- [walk 'A' to 'M' of the cross reference of lastname] Burn, Cool, Cool and Killer. Joey has no lastname.
SELECT * FROM names WHERE lastname BETWEEN 'A' AND 'M';
-- [walk n1.id to 3 in a sub-query] for each id, the number of ids from it to 3: 4, 3, 2, 1, 0 and 0
SELECT n1.id, (SELECT COUNT(*) FROM names n2 WHERE n2.id BETWEEN n1.id AND 3) FROM names n1;
-- [walk -1 to 2] bounds with a sign: 0, 1 and 2
SELECT * FROM names WHERE id BETWEEN -1 AND +2;
-- [walk -1 to 2] both bounds not included: 0 and 1
SELECT * FROM names WHERE id > -1 AND id < 2;
-- [walk -3 to -1] no row
SELECT * FROM names WHERE id BETWEEN -3 AND -1;
-- [walk 1 to 2, and 4 to 5] an OR of two ranges: 1, 2, 4 and 5
SELECT * FROM names WHERE id BETWEEN 1 AND 2 OR id BETWEEN 4 AND 5;
-- [walk 'A' to 'D', and 'L' to 'M' of the cross reference of firstname] an OR of two ranges: Acid, Cereal and Lord
SELECT * FROM names WHERE firstname BETWEEN 'A' AND 'D' OR firstname BETWEEN 'L' AND 'M';
-- [walk to 4] a range OR another condition, ANDed with a bound: 0, 1, 2 and 4
SELECT * FROM names WHERE (id BETWEEN 0 AND 2 OR firstname = 'Joey') AND id <= 4;
-- [walk n1.lo to n2.hi] the two bounds come from two other tables: ids 1 to 4
SELECT n1.id, n2.id, n3.id FROM names n1 INNER JOIN names n2 ON n2.id = 4 INNER JOIN names n3 ON n3.id >= n1.id AND n3.id <= n2.id WHERE n1.id = 1;
-- [walk 'A' to 'Z' of the cross reference of firstname] DISTINCT: Acid, Cereal, Joey, Lord and Zero
SELECT DISTINCT firstname FROM names WHERE firstname BETWEEN 'A' AND 'Z' ORDER BY firstname;
-- [walk 'A' to 'Zz' of the cross reference of firstname] GROUP BY: Zero has 2 rows
SELECT firstname, COUNT(*) FROM names WHERE firstname BETWEEN 'A' AND 'Zz' GROUP BY firstname ORDER BY firstname;
-- [walk 0 to 3, and 2 to 5] UNION of two ranges: 0 to 5
SELECT * FROM names WHERE id BETWEEN 0 AND 3 UNION SELECT * FROM names WHERE id BETWEEN 2 AND 5 ORDER BY id;
