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
-- Part A: the key column of a READONLY table with one key column. A range on it walks the table's global from one
-- bound to the other. The table has the 12 rows id = 1 to 12, with v = 10 * id.
SELECT XECUTE_M_CODE('for id=1:1:12 set ^TBO02a(id)=10*id');
SELECT XECUTE_M_CODE('zwrite ^TBO02a');
CREATE TABLE TBO02a (id INTEGER PRIMARY KEY, v INTEGER PIECE 1) GLOBAL "^TBO02a(keys(""id""))" READONLY;
-- [no walk] all 12 rows
SELECT * FROM TBO02a;
-- [walk 3 to 5] 3|30, 4|40 and 5|50. The walk starts at 3, steps to 4 and 5, and ends at 6, past 5: no condition is
-- left to check on each row. A walk that ended only at the end of the global would step through 6 to 12, and check
-- "id <= 5" on each row.
SELECT * FROM TBO02a WHERE id BETWEEN 3 AND 5;
-- [walk 3 to 5] the same range written as two conditions, the upper bound first, and as "5 >= id AND 3 <= id"
SELECT * FROM TBO02a WHERE id <= 5 AND id >= 3;
SELECT * FROM TBO02a WHERE 5 >= id AND 3 <= id;
-- [walk 3 to 5] "5 > id AND 3 < id", both bounds not included: 4|40
SELECT * FROM TBO02a WHERE 5 > id AND 3 < id;
-- [walk 3 to 5] the bounds included or not: 3|30 and 4|40; 4|40 and 5|50; 4|40
SELECT * FROM TBO02a WHERE id >= 3 AND id < 5;
SELECT * FROM TBO02a WHERE id > 3 AND id <= 5;
SELECT * FROM TBO02a WHERE id > 3 AND id < 5;
-- [walk 3 to 3] 3|30
SELECT * FROM TBO02a WHERE id BETWEEN 3 AND 3;
-- [walk 3 to 3] no row: the range is empty
SELECT * FROM TBO02a WHERE id > 3 AND id <= 3;
-- [walk 5 to 3] no row: the lower bound is above the upper one. The walk skips 5, which is past the end bound, and its
-- first step ends it. This query has the plan of "id BETWEEN 3 AND 5" above: Octo makes the literals of a query its
-- parameters, so the plan cannot assume the bounds are in order.
SELECT * FROM TBO02a WHERE id BETWEEN 5 AND 3;
-- [walk 3.5 to 5.5] 4|40 and 5|50: the walk starts at 3.5, which is not a row
SELECT * FROM TBO02a WHERE id BETWEEN 3.5 AND 5.5;
-- [walk -100 to 2] 1|10 and 2|20: the walk starts before the first row. -100 is the minus of the literal 100, which
-- the walk computes once, as it does a value.
SELECT * FROM TBO02a WHERE id BETWEEN -100 AND 2;
-- [walk -100 to 2] the same with "+2", the plus of the literal 2
SELECT * FROM TBO02a WHERE id >= -100 AND id <= +2;
-- [walk -3 to -1] no row: no id is negative
SELECT * FROM TBO02a WHERE id BETWEEN -3 AND -1;
-- [walk -100 to 2] the sign on the left of the comparison: "-100 <= id" is the same bound as "id >= -100"
SELECT * FROM TBO02a WHERE -100 <= id AND id <= 2;
-- [walk to 5] 1|10 to 5|50: "-v" is the minus of a column, not of a value, so "id > -v" is checked on each row
SELECT * FROM TBO02a WHERE id > -v AND id <= 5;
-- [walk to 5] the same with "-v" on the left of the comparison: "-v < id" is checked on each row
SELECT * FROM TBO02a WHERE -v < id AND id <= 5;
-- [walk to 2] a comparison of two values fixes no key, and is checked on each row: 1|10 and 2|20
SELECT * FROM TBO02a WHERE -1 < 0 AND id <= 2;
-- [walk 11 to 100] 11|110 and 12|120: the walk ends at the end of the global
SELECT * FROM TBO02a WHERE id BETWEEN 11 AND 100;
-- [walk 5 down to 3] ORDER BY id DESC walks the range backward, from 5: 5|50, 4|40 and 3|30, with no sort
SELECT * FROM TBO02a WHERE id BETWEEN 3 AND 5 ORDER BY id DESC;
-- [walk 5 down to 3] 3 not included: 5|50 and 4|40
SELECT * FROM TBO02a WHERE id > 3 AND id <= 5 ORDER BY id DESC;
-- [walk from 3, down] one upper bound in a descending walk: the walk starts at 3 (included or not) and goes down to the
-- first row: 3|30, 2|20 and 1|10; then 2|20 and 1|10
SELECT * FROM TBO02a WHERE id <= 3 ORDER BY id DESC;
SELECT * FROM TBO02a WHERE id < 3 ORDER BY id DESC;
-- [walk 3 to 9] LIMIT 2: 3|30 and 4|40. The walk stops at the LIMIT.
SELECT * FROM TBO02a WHERE id BETWEEN 3 AND 9 LIMIT 2;
-- [walk 9 down to 3] LIMIT 2: 9|90 and 8|80
SELECT * FROM TBO02a WHERE id BETWEEN 3 AND 9 ORDER BY id DESC LIMIT 2;
-- [walk 3 to 9] COUNT, MIN and MAX of the rows in the range: 7, 30 and 90
SELECT COUNT(*), MIN(v), MAX(v) FROM TBO02a WHERE id BETWEEN 3 AND 9;
-- [walk to 3, and walk from 10] NOT BETWEEN is "id < 3 OR id > 10". Octo runs each side of the OR as a query of its
-- own, a walk with one bound: 1|10, 2|20, 11|110 and 12|120
SELECT * FROM TBO02a WHERE id NOT BETWEEN 3 AND 10;
-- [walk from 3] two lower bounds: the walk starts at 3, the first one, and goes on to the end of the global. "id > 9"
-- is checked on each row: 10|100, 11|110 and 12|120
SELECT * FROM TBO02a WHERE id > 3 AND id > 9;
-- [walk to 5] two upper bounds: the walk starts at the first row and ends at 5. "id < 3" is checked on each row:
-- 1|10 and 2|20
SELECT * FROM TBO02a WHERE id < 5 AND id < 3;
-- [walk 3 to 5] a third bound is checked on each row: "id > 4" gives 5|50
SELECT * FROM TBO02a WHERE id >= 3 AND id <= 5 AND id > 4;
-- [walk 3 to 5] "id = 4" comes after the range, and is checked on each row: 4|40
SELECT * FROM TBO02a WHERE id BETWEEN 3 AND 5 AND id = 4;
-- [no walk] "id = 4" comes first and fixes the key to 4. The range is checked on that row: 4|40
SELECT * FROM TBO02a WHERE id = 4 AND id BETWEEN 3 AND 5;
-- [walk 3 to 9] "id IN (2, 4, 9)" comes after the range, and is checked on each row: 4|40 and 9|90
SELECT * FROM TBO02a WHERE id BETWEEN 3 AND 9 AND id IN (2, 4, 9);
-- [walk 3 to 9] "id <> 5" and the range on v are checked on each row: 4|40 and 6|60
SELECT * FROM TBO02a WHERE id BETWEEN 3 AND 9 AND id <> 5 AND v BETWEEN 40 AND 60;
-- [walk 3 to NULL] a bound that is NULL: no row, and no read
SELECT * FROM TBO02a WHERE id >= 3 AND id < NULL;
-- [no walk] "id >= NULL" is never true: no row, and no read
SELECT * FROM TBO02a WHERE id BETWEEN NULL AND 5;
-- [walk from 3] a bound that is an expression of the key column itself is checked on each row: 3|30, 4|40 and 5|50
SELECT * FROM TBO02a WHERE id >= 3 AND id + 0 <= 5;
-- [walk from 3] "v <= 60" is on another column: it does not end the walk of id, and is checked on each row: 3|30, 4|40,
-- 5|50 and 6|60
SELECT * FROM TBO02a WHERE id >= 3 AND v <= 60;

-- ############################################################################################################
-- Part B: an INTEGER column that is not a key column. A range on it walks its AIM cross reference from one bound to
-- the other: the values of v in order, and under each one the ids of its rows. Two rows have v = 30, and two have no v
-- (NULL).
SELECT XECUTE_M_CODE('set ^TBO02b(1)=50,^TBO02b(2)=10,^TBO02b(3)=30,^TBO02b(4)="",^TBO02b(5)=30,^TBO02b(6)=70');
SELECT XECUTE_M_CODE('set ^TBO02b(7)=20,^TBO02b(8)=90,^TBO02b(9)=40,^TBO02b(10)=60,^TBO02b(11)="",^TBO02b(12)=80');
SELECT XECUTE_M_CODE('zwrite ^TBO02b');
CREATE TABLE TBO02b (id INTEGER PRIMARY KEY, v INTEGER PIECE 1) GLOBAL "^TBO02b(keys(""id""))" READONLY;
-- [no walk] all 12 rows
SELECT * FROM TBO02b;
-- [walk 30 to 60] in the order of v: 3|30, 5|30, 9|40, 1|50 and 10|60.
SELECT * FROM TBO02b WHERE v BETWEEN 30 AND 60;
-- [walk 30 to 60] the bounds not included: 9|40 and 1|50
SELECT * FROM TBO02b WHERE v > 30 AND v < 60;
-- [walk 60 to 30] no row
SELECT * FROM TBO02b WHERE v BETWEEN 60 AND 30;
-- [walk 35 to 55] bounds that are not values of v: 9|40 and 1|50
SELECT * FROM TBO02b WHERE v BETWEEN 35 AND 55;
-- [walk 30 to 60] ORDER BY v DESC: the plan sorts the rows of the walk. (The cross reference of an INTEGER column of a
-- READONLY table is not in SQL order when the table holds a value that is not a canonical number, e.g. 010, YDBOcto#1168.
-- Part H walks one in descending order.) 10|60, 1|50, 9|40, then the two rows with 30
SELECT * FROM TBO02b WHERE v BETWEEN 30 AND 60 ORDER BY v DESC;
-- [walk 30 to 60] LIMIT 2, after the sort: 10|60 and 1|50
SELECT * FROM TBO02b WHERE v BETWEEN 30 AND 60 ORDER BY v DESC LIMIT 2;
-- [walk to 30] one bound: the walk starts at the first value of v, and ends at 30. The NULL values of v are not in a
-- range: 2|10, 7|20, 3|30 and 5|30
SELECT * FROM TBO02b WHERE v <= 30;
-- [walk 30 to NULL] a bound that is NULL: no row
SELECT * FROM TBO02b WHERE v > 30 AND v < NULL;
-- [walk 30 to 60] the range on v comes first and walks its cross reference. The range on id is checked on each row:
-- 3|30, 5|30 and 1|50
SELECT * FROM TBO02b WHERE v BETWEEN 30 AND 60 AND id BETWEEN 1 AND 5;
-- [walk 1 to 5] the range on id comes first and walks the table's global. The range on v is checked on each row:
-- 1|50, 3|30 and 5|30
SELECT * FROM TBO02b WHERE id BETWEEN 1 AND 5 AND v BETWEEN 30 AND 60;

-- ############################################################################################################
-- Part C: a VARCHAR column that is not a key column. Its AIM cross reference holds each value as "#"_value, so the
-- values are in SQL order there, including those that look like numbers: '10' < '2' < '9' < 'Apple' < 'apple'.
SELECT XECUTE_M_CODE('set ^TBO02c(1)="apple",^TBO02c(2)="10",^TBO02c(3)="9",^TBO02c(4)="banana",^TBO02c(5)=""');
SELECT XECUTE_M_CODE('set ^TBO02c(6)="cherry",^TBO02c(7)="2",^TBO02c(8)="date",^TBO02c(9)="Apple",^TBO02c(10)="fig"');
SELECT XECUTE_M_CODE('set ^TBO02c(11)="banana",^TBO02c(12)="grape"');
SELECT XECUTE_M_CODE('zwrite ^TBO02c');
CREATE TABLE TBO02c (id INTEGER PRIMARY KEY, s VARCHAR PIECE 1) GLOBAL "^TBO02c(keys(""id""))" READONLY;
-- [no walk] all 12 rows. Row 5 has no s (NULL).
SELECT * FROM TBO02c;
-- [walk 'b' to 'd'] 4|banana, 11|banana and 6|cherry ('date' is after 'd').
SELECT * FROM TBO02c WHERE s BETWEEN 'b' AND 'd';
-- [walk '1' to '9'] the values that look like numbers are compared as strings: 2|10, 7|2 and 3|9
SELECT * FROM TBO02c WHERE s BETWEEN '1' AND '9';
-- [walk '10' to 'apple'] both bounds not included: 7|2, 3|9 and 9|Apple
SELECT * FROM TBO02c WHERE s > '10' AND s < 'apple';
-- [walk 'g' down to 'c'] ORDER BY s DESC walks backward, from 'g': 10|fig, 8|date and 6|cherry
SELECT * FROM TBO02c WHERE s BETWEEN 'c' AND 'g' ORDER BY s DESC;
-- [walk 'fig' down to 'cherry'] 'cherry' not included: 10|fig and 8|date
SELECT * FROM TBO02c WHERE s > 'cherry' AND s <= 'fig' ORDER BY s DESC;
-- [walk 'fig' to 'c'] no row
SELECT * FROM TBO02c WHERE s > 'fig' AND s < 'c';
-- [walk 'c' down to 'fig'] no row, in descending order
SELECT * FROM TBO02c WHERE s > 'fig' AND s < 'c' ORDER BY s DESC;
-- [walk from 'date', down] one bound in a descending walk: the walk starts at the last value: 12|grape, 10|fig, 8|date
SELECT * FROM TBO02c WHERE s >= 'date' ORDER BY s DESC;
-- [walk from 'date'] one lower bound in an ascending walk, included or not: 8|date, 10|fig and 12|grape; then 10|fig and
-- 12|grape
SELECT * FROM TBO02c WHERE s >= 'date' ORDER BY s;
SELECT * FROM TBO02c WHERE s > 'date' ORDER BY s;
-- [walk from 'apple', down] one upper bound in a descending walk, included or not: 1|apple, 9|Apple, 3|9, 7|2 and 2|10;
-- then 9|Apple, 3|9, 7|2 and 2|10
SELECT * FROM TBO02c WHERE s <= 'apple' ORDER BY s DESC;
SELECT * FROM TBO02c WHERE s < 'apple' ORDER BY s DESC;
-- [walk '' to 'b'] a bound that is the empty string, which Octo takes as NULL: no row. The same range with the bound
-- on each row (s || '' is not the column, so its cross reference is not used) gives no row either.
SELECT * FROM TBO02c WHERE s >= '' AND s < 'b';
SELECT * FROM TBO02c WHERE s || '' >= '' AND s || '' < 'b';
-- [walk 'a' to 'bananas'] the range comes first, the LIKE is checked on each row: 1|apple
SELECT * FROM TBO02c WHERE s >= 'a' AND s < 'bananas' AND s LIKE 'a%';
-- [walk 'b%'] the LIKE comes first and walks the values that start with 'b'. The range is checked on each row:
-- 4|banana and 11|banana
SELECT * FROM TBO02c WHERE s LIKE 'b%' AND s >= 'a' AND s < 'bananas';

-- ############################################################################################################
-- Part D: a table with two key columns, g and k. A range on k, the key column after the first, walks the AIM cross
-- reference of k (YDBOcto#660), from one bound to the other. With a condition on g, k is walked in the table's global
-- under each g instead. The table has the rows g = 1 to 3, k = 1 to 10, with v = 100 * g + k.
SELECT XECUTE_M_CODE('for g=1:1:3 for k=1:1:10 set ^TBO02d(g,k)=100*g+k');
SELECT XECUTE_M_CODE('zwrite ^TBO02d(1,*)');
CREATE TABLE TBO02d (g INTEGER, k INTEGER, v INTEGER PIECE 1, PRIMARY KEY (g, k)) GLOBAL "^TBO02d(keys(""g""),keys(""k""))" READONLY;
-- [no walk] all 30 rows
SELECT * FROM TBO02d;
-- [walk 4 to 6 of the cross reference of k] 9 rows: g = 1 to 3 with k = 4, then with k = 5, then with k = 6.
SELECT * FROM TBO02d WHERE k BETWEEN 4 AND 6;
-- [walk 8 to 9 of the cross reference of k] 1|9|109, 2|9|209 and 3|9|309
SELECT * FROM TBO02d WHERE k > 8 AND k <= 9;
-- [walk 4 to 6 of the cross reference of k] ORDER BY k DESC: the rows with k = 6, then 5, then 4. The plan sorts them,
-- as in Part B (YDBOcto#1168). Part H walks the cross reference of a key column in descending order.
SELECT * FROM TBO02d WHERE k BETWEEN 4 AND 6 ORDER BY k DESC;
-- [walk 4 to 6 under g = 2] 2|4|204, 2|5|205 and 2|6|206
SELECT * FROM TBO02d WHERE g = 2 AND k BETWEEN 4 AND 6;
-- [walk 2 to 3, and 9 to 10 under each] both key columns are walked in the table's global, each from one bound to the
-- other: 2|9|209, 2|10|210, 3|9|309 and 3|10|310
SELECT * FROM TBO02d WHERE g BETWEEN 2 AND 3 AND k BETWEEN 9 AND 10;

-- ############################################################################################################
-- Part E: a FileMan TIMESTAMP key column after the first, under a constant subscript. Its AIM cross reference holds
-- each date converted to UNIX time, and, after it, the subscripts of the global in their stored form. A range on the
-- date walks the converted values from one bound to the other. 3241115.08 is 2024-11-15 08:00:00. Patient p has its appointments p seconds after the hour.
SELECT XECUTE_M_CODE('for p=1:1:3 for d=3241114.09,3241115.08,3241115.14,3241116.1,3241117.11 set ^TBO02e(p,"S",d+(p/1000000))="p"_p');
SELECT XECUTE_M_CODE('zwrite ^TBO02e');
CREATE TABLE TBO02e (p INTEGER, appt TIMESTAMP(FILEMAN), status VARCHAR PIECE 1, PRIMARY KEY (p, appt)) GLOBAL "^TBO02e(keys(""p""),""S"",keys(""appt""))" READONLY;
-- [no walk] all 15 rows
SELECT * FROM TBO02e;
-- [walk the day 2024-11-15 of the cross reference of appt] the 6 appointments of that day, in date order.
SELECT * FROM TBO02e WHERE appt >= timestamp '2024-11-15 00:00:00' AND appt < timestamp '2024-11-16 00:00:00';
-- [walk 2024-11-16 down to 2024-11-15 of the cross reference of appt] LIMIT 1: the last appointment of 2024-11-15,
-- 3|2024-11-15 14:00:03|p3
SELECT * FROM TBO02e WHERE appt BETWEEN timestamp '2024-11-15 00:00:00' AND timestamp '2024-11-16 00:00:00' ORDER BY appt DESC LIMIT 1;

-- ############################################################################################################
-- Part F: a DATE first key column of a READONLY table, stored as text in more than one form ("2024-01-15" and
-- "2024-1-15"). A range on it walks its AIM cross reference, which holds each date converted to UNIX time.
SELECT XECUTE_M_CODE('set ^TBO02f("2024-01-14")="a",^TBO02f("2024-01-15")="b",^TBO02f("2024-1-15")="c",^TBO02f("2024-01-16")="d",^TBO02f("2024-02-01")="e"');
SELECT XECUTE_M_CODE('zwrite ^TBO02f');
CREATE TABLE TBO02f (k DATE PRIMARY KEY, v VARCHAR PIECE 1) GLOBAL "^TBO02f(keys(""k""))" READONLY;
-- [no walk] all 5 rows, in M order
SELECT * FROM TBO02f;
-- [walk 2024-01-15 to 2024-01-16 of the cross reference of k] both stored forms of 2024-01-15, and 2024-01-16: b, c
-- and d.
SELECT * FROM TBO02f WHERE k BETWEEN date '2024-01-15' AND date '2024-01-16';
-- [walk 2024-01-14 to 2024-01-16 of the cross reference of k] both bounds not included: b and c
SELECT * FROM TBO02f WHERE k > date '2024-01-14' AND k < date '2024-01-16';

-- ############################################################################################################
-- Part G: a READONLY DATE column that is not a key column, stored as text. Its AIM cross reference holds each date
-- converted to UNIX time.
SELECT XECUTE_M_CODE('set ^TBO02g(1)="2024-01-15",^TBO02g(2)="2024-1-16",^TBO02g(3)="2024-01-17",^TBO02g(4)="",^TBO02g(5)="2024-01-18"');
SELECT XECUTE_M_CODE('zwrite ^TBO02g');
CREATE TABLE TBO02g (id INTEGER PRIMARY KEY, d DATE PIECE 1) GLOBAL "^TBO02g(keys(""id""))" READONLY;
-- [no walk] all 5 rows. Row 4 has no date (NULL).
SELECT * FROM TBO02g;
-- [walk 2024-01-16 to 2024-01-17 of the cross reference of d] 2|2024-01-16 and 3|2024-01-17. This query builds the
-- cross reference.
SELECT * FROM TBO02g WHERE d BETWEEN date '2024-01-16' AND date '2024-01-17';
-- [walk 2024-01-17 down to 2024-01-16 of the cross reference of d] 3|2024-01-17 and 2|2024-01-16
SELECT * FROM TBO02g WHERE d BETWEEN date '2024-01-16' AND date '2024-01-17' ORDER BY d DESC;

-- ############################################################################################################
-- Part H: READWRITE tables, whose global Octo keeps: an INTEGER key column and a NUMERIC column that is not a key
-- column, a table with two key columns, then a TIMESTAMP WITH TIME ZONE key column. A range on a key column walks the
-- table's global; on the NUMERIC column or on a key column after the first, its AIM cross reference. Then an UPDATE and a
-- DELETE of the rows of a range.
CREATE TABLE TBO02h (id INTEGER PRIMARY KEY, n NUMERIC);
INSERT INTO TBO02h VALUES (1, 1.5), (2, 2.25), (3, -1), (4, 10), (5, 2.5), (6, 3), (7, NULL), (8, 0);
-- [no walk] all 8 rows
SELECT * FROM TBO02h;
-- [walk 3 to 6] 3|-1, 4|10, 5|2.5 and 6|3
SELECT * FROM TBO02h WHERE id BETWEEN 3 AND 6;
-- [walk 0 to 2.5 of the cross reference of n] in the order of n: 8|0, 1|1.5, 2|2.25 and 5|2.5. This query builds the
-- cross reference.
SELECT * FROM TBO02h WHERE n BETWEEN 0 AND 2.5;
-- [walk -1 to 2.25 of the cross reference of n] both bounds not included: 8|0 and 1|1.5
SELECT * FROM TBO02h WHERE n > -1 AND n < 2.25;
-- [walk 2.5 down to 0 of the cross reference of n] ORDER BY n DESC walks it backward, from 2.5, with no sort: 5|2.5,
-- 2|2.25, 1|1.5 and 8|0
SELECT * FROM TBO02h WHERE n BETWEEN 0 AND 2.5 ORDER BY n DESC;
CREATE TABLE TBO02hk (g INTEGER, k INTEGER, PRIMARY KEY (g, k));
INSERT INTO TBO02hk VALUES (1, 1), (1, 2), (1, 3), (1, 4), (2, 1), (2, 2), (2, 3), (2, 4), (3, 2), (3, 3);
-- [no walk] all 10 rows
SELECT * FROM TBO02hk;
-- [walk 3 down to 2 of the cross reference of k] ORDER BY k DESC walks the cross reference of the key column k
-- backward, from 3, with no sort: the rows with k = 3 (g = 1, 2 and 3), then those with k = 2. This query builds the
-- cross reference.
SELECT * FROM TBO02hk WHERE k BETWEEN 2 AND 3 ORDER BY k DESC;
-- [walk 2 to 3 of the cross reference of k] ORDER BY k: the rows with k = 2, then those with k = 3, with no sort
SELECT * FROM TBO02hk WHERE k >= 2 AND k < 4 ORDER BY k;
-- [walk 3 to 5] the UPDATE moves rows 3, 4 and 5 to 103, 104 and 105, past the end of the range
UPDATE TBO02h SET id = id + 100 WHERE id BETWEEN 3 AND 5;
SELECT * FROM TBO02h;
-- [walk 2 to 6] the DELETE removes rows 2 and 6, the rows left in the range
DELETE FROM TBO02h WHERE id BETWEEN 2 AND 6;
SELECT * FROM TBO02h;
CREATE TABLE TBO02tz (t TIMESTAMP WITH TIME ZONE PRIMARY KEY, v INTEGER);
INSERT INTO TBO02tz VALUES (timestamp with time zone '2024-01-15 10:00:00+00', 1), (timestamp with time zone '2024-01-15 12:00:00+00', 2), (timestamp with time zone '2024-01-15 14:00:00+00', 3), (timestamp with time zone '2024-01-16 10:00:00+00', 4);
-- [no walk] all 4 rows, shown in UTC (the test sets TZ=UTC)
SELECT t, v FROM TBO02tz;
-- [walk 11:00 to 14:00] 2024-01-15 12:00:00+00|2 and 2024-01-15 14:00:00+00|3
SELECT t, v FROM TBO02tz WHERE t BETWEEN timestamp with time zone '2024-01-15 11:00:00+00' AND timestamp with time zone '2024-01-15 14:00:00+00';
-- [walk 11:00 to 14:00] 14:00 not included: 2024-01-15 12:00:00+00|2
SELECT t, v FROM TBO02tz WHERE t >= timestamp with time zone '2024-01-15 11:00:00+00' AND t < timestamp with time zone '2024-01-15 14:00:00+00';
CREATE TABLE TBO02tm (t TIME PRIMARY KEY, v INTEGER);
INSERT INTO TBO02tm VALUES (time '08:00:00', 1), (time '12:00:00', 2), (time '18:00:00', 3), (time '23:00:00', 4);
-- [walk 09:00 to 18:00] a TIME key column: 12:00:00|2 and 18:00:00|3
SELECT * FROM TBO02tm WHERE t BETWEEN time '09:00:00' AND time '18:00:00';
-- Negative values: a NUMERIC column that is not a key column, and a NUMERIC key column
CREATE TABLE TBO02n (id INTEGER PRIMARY KEY, n NUMERIC);
INSERT INTO TBO02n VALUES (1, -5), (2, -1.5), (3, -1), (4, 0), (5, 0.5), (6, 1), (7, 2);
-- [walk -1.5 to 1 of the cross reference of n] in the order of n: 2|-1.5, 3|-1, 4|0, 5|.5 and 6|1
SELECT * FROM TBO02n WHERE n BETWEEN -1.5 AND 1;
-- [walk 1 down to -1.5 of the cross reference of n] ORDER BY n DESC: 6|1, 5|.5, 4|0, 3|-1 and 2|-1.5
SELECT * FROM TBO02n WHERE n BETWEEN -1.5 AND 1 ORDER BY n DESC;
CREATE TABLE TBO02nk (k NUMERIC PRIMARY KEY, v INTEGER);
INSERT INTO TBO02nk VALUES (-5, 1), (-1.5, 2), (-1, 3), (0, 4), (0.5, 5), (1, 6), (2, 7);
-- [walk -1.5 to 1] -1.5|2, -1|3, 0|4, .5|5 and 1|6
SELECT * FROM TBO02nk WHERE k BETWEEN -1.5 AND 1;
-- [walk 0.5 down to -2] both bounds not included, ORDER BY k DESC: 0|4, -1|3 and -1.5|2
SELECT * FROM TBO02nk WHERE k > -2 AND k < 0.5 ORDER BY k DESC;
-- [walk -1.5 to 1 of the cross reference of n] an UPDATE of the column that the walk goes through: the 5 rows of the
-- range move past its upper bound, and each one is updated once
UPDATE TBO02n SET n = n + 10 WHERE n BETWEEN -1.5 AND 1;
SELECT * FROM TBO02n;

-- ############################################################################################################
-- Part I: the bounds are columns of another table. Each row of TBO02i gives a range lo to hi of the ids of TBO02a
-- (Part A, ids 1 to 12). The key of TBO02a is walked from lo to hi for each row of TBO02i.
SELECT XECUTE_M_CODE('set ^TBO02i(1)="3|5",^TBO02i(2)="10|20",^TBO02i(3)="6|6",^TBO02i(4)="8|7"');
SELECT XECUTE_M_CODE('zwrite ^TBO02i');
CREATE TABLE TBO02i (n INTEGER PRIMARY KEY, lo INTEGER PIECE 1, hi INTEGER PIECE 2) GLOBAL "^TBO02i(keys(""n""))" READONLY;
-- [no walk] all 4 rows. Row 4 is an empty range (8 to 7).
SELECT * FROM TBO02i;
-- [walk lo to hi] an INNER JOIN whose ON has the range: 1|3, 1|4, 1|5, 2|10, 2|11, 2|12 and 3|6
SELECT i.n, a.id FROM TBO02i i INNER JOIN TBO02a a ON a.id BETWEEN i.lo AND i.hi;
-- [walk lo to hi] the same with the range in the WHERE
SELECT i.n, a.id FROM TBO02i i, TBO02a a WHERE a.id >= i.lo AND a.id <= i.hi;
-- [walk lo to hi] a LEFT JOIN: row 4 has no row of TBO02a in its range, and is joined with NULL: 4|
SELECT i.n, a.id FROM TBO02i i LEFT JOIN TBO02a a ON a.id BETWEEN i.lo AND i.hi;
-- [walk lo to 4] one bound is a column, the other a value: 1|3 and 1|4
SELECT i.n, a.id FROM TBO02i i INNER JOIN TBO02a a ON a.id >= i.lo AND a.id <= 4;
-- [walk lo to hi] a LEFT JOIN whose ON has one bound and whose WHERE has the other: 1|3, 1|4, 1|5, 2|10, 2|11, 2|12
-- and 3|6. The WHERE ends the walk of TBO02a at hi, but it stays a check on each row: the LEFT JOIN gives row 4 of
-- TBO02i (lo 8, hi 7, no id in its range) as 4|NULL, and only the WHERE drops it.
SELECT i.n, a.id FROM TBO02i i LEFT JOIN TBO02a a ON a.id >= i.lo WHERE a.id <= i.hi;
-- [walk lo to hi in a sub-query] the bounds come from the query around the sub-query: 1|3, 2|3, 3|1 and 4|0
SELECT i.n, (SELECT COUNT(*) FROM TBO02a a WHERE a.id BETWEEN i.lo AND i.hi) FROM TBO02i i;
-- [walk lo to hi in a sub-query] EXISTS, with both bounds not included: rows 1 (id 4) and 2 (ids 11 and 12). The
-- EXISTS itself is checked on each row of TBO02i.
SELECT i.n FROM TBO02i i WHERE EXISTS (SELECT 1 FROM TBO02a a WHERE a.id > i.lo AND a.id < i.hi);

-- ############################################################################################################
-- Part J: a table kept in a local variable, lvTBO02j. A range on its key column walks the local variable; on v, a cross
-- reference that is kept in a local variable too.
SELECT XECUTE_M_CODE('for id=1:1:8 set lvTBO02j(id)=(9-id)*10');
SELECT XECUTE_M_CODE('zwrite lvTBO02j');
CREATE TABLE TBO02j (id INTEGER PRIMARY KEY, v INTEGER PIECE 1) GLOBAL "lvTBO02j(keys(""id""))" READONLY;
-- [no walk] all 8 rows
SELECT * FROM TBO02j;
-- [walk 3 to 5] 3|60, 4|50 and 5|40
SELECT * FROM TBO02j WHERE id BETWEEN 3 AND 5;
-- [walk 30 to 50 of the cross reference of v] in the order of v: 6|30, 5|40 and 4|50
SELECT * FROM TBO02j WHERE v BETWEEN 30 AND 50;

-- ############################################################################################################
-- Part K: the keywords of a key column that decide which subscripts are rows: START and ENDPOINT, STARTINCLUDE, END,
-- SKIP and SKIPCONDITION. START 3 ENDPOINT 9 limits the table to the rows 4 to 9 of ^TBO02k (START is not
-- included without STARTINCLUDE). The walk of a range checks them on each value.
SELECT XECUTE_M_CODE('for id=1:1:12 set ^TBO02k(id)=10*id');
CREATE TABLE TBO02k (id INTEGER PRIMARY KEY START 3 ENDPOINT 9, v INTEGER PIECE 1) GLOBAL "^TBO02k(keys(""id""))" READONLY;
-- [no walk] the 6 rows 4 to 9
SELECT * FROM TBO02k;
-- [walk 1 to 5] 4|40 and 5|50
SELECT * FROM TBO02k WHERE id BETWEEN 1 AND 5;
-- [walk 8 to 20] 8|80 and 9|90
SELECT * FROM TBO02k WHERE id BETWEEN 8 AND 20;
-- [walk 1 to 3] no row: 3 is the START, which is not a row
SELECT * FROM TBO02k WHERE id BETWEEN 1 AND 3;
CREATE TABLE TBO02ki (id INTEGER PRIMARY KEY START 3 STARTINCLUDE ENDPOINT 9, v INTEGER PIECE 1) GLOBAL "^TBO02k(keys(""id""))" READONLY;
-- [walk 1 to 3] with STARTINCLUDE, 3 is a row: 3|30
SELECT * FROM TBO02ki WHERE id BETWEEN 1 AND 3;
-- An END condition on the key column: the table has the rows 1 to 6. A range on such a column is checked on each row.
CREATE TABLE TBO02ke (id INTEGER PRIMARY KEY END "(keys(""id"")="""")!(keys(""id"")>6)", v INTEGER PIECE 1) GLOBAL "^TBO02k(keys(""id""))" READONLY;
-- [no walk] 5|50 and 6|60
SELECT * FROM TBO02ke WHERE id BETWEEN 5 AND 8;
-- SKIP on the key column: the rows 1 to 12 of ^TBO02k except 4 and 5. The walk of a range skips them too.
CREATE TABLE TBO02ks (id INTEGER PRIMARY KEY SKIP '4,5', v INTEGER PIECE 1) GLOBAL "^TBO02k(keys(""id""))" READONLY;
-- [walk 3 to 6] 3|30 and 6|60
SELECT * FROM TBO02ks WHERE id BETWEEN 3 AND 6;
-- [walk 6 down to 3] ORDER BY id DESC: 6|60 and 3|30
SELECT * FROM TBO02ks WHERE id BETWEEN 3 AND 6 ORDER BY id DESC;
-- [walk 4 to 5] no row: both values are skipped
SELECT * FROM TBO02ks WHERE id BETWEEN 4 AND 5;
-- SKIPCONDITION on the key column: the odd ids are skipped, so the rows are the even ids 2 to 12
CREATE TABLE TBO02kc (id INTEGER PRIMARY KEY SKIPCONDITION "keys(""id"")#2", v INTEGER PIECE 1) GLOBAL "^TBO02k(keys(""id""))" READONLY;
-- [walk 3 to 8] 4|40, 6|60 and 8|80
SELECT * FROM TBO02kc WHERE id BETWEEN 3 AND 8;
-- [walk 3 to 3] no row: 3 is skipped
SELECT * FROM TBO02kc WHERE id BETWEEN 3 AND 3;
-- SKIP on a key column after the first, over the global of Part D: the rows with k = 5 are skipped
CREATE TABLE TBO02ds (g INTEGER, k INTEGER SKIP '5', v INTEGER PIECE 1, PRIMARY KEY (g, k)) GLOBAL "^TBO02d(keys(""g""),keys(""k""))" READONLY;
-- [walk 4 to 6 of k] 6 rows: g = 1 to 3 with k = 4 and with k = 6
SELECT * FROM TBO02ds WHERE k BETWEEN 4 AND 6;
-- [walk 4 to 6 under g = 2] 2|4|204 and 2|6|206
SELECT * FROM TBO02ds WHERE g = 2 AND k BETWEEN 4 AND 6;
-- ############################################################################################################
-- Part L: a range on a column of a sub-query in the FROM is checked on each row of the sub-query, as key fixing does
-- not fix a column of a sub-query (YDBOcto#355): 3|30, 4|40 and 5|50
SELECT * FROM (SELECT id, v FROM TBO02a) x WHERE x.id BETWEEN 3 AND 5;
-- The same range on a column of a VIEW, which Octo runs as a sub-query in the FROM, so the range is checked on each
-- row of the VIEW in the same way: 3|30, 4|40 and 5|50
CREATE VIEW TBO02av AS SELECT id, v FROM TBO02a;
SELECT * FROM TBO02av WHERE id BETWEEN 3 AND 5;

-- ############################################################################################################
-- Part M: an OR of two ranges. Octo runs each side of the OR as a plan of its own, so each side walks its range.
-- [walk 2 to 3, and 8 to 9] 2|20, 3|30, 8|80 and 9|90
SELECT * FROM TBO02a WHERE id BETWEEN 2 AND 3 OR id BETWEEN 8 AND 9;
-- [walk 10 to 30, and 70 to 80 of the cross reference of v] 2|10, 7|20, 3|30, 5|30, 6|70 and 12|80
SELECT * FROM TBO02b WHERE v BETWEEN 10 AND 30 OR v BETWEEN 70 AND 80;
