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
-- Part A: a DATE first key column of a READONLY table. Its FOR loop goes through the dates in their stored form, so
-- a condition on it is served by its cross reference, which holds each date converted to UNIX time. 2024-01-15 is
-- stored as "2024-01-15" and as "2024-1-15": two rows, whose key has the same value.
SELECT XECUTE_M_CODE('set ^TWO24d1("2024-03-01")="a",^TWO24d1("2023-12-31")="b",^TWO24d1("2024-01-15")="c",^TWO24d1("2024-1-15")="d"');
SELECT XECUTE_M_CODE('zwrite ^TWO24d1');
CREATE TABLE TWO24d1 (k DATE PRIMARY KEY, v VARCHAR PIECE 1) GLOBAL "^TWO24d1(keys(""k""))" READONLY;
-- [no key column xref] the rows of the table, in M order: 2023-12-31|b, 2024-01-15|c, 2024-03-01|a and 2024-01-15|d
SELECT * FROM TWO24d1;
-- [xref on k] 2024-01-15|c and 2024-01-15|d
SELECT * FROM TWO24d1 WHERE k = date '2024-01-15';
-- [xref on k] from 2024-01-01: c, d and a
SELECT * FROM TWO24d1 WHERE k >= date '2024-01-01';

-- ############################################################################################################
-- Part B: a DATE key column after the first, of a READONLY table. Its cross reference holds each date converted to
-- UNIX time, and after it the subscripts of the global in their stored form. A condition on k2 probes or walks the
-- converted values, and reads k1 and k2, in their stored form, from under each one. 2024-01-15 is stored in two forms:
-- "2024-01-15" (k1 = 2) and "2024-1-15" (k1 = 3).
SELECT XECUTE_M_CODE('set ^TWO24d(1,"2024-03-01")="a",^TWO24d(1,"2023-12-31")="b",^TWO24d(2,"2024-01-15")="c",^TWO24d(3,"2024-1-15")="d",^TWO24d(4,"2024-01-16")="e"');
SELECT XECUTE_M_CODE('zwrite ^TWO24d');
CREATE TABLE TWO24d (k1 INTEGER, k2 DATE, v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TWO24d(keys(""k1""),keys(""k2""))" READONLY;
-- [no key column xref] the rows of the table, in M order: 1|2023-12-31|b, 1|2024-03-01|a, 2|2024-01-15|c,
-- 3|2024-01-15|d and 4|2024-01-16|e
SELECT * FROM TWO24d;
-- [xref on k2] both stored forms of 2024-01-15: 2|2024-01-15|c and 3|2024-01-15|d
SELECT * FROM TWO24d WHERE k2 = date '2024-01-15';
-- [xref on k2] 1|2023-12-31|b and 1|2024-03-01|a
SELECT * FROM TWO24d WHERE k2 IN (date '2023-12-31', date '2024-03-01');
-- [xref on k2] the walk starts after 2024-01-15: 4|2024-01-16|e and 1|2024-03-01|a
SELECT * FROM TWO24d WHERE k2 > date '2024-01-15';
-- [xref on k2] from 2024-01-01 to 2024-01-16: c, d and e
SELECT * FROM TWO24d WHERE k2 BETWEEN date '2024-01-01' AND date '2024-01-16';
-- [xref on k2] no row
SELECT * FROM TWO24d WHERE k2 = date '2025-01-01';
-- [no key column xref] k1, the key column before k2, has a condition: its loop runs once, and k2 is checked on each
-- row under it. 1|2023-12-31|b
SELECT * FROM TWO24d WHERE k1 = 1 AND k2 = date '2023-12-31';
-- [no key column xref] the table already has a cross reference, of v, for the condition that comes first: k2 is
-- checked on each row. 3|2024-01-15|d
SELECT * FROM TWO24d WHERE v = 'd' AND k2 = date '2024-01-15';

-- START '"2024-"' applies to the stored form: "2023-12-31" is before "2024-", so it is not a row, even though the
-- cross reference holds its date.
SELECT XECUTE_M_CODE('merge ^TWO24ds=^TWO24d');
CREATE TABLE TWO24ds (k1 INTEGER, k2 DATE START '"2024-"', v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TWO24ds(keys(""k1""),keys(""k2""))" READONLY;
-- [no key column xref] the rows of the table: 1|2024-03-01|a, 2|2024-01-15|c, 3|2024-01-15|d and 4|2024-01-16|e
SELECT * FROM TWO24ds;
-- [xref on k2] no row: 2023-12-31 is outside START
SELECT * FROM TWO24ds WHERE k2 = date '2023-12-31';
-- [xref on k2] the 4 rows of the table, not 2023-12-31
SELECT * FROM TWO24ds WHERE k2 >= date '2023-01-01';

-- SKIP and SKIPCONDITION apply to the stored form too. Each table drops "2024-1-15" (k1 = 3), so a condition on k2
-- finds only "2024-01-15" (k1 = 2), although the cross reference holds the date of both.
CREATE TABLE TWO24sk (k1 INTEGER, k2 DATE SKIP '"2024-1-15"', v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TWO24d(keys(""k1""),keys(""k2""))" READONLY;
CREATE TABLE TWO24sc (k1 INTEGER, k2 DATE SKIPCONDITION "$length(keys(""k2""))<10", v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TWO24d(keys(""k1""),keys(""k2""))" READONLY;
-- [no key column xref] the rows of each table: those of TWO24d but 3|2024-01-15|d
SELECT * FROM TWO24sk;
SELECT * FROM TWO24sc;
-- [xref on k2] 2|2024-01-15|c only, in each
SELECT * FROM TWO24sk WHERE k2 = date '2024-01-15';
SELECT * FROM TWO24sc WHERE k2 = date '2024-01-15';
-- [xref on k2] 2|2024-01-15|c and 4|2024-01-16|e, in each
SELECT * FROM TWO24sk WHERE k2 BETWEEN date '2024-01-15' AND date '2024-01-16';
SELECT * FROM TWO24sc WHERE k2 BETWEEN date '2024-01-15' AND date '2024-01-16';

-- ############################################################################################################
-- Part C: a BOOLEAN first key column of a READONLY table. TRUE is stored as 1 and as "Y", FALSE as 0 and as "f": four
-- rows, two for each value of the key. Its cross reference holds 0 or 1, converted from the stored form.
SELECT XECUTE_M_CODE('set ^TWO24b1(1)="a",^TWO24b1(0)="b",^TWO24b1("Y")="c",^TWO24b1("f")="d"');
SELECT XECUTE_M_CODE('zwrite ^TWO24b1');
CREATE TABLE TWO24b1 (k BOOLEAN PRIMARY KEY, v VARCHAR PIECE 1) GLOBAL "^TWO24b1(keys(""k""))" READONLY;
-- [no key column xref] the rows of the table, in M order: f|b, t|a, t|c and f|d
SELECT * FROM TWO24b1;
-- [xref on k] t|a and t|c
SELECT * FROM TWO24b1 WHERE k = true;
-- [xref on k] f|b and f|d
SELECT * FROM TWO24b1 WHERE k = false;
-- [xref on k] every row
SELECT * FROM TWO24b1 WHERE k IN (true, false);

-- ############################################################################################################
-- Part D: a BOOLEAN key column after the first, of a READONLY table. Its cross reference holds 0 or 1, converted from
-- the stored form: TRUE is stored as 1 (k1 = 1) and as "Y" (k1 = 2), FALSE as 0 (k1 = 1) and as "f" (k1 = 3).
SELECT XECUTE_M_CODE('set ^TWO24b(1,1)="a",^TWO24b(1,0)="b",^TWO24b(2,"Y")="c",^TWO24b(3,"f")="d"');
SELECT XECUTE_M_CODE('zwrite ^TWO24b');
CREATE TABLE TWO24b (k1 INTEGER, k2 BOOLEAN, v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TWO24b(keys(""k1""),keys(""k2""))" READONLY;
-- [no key column xref] the rows of the table, in M order: 1|f|b, 1|t|a, 2|t|c and 3|f|d
SELECT * FROM TWO24b;
-- [xref on k2] 1|t|a and 2|t|c
SELECT * FROM TWO24b WHERE k2 = true;
-- [xref on k2] 1|f|b and 3|f|d
SELECT * FROM TWO24b WHERE k2 = false;
-- [xref on k2] every row
SELECT * FROM TWO24b WHERE k2 IN (true, false);

-- ############################################################################################################
-- Part E: TIME and TIMESTAMP first key columns of a READONLY table, stored as text. 09:00:00 is stored as "09:00:00"
-- and as "9:00:00": two rows, whose key has the same value.
SELECT XECUTE_M_CODE('set ^TWO24t1("09:00:00")="a",^TWO24t1("9:00:00")="b",^TWO24t1("10:30:00")="c"');
SELECT XECUTE_M_CODE('zwrite ^TWO24t1');
CREATE TABLE TWO24t1 (k TIME PRIMARY KEY, v VARCHAR PIECE 1) GLOBAL "^TWO24t1(keys(""k""))" READONLY;
-- [no key column xref] the rows of the table, in M order: 09:00:00|a, 10:30:00|c and 09:00:00|b
SELECT * FROM TWO24t1;
-- [xref on k] 09:00:00|a and 09:00:00|b
SELECT * FROM TWO24t1 WHERE k = time '09:00:00';
-- [xref on k] 10:30:00|c
SELECT * FROM TWO24t1 WHERE k > time '09:00:00';
SELECT XECUTE_M_CODE('set ^TWO24ts1("2024-01-15 09:00:00")="a",^TWO24ts1("2024-1-15 9:00:00")="b",^TWO24ts1("2024-01-15 10:30:00")="c"');
SELECT XECUTE_M_CODE('zwrite ^TWO24ts1');
CREATE TABLE TWO24ts1 (k TIMESTAMP PRIMARY KEY, v VARCHAR PIECE 1) GLOBAL "^TWO24ts1(keys(""k""))" READONLY;
-- [no key column xref] the rows of the table, in M order: 2024-01-15 09:00:00|a, 2024-01-15 10:30:00|c and
-- 2024-01-15 09:00:00|b
SELECT * FROM TWO24ts1;
-- [xref on k] 2024-01-15 09:00:00|a and 2024-01-15 09:00:00|b
SELECT * FROM TWO24ts1 WHERE k = timestamp '2024-01-15 09:00:00';
-- [xref on k] a and b: up to 09:00:00
SELECT * FROM TWO24ts1 WHERE k <= timestamp '2024-01-15 09:00:00';

-- ############################################################################################################
-- Part F: TIME and TIMESTAMP key columns after the first, of a READONLY table, stored as text. 09:00:00 is stored as
-- "09:00:00" (k1 = 1) and as "9:00:00" (k1 = 2).
SELECT XECUTE_M_CODE('set ^TWO24t(1,"09:00:00")="a",^TWO24t(2,"9:00:00")="b",^TWO24t(3,"10:30:00")="c"');
SELECT XECUTE_M_CODE('zwrite ^TWO24t');
CREATE TABLE TWO24t (k1 INTEGER, k2 TIME, v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TWO24t(keys(""k1""),keys(""k2""))" READONLY;
-- [no key column xref] the rows of the table: 1|09:00:00|a, 2|09:00:00|b and 3|10:30:00|c
SELECT * FROM TWO24t;
-- [xref on k2] 1|09:00:00|a and 2|09:00:00|b
SELECT * FROM TWO24t WHERE k2 = time '09:00:00';
-- [xref on k2] 3|10:30:00|c
SELECT * FROM TWO24t WHERE k2 > time '09:00:00';
SELECT XECUTE_M_CODE('set ^TWO24ts(1,"2024-01-15 09:00:00")="a",^TWO24ts(2,"2024-1-15 9:00:00")="b",^TWO24ts(3,"2024-01-15 10:30:00")="c"');
SELECT XECUTE_M_CODE('zwrite ^TWO24ts');
CREATE TABLE TWO24ts (k1 INTEGER, k2 TIMESTAMP, v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TWO24ts(keys(""k1""),keys(""k2""))" READONLY;
-- [no key column xref] the rows of the table: 1|2024-01-15 09:00:00|a, 2|2024-01-15 09:00:00|b and
-- 3|2024-01-15 10:30:00|c
SELECT * FROM TWO24ts;
-- [xref on k2] 1|2024-01-15 09:00:00|a and 2|2024-01-15 09:00:00|b
SELECT * FROM TWO24ts WHERE k2 = timestamp '2024-01-15 09:00:00';
-- [xref on k2] 1 and 2: up to 09:00:00
SELECT * FROM TWO24ts WHERE k2 <= timestamp '2024-01-15 09:00:00';

-- ############################################################################################################
-- Part G: a comparison with a column of another table, IS NULL and IS NOT NULL, and a table kept in a local variable.
-- A comparison with a column fixes the key of the later table in the FROM list to the column of the earlier one, here
-- through the cross reference of the later table's key column. The earlier column's value is in the converted form, as
-- the cross reference is.
-- [xref on d1.k] each TWO24d row probes the cross reference of TWO24d1.k with its k2: a with a, b with b, and each of c
-- and d (2024-01-15) with both TWO24d1 rows of 2024-01-15, c and d: 6 rows
-- [sort] the ORDER BY items are columns of two tables (d.v, then d1.v).
SELECT d.k1, d.v, d1.v FROM TWO24d d INNER JOIN TWO24d1 d1 ON d.k2 = d1.k ORDER BY d.v, d1.v;
-- [xref on d.k2] the same tables the other way round: each TWO24d1 row probes the cross reference of TWO24d.k2: the
-- same 6 rows
-- [sort] the ORDER BY items are columns of two tables (d.v, then d1.v).
SELECT d.k1, d.v, d1.v FROM TWO24d1 d1 INNER JOIN TWO24d d ON d.k2 = d1.k ORDER BY d.v, d1.v;
-- [xref on d1.k] a LEFT JOIN: e (2024-01-16) has no TWO24d1 row, so its d1.v is NULL (shown empty)
-- [sort] the ORDER BY items are columns of two tables (d.v, then d1.v).
SELECT d.v, d1.v FROM TWO24d d LEFT JOIN TWO24d1 d1 ON d1.k = d.k2 ORDER BY d.v, d1.v;
-- A RIGHT or FULL JOIN outputs, after the rows that match, the rows of its right table that matched none: its second
-- half walks that table's own global, not the cross reference that the ON condition used.
-- [xref on d.k2] a RIGHT JOIN whose ON fixes the converted key of the right table to a constant: c and d match their
-- TWO24d1 row of the same v; a, b and e (other dates) come from the second half with a NULL d1.v (shown empty)
-- [sort] the ORDER BY items are columns of two tables (d.v, then d1.v).
SELECT d.v, d1.v FROM TWO24d1 d1 RIGHT JOIN TWO24d d ON d.k2 = date '2024-01-15' AND d1.v = d.v ORDER BY d.v, d1.v;
-- [xref on d.k2] a RIGHT JOIN on the converted keys of both tables: each TWO24d1 row probes the cross reference of
-- TWO24d.k2: the 6 pairs of the INNER JOIN above, then e (2024-01-16), which no TWO24d1 row matches, from the second
-- half with a NULL d1.v
-- [sort] the ORDER BY items are columns of two tables (d.v, then d1.v).
SELECT d.v, d1.v FROM TWO24d1 d1 RIGHT JOIN TWO24d d ON d1.k = d.k2 ORDER BY d.v, d1.v;
-- [xref on d.k2] a FULL JOIN on the converted keys, with d.v <> 'a' in the ON: b with b, and each of c and d with both
-- c and d; TWO24d1's a matches nothing (a NULL d.v), and TWO24d's a and e match nothing (a NULL d1.v)
-- [sort] the ORDER BY items are columns of two tables (d1.v, then d.v).
SELECT d1.v, d.v FROM TWO24d1 d1 FULL JOIN TWO24d d ON d1.k = d.k2 AND d.v <> 'a' ORDER BY d1.v, d.v;
-- [xref on k2] two tables in the FROM list: the IN on d.k2 uses the cross reference of k2 (the order of the tables is
-- chosen without it). c and d (2024-01-15), each with the TWO24b row of the same v: c with 2|t|c, d with 3|f|d
-- [sort] TWO24d already uses a cross reference, of k2, for the IN: a table gets one per query, so the ORDER BY on
-- d.v cannot walk the cross reference of v.
SELECT d.v, b.k1, b.k2 FROM TWO24d d, TWO24b b WHERE d.k2 IN (date '2024-01-15') AND b.v = d.v ORDER BY 1;
-- [no key column xref] a key column is never NULL: no row, and IS NULL is checked on each row
SELECT * FROM TWO24d WHERE k2 IS NULL;
-- [no key column xref] a key column is never NULL: IS NOT NULL is always true, and is not checked. The 5 rows
SELECT * FROM TWO24d WHERE k2 IS NOT NULL;
-- [xref on d1.k] the same after a LEFT JOIN, where d1.k is NULL for e: IS NOT NULL stays, and drops e
-- [sort] the ORDER BY items are columns of two tables (d.v, then d1.v).
SELECT d.v, d1.v FROM TWO24d d LEFT JOIN TWO24d1 d1 ON d1.k = d.k2 WHERE d1.k IS NOT NULL ORDER BY d.v, d1.v;
SELECT XECUTE_M_CODE('set lvd(1,"2024-01-15")="x",lvd(2,"2024-1-15")="y",lvd(3,"2024-03-01")="z"');
CREATE TABLE TWO24lv (k1 INTEGER, k2 DATE, v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "lvd(keys(""k1""),keys(""k2""))" READONLY;
-- [no key column xref] the rows of the table: 1|2024-01-15|x, 2|2024-01-15|y and 3|2024-03-01|z
SELECT * FROM TWO24lv;
-- [no key column xref] 1|2024-01-15|x and 2|2024-01-15|y
SELECT * FROM TWO24lv WHERE k2 = date '2024-01-15';
