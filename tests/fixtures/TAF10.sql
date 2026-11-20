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
-- Part A: a READONLY table with two key columns. ^TAF10a(9,1) is not a row: it has no value, only ^TAF10a(9,1,"X")
-- below it. The table's global alone would give 9 as MAX(a); the cross reference of a, which indexes only the rows,
-- gives 2.
SELECT XECUTE_M_CODE('set ^TAF10a(1,5)="p",^TAF10a(1,7)="q",^TAF10a(2,3)="r",^TAF10a(2,4)="",^TAF10a(9,1,"X")="not a row"');
SELECT XECUTE_M_CODE('zwrite ^TAF10a');
CREATE TABLE TAF10a (a INTEGER, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10a(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|5|p, 1|7|q, 2|3|r and 2|4| (v NULL)
SELECT * FROM TAF10a;
-- [one value] the first key column: 1 and 2
SELECT MIN(a), MAX(a) FROM TAF10a;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a) FROM TAF10a WHERE 1 = 1;
-- [one value] the second key column: 3 and 7
SELECT MIN(b), MAX(b) FROM TAF10a;
-- [reads every row] the same, through every row
SELECT MIN(b), MAX(b) FROM TAF10a WHERE 1 = 1;
-- [one value] a non-key column, whose NULL is ignored: p and r
SELECT MIN(v), MAX(v) FROM TAF10a;
-- [reads every row] the same, through every row
SELECT MIN(v), MAX(v) FROM TAF10a WHERE 1 = 1;

-- ############################################################################################################
-- Part B: a READWRITE table with two key columns, the first a VARCHAR with numeric-looking values. M orders the
-- subscripts 9, 10, A (numbers first); SQL orders the values '10', '9', 'A'. The cross reference of k1 holds "#"_value,
-- which sorts as SQL does.
CREATE TABLE TAF10b (k1 VARCHAR, k2 INTEGER, v INTEGER, PRIMARY KEY (k1, k2));
INSERT INTO TAF10b VALUES ('9', 2, 20), ('10', 1, 10), ('A', 3, 30), ('A', 1, NULL);
-- The rows, in M order: 9|2|20, 10|1|10, A|1| and A|3|30
SELECT * FROM TAF10b;
-- [one value] 10 and A
SELECT MIN(k1), MAX(k1) FROM TAF10b;
-- [reads every row] the same, through every row
SELECT MIN(k1), MAX(k1) FROM TAF10b WHERE 1 = 1;
-- [one value] 1 and 3
SELECT MIN(k2), MAX(k2) FROM TAF10b;
-- [reads every row] the same, through every row
SELECT MIN(k2), MAX(k2) FROM TAF10b WHERE 1 = 1;
-- [one value] 10 and 30
SELECT MIN(v), MAX(v) FROM TAF10b;
-- [reads every row] the same, through every row
SELECT MIN(v), MAX(v) FROM TAF10b WHERE 1 = 1;
-- The cross references of k1, k2 and v exist now; AIM's triggers keep them up to date as rows change. Delete the rows
-- holding MAX(k1), MAX(k2) and MAX(v): each must leave its cross reference.
DELETE FROM TAF10b WHERE k1 = 'A';
-- The rows: 9|2|20 and 10|1|10
SELECT * FROM TAF10b;
-- [one value] k1 10 and 9, k2 1 and 2, v 10 and 20
SELECT MIN(k1), MAX(k1), MIN(k2), MAX(k2), MIN(v), MAX(v) FROM TAF10b;
-- [reads every row] the same, through every row
SELECT MIN(k1), MAX(k1), MIN(k2), MAX(k2), MIN(v), MAX(v) FROM TAF10b WHERE 1 = 1;
-- Insert rows with a new MIN(k1) ('1' is before '10' in SQL order), MAX(k1), MIN(k2), MAX(k2), MIN(v) and MAX(v)
INSERT INTO TAF10b VALUES ('B', 0, 5), ('1', 7, 70);
-- The rows, in M order: 1|7|70, 9|2|20, 10|1|10 and B|0|5
SELECT * FROM TAF10b;
-- [one value] k1 1 and B, k2 0 and 7, v 5 and 70
SELECT MIN(k1), MAX(k1), MIN(k2), MAX(k2), MIN(v), MAX(v) FROM TAF10b;
-- [reads every row] the same, through every row
SELECT MIN(k1), MAX(k1), MIN(k2), MAX(k2), MIN(v), MAX(v) FROM TAF10b WHERE 1 = 1;
-- Update the row holding MAX(v) to a value below every other
UPDATE TAF10b SET v = 1 WHERE k1 = '1';
-- The rows, in M order: 1|7|1, 9|2|20, 10|1|10 and B|0|5
SELECT * FROM TAF10b;
-- [one value] k1 1 and B, k2 0 and 7, v 1 and 20
SELECT MIN(k1), MAX(k1), MIN(k2), MAX(k2), MIN(v), MAX(v) FROM TAF10b;
-- [reads every row] the same, through every row
SELECT MIN(k1), MAX(k1), MIN(k2), MAX(k2), MIN(v), MAX(v) FROM TAF10b WHERE 1 = 1;

-- ############################################################################################################
-- Part C: date/time columns of a READONLY table. The global holds them as text, whose order is not that of the values;
-- the cross reference holds them converted to UNIX time, which is. The example of YDBOcto!1584: as text,
-- "2023-01-01T01:01:01" is after "2023-01-01 02:01:01", but it is the earlier timestamp.
SELECT XECUTE_M_CODE('set ^TAF10t("2023-01-01T01:01:01")="",^TAF10t("2023-01-01 02:01:01")=""');
SELECT XECUTE_M_CODE('zwrite ^TAF10t');
CREATE TABLE TAF10t (dob TIMESTAMP PRIMARY KEY) GLOBAL "^TAF10t" READONLY;
-- The rows, in M order: 2023-01-01 02:01:01 and 2023-01-01 01:01:01
SELECT * FROM TAF10t;
-- [one value] 2023-01-01 01:01:01 and 2023-01-01 02:01:01
SELECT MIN(dob), MAX(dob) FROM TAF10t;
-- [reads every row] the same, through every row
SELECT MIN(dob), MAX(dob) FROM TAF10t WHERE 1 = 1;

-- A DATE key column after the first, and a DATE non-key column, each with a value stored without its leading zeros. As
-- text, "2024-1-15" is after "2024-03-01" and "2024-7-4" after "2024-10-01", which are the later dates.
SELECT XECUTE_M_CODE('set ^TAF10d(1,"2024-03-01")="2024-10-01",^TAF10d(1,"2023-12-31")="2024-7-4",^TAF10d(2,"2024-1-15")="",^TAF10d(3,"2024-01-15")="2024-07-03"');
SELECT XECUTE_M_CODE('zwrite ^TAF10d');
CREATE TABLE TAF10d (k1 INTEGER, k2 DATE, d DATE PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TAF10d(keys(""k1""),keys(""k2""))" READONLY;
-- The rows, in M order: 1|2023-12-31|2024-07-04, 1|2024-03-01|2024-10-01, 2|2024-01-15| (d NULL) and
-- 3|2024-01-15|2024-07-03
SELECT * FROM TAF10d;
-- [one value] 2023-12-31 and 2024-03-01
SELECT MIN(k2), MAX(k2) FROM TAF10d;
-- [reads every row] the same, through every row
SELECT MIN(k2), MAX(k2) FROM TAF10d WHERE 1 = 1;
-- [one value] 2024-07-03 and 2024-10-01; the NULL d of k1 = 2 is ignored
SELECT MIN(d), MAX(d) FROM TAF10d;
-- [reads every row] the same, through every row
SELECT MIN(d), MAX(d) FROM TAF10d WHERE 1 = 1;

-- ############################################################################################################
-- Part D: the START, STARTINCLUDE, ENDPOINT, END, SKIP and SKIPCONDITION of the key columns of a table with two key
-- columns. AIM builds each cross reference over the range START:ENDPOINT of each key column, inclusive at both ends, so
-- it also indexes the START subscript, which is not a row (START is exclusive). SKIP and SKIPCONDITION are conditions,
-- not ranges: the cross reference indexes the subscripts they exclude too. MIN/MAX walk the key subscripts of the cross
-- reference under each value, past START, SKIP and SKIPCONDITION, until they find one in a row. END, also a condition,
-- stops the walk of its key column's subscripts, as it stops the table scan. One global for every table of this part;
-- the nodes ^TAF10s(1,9)="z" and ^TAF10s(2,3)="c" are the traps: each is the MIN or MAX of a column in the tables that
-- exclude it.
SELECT XECUTE_M_CODE('set ^TAF10s(1,1)="m",^TAF10s(1,9)="z",^TAF10s(2,3)="c",^TAF10s(2,5)="e",^TAF10s(3,2)="b",^TAF10s(3,8)="y",^TAF10s(4,0)="a",^TAF10s(4,6)="f"');
SELECT XECUTE_M_CODE('zwrite ^TAF10s');

-- START on the first key column: a = 1 is not in the table, so b = 9 and v = z are not either
CREATE TABLE TAF10s1 (a INTEGER START 1, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 2|3|c, 2|5|e, 3|2|b, 3|8|y, 4|0|a and 4|6|f
SELECT * FROM TAF10s1;
-- [one value] a 2 and 4, b 0 and 8, v a and y
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s1;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s1 WHERE 1 = 1;

-- START on the second key column: the node of b = 3 (v = c) is in the cross references of a, b and v, but
-- is not a row. Under each value, the walk seeds the b subscript with 3.
CREATE TABLE TAF10s2 (a INTEGER, b INTEGER START 3, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|9|z, 2|5|e, 3|8|y and 4|6|f
SELECT * FROM TAF10s2;
-- [one value] a 1 and 4, b 5 and 9, v e and z
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s2;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s2 WHERE 1 = 1;

-- START on both key columns
CREATE TABLE TAF10s3 (a INTEGER START 1, b INTEGER START 3, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 2|5|e, 3|8|y and 4|6|f
SELECT * FROM TAF10s3;
-- [one value] a 2 and 4, b 5 and 8, v e and y
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s3;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s3 WHERE 1 = 1;

-- START with STARTINCLUDE on the second key column: b = 3 is in the table, as AIM's range is
CREATE TABLE TAF10s4 (a INTEGER, b INTEGER START 3 STARTINCLUDE, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|9|z, 2|3|c, 2|5|e, 3|8|y and 4|6|f
SELECT * FROM TAF10s4;
-- [one value] a 1 and 4, b 3 and 9, v c and z
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s4;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s4 WHERE 1 = 1;

-- ENDPOINT on the first key column
CREATE TABLE TAF10s5 (a INTEGER ENDPOINT 3, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|m, 1|9|z, 2|3|c, 2|5|e, 3|2|b and 3|8|y
SELECT * FROM TAF10s5;
-- [one value] a 1 and 3, b 1 and 9, v b and z
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s5;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s5 WHERE 1 = 1;

-- ENDPOINT on the second key column
CREATE TABLE TAF10s6 (a INTEGER, b INTEGER ENDPOINT 5, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|m, 2|3|c, 2|5|e, 3|2|b and 4|0|a
SELECT * FROM TAF10s6;
-- [one value] a 1 and 4, b 0 and 5, v a and m
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s6;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s6 WHERE 1 = 1;

-- START and ENDPOINT on the first key column
CREATE TABLE TAF10s7 (a INTEGER START 1 ENDPOINT 3, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 2|3|c, 2|5|e, 3|2|b and 3|8|y
SELECT * FROM TAF10s7;
-- [one value] a 2 and 3, b 2 and 8, v b and y
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s7;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s7 WHERE 1 = 1;

-- START on the first key column past every row: only the nodes of a = 4 (the START) are in the cross references
CREATE TABLE TAF10s8 (a INTEGER START 4, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: none
SELECT * FROM TAF10s8;
-- [one value] no row: NULL for each
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s8;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s8 WHERE 1 = 1;

-- END on the first key column
CREATE TABLE TAF10s9 (a INTEGER END "(keys(""a"")="""")!(keys(""a"")>3)", b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|m, 1|9|z, 2|3|c, 2|5|e, 3|2|b and 3|8|y
SELECT * FROM TAF10s9;
-- [one value] a 1 and 3, b 1 and 9, v b and z
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s9;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s9 WHERE 1 = 1;

-- SKIP on the first key column
CREATE TABLE TAF10s10 (a INTEGER SKIP '4', b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|m, 1|9|z, 2|3|c, 2|5|e, 3|2|b and 3|8|y
SELECT * FROM TAF10s10;
-- [one value] a 1 and 3, b 1 and 9, v b and z
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s10;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s10 WHERE 1 = 1;

-- SKIPCONDITION on the second key column
CREATE TABLE TAF10s11 (a INTEGER, b INTEGER SKIPCONDITION "keys(""b"")=9", v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|m, 2|3|c, 2|5|e, 3|2|b, 3|8|y, 4|0|a and 4|6|f
SELECT * FROM TAF10s11;
-- [one value] a 1 and 4, b 0 and 8, v a and y
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s11;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s11 WHERE 1 = 1;

-- SKIP on the second key column: b = 9 (v = z) is not in the table
CREATE TABLE TAF10s16 (a INTEGER, b INTEGER SKIP '9', v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|m, 2|3|c, 2|5|e, 3|2|b, 3|8|y, 4|0|a and 4|6|f
SELECT * FROM TAF10s16;
-- [one value] a 1 and 4, b 0 and 8, v a and y
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s16;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s16 WHERE 1 = 1;
-- SKIPCONDITION on the second key column that names the first: the nodes under a = 4 are not in the table
CREATE TABLE TAF10s17 (a INTEGER, b INTEGER SKIPCONDITION "keys(""a"")=4", v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|m, 1|9|z, 2|3|c, 2|5|e, 3|2|b and 3|8|y
SELECT * FROM TAF10s17;
-- [one value] a 1 and 3, b 1 and 9, v b and z
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s17;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s17 WHERE 1 = 1;
-- START and SKIP on the first key column: a = 1 and a = 3 are not in the table
CREATE TABLE TAF10s18 (a INTEGER START 1 SKIP '3', b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 2|3|c, 2|5|e, 4|0|a and 4|6|f
SELECT * FROM TAF10s18;
-- [one value] a 2 and 4, b 0 and 6, v a and f
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s18;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s18 WHERE 1 = 1;
-- END on the second key column: past b = 5 under each a
CREATE TABLE TAF10s20 (a INTEGER, b INTEGER END "(keys(""b"")="""")!(keys(""b"")>5)", v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|m, 2|3|c, 2|5|e, 3|2|b and 4|0|a
SELECT * FROM TAF10s20;
-- [one value] a 1 and 4, b 0 and 5, v a and m
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s20;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s20 WHERE 1 = 1;
-- START and END on the first key column: a = 2 and a = 3
CREATE TABLE TAF10s21 (a INTEGER START 1 END "(keys(""a"")="""")!(keys(""a"")>3)", b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 2|3|c, 2|5|e, 3|2|b and 3|8|y
SELECT * FROM TAF10s21;
-- [one value] a 2 and 3, b 2 and 8, v b and y
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s21;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s21 WHERE 1 = 1;
-- START on the first key column, and v with a GLOBAL of its own. Its cross reference holds the key subscripts in the
-- order that GLOBAL lists them. The same node, with the keys in key order: MIN/MAX of v use the cross reference.
CREATE TABLE TAF10s12 (a INTEGER START 1, b INTEGER, v VARCHAR GLOBAL "^TAF10s(keys(""a""),keys(""b""))" PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 2|3|c, 2|5|e, 3|2|b, 3|8|y, 4|0|a and 4|6|f
SELECT * FROM TAF10s12;
-- [one value] a and y
SELECT MIN(v), MAX(v) FROM TAF10s12;
-- [reads every row] the same, through every row
SELECT MIN(v), MAX(v) FROM TAF10s12 WHERE 1 = 1;
-- v in a global that lists the keys in the other order, ^TAF10r(b,a), with the same values as ^TAF10s(a,b). A cross
-- reference of such a column does not hold v for the rows of the table (a WHERE does not use one either): MIN/MAX of v
-- read every row.
SELECT XECUTE_M_CODE('set ^TAF10r(1,1)="m",^TAF10r(9,1)="z",^TAF10r(3,2)="c",^TAF10r(5,2)="e",^TAF10r(2,3)="b",^TAF10r(8,3)="y",^TAF10r(0,4)="a",^TAF10r(6,4)="f"');
SELECT XECUTE_M_CODE('zwrite ^TAF10r');
CREATE TABLE TAF10s13 (a INTEGER START 1, b INTEGER, v VARCHAR GLOBAL "^TAF10r(keys(""b""),keys(""a""))" PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 2|3|c, 2|5|e, 3|2|b, 3|8|y, 4|0|a and 4|6|f
SELECT * FROM TAF10s13;
-- [reads every row] a and y
SELECT MIN(v), MAX(v) FROM TAF10s13;
-- [reads every row] the same, through every row
SELECT MIN(v), MAX(v) FROM TAF10s13 WHERE 1 = 1;

-- The same, with SKIP on b instead of START: MIN/MAX of v read every row
CREATE TABLE TAF10s19 (a INTEGER, b INTEGER SKIP '9', v VARCHAR GLOBAL "^TAF10r(keys(""b""),keys(""a""))" PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|m, 2|3|c, 2|5|e, 3|2|b, 3|8|y, 4|0|a and 4|6|f
SELECT * FROM TAF10s19;
-- [reads every row] a and y
SELECT MIN(v), MAX(v) FROM TAF10s19;
-- [reads every row] the same, through every row
SELECT MIN(v), MAX(v) FROM TAF10s19 WHERE 1 = 1;
-- The same, with no START, SKIP, SKIPCONDITION or END: MIN/MAX of v still read every row
CREATE TABLE TAF10s22 (a INTEGER, b INTEGER, v VARCHAR GLOBAL "^TAF10r(keys(""b""),keys(""a""))" PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|m, 1|9|z, 2|3|c, 2|5|e, 3|2|b, 3|8|y, 4|0|a and 4|6|f
SELECT * FROM TAF10s22;
-- [reads every row] a and z
SELECT MIN(v), MAX(v) FROM TAF10s22;
-- [reads every row] the same, through every row
SELECT MIN(v), MAX(v) FROM TAF10s22 WHERE 1 = 1;
-- v in a global with no subscript: the node ^TAF10n holds the v of every row. AIM cross references the subscripts of a
-- node, so MIN/MAX of v read every row.
SELECT XECUTE_M_CODE('set ^TAF10n="q"');
SELECT XECUTE_M_CODE('zwrite ^TAF10n');
SELECT XECUTE_M_CODE('set ^TAF10p(1)="z",^TAF10p(2)="c",^TAF10p(3)="b",^TAF10p(4)="y"');
SELECT XECUTE_M_CODE('zwrite ^TAF10p');
CREATE TABLE TAF10s14 (a INTEGER START 1, b INTEGER, v VARCHAR GLOBAL "^TAF10n" PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 2|3|q, 2|5|q, 3|2|q, 3|8|q, 4|0|q and 4|6|q
SELECT * FROM TAF10s14;
-- [reads every row] q and q
SELECT MIN(v), MAX(v) FROM TAF10s14;
-- [reads every row] the same, through every row
SELECT MIN(v), MAX(v) FROM TAF10s14 WHERE 1 = 1;
-- The same on a table with one key column (a non-key column, whose MIN/MAX read its cross reference since YDBOcto#617)
CREATE TABLE TAF10n1 (id INTEGER PRIMARY KEY, v VARCHAR GLOBAL "^TAF10n" PIECE 1) GLOBAL "^TAF10p(keys(""id""))" READONLY;
-- The rows: 1|q, 2|q, 3|q and 4|q
SELECT * FROM TAF10n1;
-- [reads every row] q and q
SELECT MIN(v), MAX(v) FROM TAF10n1;
-- [reads every row] the same, through every row
SELECT MIN(v), MAX(v) FROM TAF10n1 WHERE 1 = 1;
-- v in a global under the first key column only, ^TAF10p(a): its cross reference has no b subscript. z (a = 1) is not
-- in a row: MIN/MAX of v read every row.
CREATE TABLE TAF10s15 (a INTEGER START 1, b INTEGER, v VARCHAR GLOBAL "^TAF10p(keys(""a""))" PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 2|3|c, 2|5|c, 3|2|b, 3|8|b, 4|0|y and 4|6|y
SELECT * FROM TAF10s15;
-- [reads every row] b and y
SELECT MIN(v), MAX(v) FROM TAF10s15;
-- [reads every row] the same, through every row
SELECT MIN(v), MAX(v) FROM TAF10s15 WHERE 1 = 1;
-- The same global with no START: every a has rows, so MIN/MAX of v read the first and last value of its cross reference
CREATE TABLE TAF10s23 (a INTEGER, b INTEGER, v VARCHAR GLOBAL "^TAF10p(keys(""a""))" PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|z, 1|9|z, 2|3|c, 2|5|c, 3|2|b, 3|8|b, 4|0|y and 4|6|y
SELECT * FROM TAF10s23;
-- [one value] b and z
SELECT MIN(v), MAX(v) FROM TAF10s23;
-- [reads every row] the same, through every row
SELECT MIN(v), MAX(v) FROM TAF10s23 WHERE 1 = 1;

-- A table with three key columns, to walk more than one level of key subscripts under each value. The traps are
-- ^TAF10k(1,1,1)="p" (MIN(v) when c = 1 is a row) and ^TAF10k(1,2,0)="r".
SELECT XECUTE_M_CODE('set ^TAF10k(1,1,1)="p",^TAF10k(1,1,5)="q",^TAF10k(1,2,0)="r",^TAF10k(2,1,9)="s",^TAF10k(2,2,2)="t"');
SELECT XECUTE_M_CODE('zwrite ^TAF10k');
-- START on the third key column
CREATE TABLE TAF10k1 (a INTEGER, b INTEGER, c INTEGER START 1, v VARCHAR PIECE 1, PRIMARY KEY (a, b, c)) GLOBAL "^TAF10k(keys(""a""),keys(""b""),keys(""c""))" READONLY;
-- The rows: 1|1|5|q, 2|1|9|s and 2|2|2|t
SELECT * FROM TAF10k1;
-- [one value] a 1 and 2, b 1 and 2, c 2 and 9, v q and t
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(c), MAX(c), MIN(v), MAX(v) FROM TAF10k1;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(c), MAX(c), MIN(v), MAX(v) FROM TAF10k1 WHERE 1 = 1;
-- START on the first and third key columns
CREATE TABLE TAF10k2 (a INTEGER START 1, b INTEGER, c INTEGER START 1, v VARCHAR PIECE 1, PRIMARY KEY (a, b, c)) GLOBAL "^TAF10k(keys(""a""),keys(""b""),keys(""c""))" READONLY;
-- The rows: 2|1|9|s and 2|2|2|t
SELECT * FROM TAF10k2;
-- [one value] a 2 and 2, b 1 and 2, c 2 and 9, v s and t
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(c), MAX(c), MIN(v), MAX(v) FROM TAF10k2;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(c), MAX(c), MIN(v), MAX(v) FROM TAF10k2 WHERE 1 = 1;
-- START on the second (middle) key column
CREATE TABLE TAF10k3 (a INTEGER, b INTEGER START 1, c INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b, c)) GLOBAL "^TAF10k(keys(""a""),keys(""b""),keys(""c""))" READONLY;
-- The rows: 1|2|0|r and 2|2|2|t
SELECT * FROM TAF10k3;
-- [one value] a 1 and 2, b 2 and 2, c 0 and 2, v r and t
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(c), MAX(c), MIN(v), MAX(v) FROM TAF10k3;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(c), MAX(c), MIN(v), MAX(v) FROM TAF10k3 WHERE 1 = 1;

-- SKIP on the middle key column and START on the third
CREATE TABLE TAF10k4 (a INTEGER, b INTEGER SKIP '1', c INTEGER START 1, v VARCHAR PIECE 1, PRIMARY KEY (a, b, c)) GLOBAL "^TAF10k(keys(""a""),keys(""b""),keys(""c""))" READONLY;
-- The rows: 2|2|2|t
SELECT * FROM TAF10k4;
-- [one value] a 2 and 2, b 2 and 2, c 2 and 2, v t and t
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(c), MAX(c), MIN(v), MAX(v) FROM TAF10k4;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(c), MAX(c), MIN(v), MAX(v) FROM TAF10k4 WHERE 1 = 1;
-- END on the middle key column (past b = 1) and START on the third
CREATE TABLE TAF10k5 (a INTEGER, b INTEGER END "(keys(""b"")="""")!(keys(""b"")>1)", c INTEGER START 1, v VARCHAR PIECE 1, PRIMARY KEY (a, b, c)) GLOBAL "^TAF10k(keys(""a""),keys(""b""),keys(""c""))" READONLY;
-- The rows: 1|1|5|q and 2|1|9|s
SELECT * FROM TAF10k5;
-- [one value] a 1 and 2, b 1 and 1, c 5 and 9, v q and s
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(c), MAX(c), MIN(v), MAX(v) FROM TAF10k5;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(c), MAX(c), MIN(v), MAX(v) FROM TAF10k5 WHERE 1 = 1;
-- START on a READONLY DATE key column (the second) applies to the stored form, as M orders it: of "2024-03-01",
-- "2023-12-31", "2024-1-15" and "2024-01-15", only "2024-03-01" and "2024-1-15" are after "2024-01-15". The node of
-- k1 = 3 is the START: its d 2024-07-03 would be MIN(d).
CREATE TABLE TAF10ds (k1 INTEGER, k2 DATE START '"2024-01-15"', d DATE PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TAF10d(keys(""k1""),keys(""k2""))" READONLY;
-- The rows: 1|2024-03-01|2024-10-01 and 2|2024-01-15| (d NULL)
SELECT * FROM TAF10ds;
-- [one value] k2 2024-01-15 and 2024-03-01, d 2024-10-01 and 2024-10-01
SELECT MIN(k2), MAX(k2), MIN(d), MAX(d) FROM TAF10ds;
-- [reads every row] the same, through every row
SELECT MIN(k2), MAX(k2), MIN(d), MAX(d) FROM TAF10ds WHERE 1 = 1;

-- SKIP on the same READONLY DATE key column, which also applies to the stored form: "2024-1-15" (k1 = 2) is not in the
-- table, but "2024-01-15" (k1 = 3) is
CREATE TABLE TAF10dk (k1 INTEGER, k2 DATE SKIP '"2024-1-15"', d DATE PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TAF10d(keys(""k1""),keys(""k2""))" READONLY;
-- The rows: 1|2023-12-31|2024-07-04, 1|2024-03-01|2024-10-01 and 3|2024-01-15|2024-07-03
SELECT * FROM TAF10dk;
-- [one value] k1 1 and 3, k2 2023-12-31 and 2024-03-01, d 2024-07-03 and 2024-10-01
SELECT MIN(k1), MAX(k1), MIN(k2), MAX(k2), MIN(d), MAX(d) FROM TAF10dk;
-- [reads every row] the same, through every row
SELECT MIN(k1), MAX(k1), MIN(k2), MAX(k2), MIN(d), MAX(d) FROM TAF10dk WHERE 1 = 1;
-- START on a VARCHAR key column (the first), whose cross reference holds each value as "#"_value: the traps are the
-- nodes under k = "B" (the START), whose n = 1 and v = z would be MIN(n) and MAX(v)
SELECT XECUTE_M_CODE('set ^TAF10v("A",9)="x",^TAF10v("B",1)="y",^TAF10v("B",2)="z",^TAF10v("C",5)="w",^TAF10v("D",3)="u"');
SELECT XECUTE_M_CODE('zwrite ^TAF10v');
CREATE TABLE TAF10vs (k VARCHAR START '"B"', n INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (k, n)) GLOBAL "^TAF10v(keys(""k""),keys(""n""))" READONLY;
-- The rows: C|5|w and D|3|u
SELECT * FROM TAF10vs;
-- [one value] k C and D, n 3 and 5, v u and w
SELECT MIN(k), MAX(k), MIN(n), MAX(n), MIN(v), MAX(v) FROM TAF10vs;
-- [reads every row] the same, through every row
SELECT MIN(k), MAX(k), MIN(n), MAX(n), MIN(v), MAX(v) FROM TAF10vs WHERE 1 = 1;

-- START on both key columns of the same global: n = 3 (the START) puts D|3|u out of the table as well
CREATE TABLE TAF10vs2 (k VARCHAR START '"B"', n INTEGER START 3, v VARCHAR PIECE 1, PRIMARY KEY (k, n)) GLOBAL "^TAF10v(keys(""k""),keys(""n""))" READONLY;
-- The rows: C|5|w
SELECT * FROM TAF10vs2;
-- [one value] k C and C, n 5 and 5, v w and w
SELECT MIN(k), MAX(k), MIN(n), MAX(n), MIN(v), MAX(v) FROM TAF10vs2;
-- [reads every row] the same, through every row
SELECT MIN(k), MAX(k), MIN(n), MAX(n), MIN(v), MAX(v) FROM TAF10vs2 WHERE 1 = 1;

-- SKIP on the VARCHAR key column of the same global: C|5|w is not in the table
CREATE TABLE TAF10vs3 (k VARCHAR SKIP '"C"', n INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (k, n)) GLOBAL "^TAF10v(keys(""k""),keys(""n""))" READONLY;
-- The rows: A|9|x, B|1|y, B|2|z and D|3|u
SELECT * FROM TAF10vs3;
-- [one value] k A and D, n 1 and 9, v u and z
SELECT MIN(k), MAX(k), MIN(n), MAX(n), MIN(v), MAX(v) FROM TAF10vs3;
-- [reads every row] the same, through every row
SELECT MIN(k), MAX(k), MIN(n), MAX(n), MIN(v), MAX(v) FROM TAF10vs3 WHERE 1 = 1;

-- END on the key column of a table with one key column. MIN/MAX of a non-key column read its cross reference
-- (YDBOcto#617), which AIM builds over the range START:ENDPOINT: the nodes past END are in it, and the walk stops at
-- END. The traps are id = 3 (v = z, MAX(v)) and id = 4 (v = a, MIN(v)), which END puts out of the table.
SELECT XECUTE_M_CODE('set ^TAF10o(1)="m",^TAF10o(2)="c",^TAF10o(3)="z",^TAF10o(4)="a"');
SELECT XECUTE_M_CODE('zwrite ^TAF10o');
CREATE TABLE TAF10o (id INTEGER PRIMARY KEY END "(keys(""id"")="""")!(keys(""id"")>2)", v VARCHAR PIECE 1) GLOBAL "^TAF10o(keys(""id""))" READONLY;
-- The rows: 1|m and 2|c
SELECT * FROM TAF10o;
-- [one value] c and m
SELECT MIN(v), MAX(v) FROM TAF10o;
-- [reads every row] the same, through every row
SELECT MIN(v), MAX(v) FROM TAF10o WHERE 1 = 1;

-- An empty table
CREATE TABLE TAF10e (a INTEGER, b INTEGER, PRIMARY KEY (a, b)) GLOBAL "^TAF10e(keys(""a""),keys(""b""))" READONLY;
-- The rows: none
SELECT * FROM TAF10e;
-- [one value] no row: NULL and NULL
SELECT MIN(b), MAX(b) FROM TAF10e;
-- [reads every row] the same, through every row
SELECT MIN(b), MAX(b) FROM TAF10e WHERE 1 = 1;

-- ############################################################################################################
-- Part E: tables with several key columns that have no AIM cross reference, whose MIN/MAX read every row: a table kept
-- in a local variable, and a table with an ITERATOR (the functions are in TAF10.m).
SELECT XECUTE_M_CODE('set lvTAF10(1,5)="p",lvTAF10(1,7)="q",lvTAF10(2,3)="r",lvTAF10(2,4)=""');
SELECT XECUTE_M_CODE('zwrite lvTAF10');
CREATE TABLE TAF10lv (a INTEGER, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "lvTAF10(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|5|p, 1|7|q, 2|3|r and 2|4| (v NULL)
SELECT * FROM TAF10lv;
-- [reads every row] 3 and 7
SELECT MIN(b), MAX(b) FROM TAF10lv;
SELECT XECUTE_M_CODE('set ^TAF10i(1,5)="p",^TAF10i(1,7)="q",^TAF10i(2,3)="r",^TAF10i(2,4)=""');
SELECT XECUTE_M_CODE('zwrite ^TAF10i');
CREATE TABLE TAF10i (a INTEGER ITERATOR "$$a^TAF10", b INTEGER ITERATOR "$$b^TAF10", v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10i(keys(""a""),keys(""b""))";
-- The rows: 1|5|p, 1|7|q, 2|3|r and 2|4| (v NULL)
SELECT * FROM TAF10i;
-- [reads every row] 3 and 7
SELECT MIN(b), MAX(b) FROM TAF10i;

-- ############################################################################################################
-- Part F: an ITERATOR that skips nodes of the global. The rows of TAF10x are the odd ids; the even ids have the value
-- 99 in the global but are not rows. The cross reference of v would index them too and give 99 as MAX(v), so MIN/MAX
-- read every row.
SELECT XECUTE_M_CODE('set ^TAF10x(1)=10,^TAF10x(2)=99,^TAF10x(3)=30,^TAF10x(4)=99,^TAF10x(5)=50,^TAF10x(6)=99');
SELECT XECUTE_M_CODE('zwrite ^TAF10x');
CREATE TABLE TAF10x (id INTEGER PRIMARY KEY ITERATOR "$$odd^TAF10", v INTEGER PIECE 1) GLOBAL "^TAF10x(keys(""id""))";
-- The rows: 1|10, 3|30 and 5|50
SELECT * FROM TAF10x;
-- [reads every row] 10 and 50
SELECT MIN(v), MAX(v) FROM TAF10x;
-- [reads every row] the same, through every row
SELECT MIN(v), MAX(v) FROM TAF10x WHERE 1 = 1;
-- [reads every row] 1 and 5, the key column itself: the last subscript of the global, 6, is not a row
SELECT MIN(id), MAX(id) FROM TAF10x;
-- [reads every row] the same, through every row
SELECT MIN(id), MAX(id) FROM TAF10x WHERE 1 = 1;
-- A VIRTUAL key column: the ITERATOR makes up the ids 1, 2 and 3, and the global ^TAF10xv has no node at all
CREATE TABLE TAF10xv (id INTEGER PRIMARY KEY ITERATOR "$$virtual^TAF10" VIRTUAL) GLOBAL "^TAF10xv(keys(""id""))";
-- The rows: 1, 2 and 3
SELECT * FROM TAF10xv;
-- [reads every row] 1 and 3
SELECT MIN(id), MAX(id) FROM TAF10xv;
-- [reads every row] the same, through every row
SELECT MIN(id), MAX(id) FROM TAF10xv WHERE 1 = 1;
-- [reads every row] MIN/MAX of a literal: 7 and 7, as the table has rows, though its global has no node
SELECT MIN(7), MAX(7) FROM TAF10xv;

-- ############################################################################################################
-- Part G: EXTRACT columns, whose values an M expression computes: there is no node of the global for AIM to index
-- (YDBOcto#1138), so MIN/MAX read every row. Here the value is upper-cased.
SELECT XECUTE_M_CODE('set ^TAF10u(1)="banana",^TAF10u(2)="apple",^TAF10u(3)="cherry"');
SELECT XECUTE_M_CODE('zwrite ^TAF10u');
CREATE TABLE TAF10u (id INTEGER PRIMARY KEY, v VARCHAR EXTRACT "$ZCONVERT($GET(^TAF10u(keys(""id""))),""U"")") GLOBAL "^TAF10u(keys(""id""))" READONLY;
-- The rows: 1|BANANA, 2|APPLE and 3|CHERRY
SELECT * FROM TAF10u;
-- [reads every row] APPLE and CHERRY
SELECT MIN(v), MAX(v) FROM TAF10u;
-- The same on a table with two key columns
SELECT XECUTE_M_CODE('set ^TAF10w(1,1)="banana",^TAF10w(1,2)="apple",^TAF10w(2,1)="cherry"');
SELECT XECUTE_M_CODE('zwrite ^TAF10w');
CREATE TABLE TAF10w (a INTEGER, b INTEGER, v VARCHAR EXTRACT "$ZCONVERT($GET(^TAF10w(keys(""a""),keys(""b""))),""U"")", PRIMARY KEY (a, b)) GLOBAL "^TAF10w(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|BANANA, 1|2|APPLE and 2|1|CHERRY
SELECT * FROM TAF10w;
-- [reads every row] APPLE and CHERRY
SELECT MIN(v), MAX(v) FROM TAF10w;
-- A column that is not EXTRACT, in a table that also has an EXTRACT column: its MIN/MAX read its cross reference as in
-- any other table. A query that also takes MIN/MAX of the EXTRACT column reads every row.
CREATE TABLE TAF10u2 (id INTEGER PRIMARY KEY, w VARCHAR PIECE 1, v VARCHAR EXTRACT "$ZCONVERT($GET(^TAF10u(keys(""id""))),""U"")") GLOBAL "^TAF10u(keys(""id""))" READONLY;
-- The rows: 1|banana|BANANA, 2|apple|APPLE and 3|cherry|CHERRY
SELECT * FROM TAF10u2;
-- [one value] apple and cherry
SELECT MIN(w), MAX(w) FROM TAF10u2;
-- [reads every row] the same, through every row
SELECT MIN(w), MAX(w) FROM TAF10u2 WHERE 1 = 1;
-- [one value] 1 and 3, the key column
SELECT MIN(id), MAX(id) FROM TAF10u2;
-- [reads every row] w apple and cherry, v APPLE and CHERRY
SELECT MIN(w), MAX(w), MIN(v), MAX(v) FROM TAF10u2;
-- The same on a table with two key columns
CREATE TABLE TAF10w2 (a INTEGER, b INTEGER, w VARCHAR PIECE 1, v VARCHAR EXTRACT "$ZCONVERT($GET(^TAF10w(keys(""a""),keys(""b""))),""U"")", PRIMARY KEY (a, b)) GLOBAL "^TAF10w(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|banana|BANANA, 1|2|apple|APPLE and 2|1|cherry|CHERRY
SELECT * FROM TAF10w2;
-- [one value] a 1 and 2, b 1 and 2, w apple and cherry
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(w), MAX(w) FROM TAF10w2;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(w), MAX(w) FROM TAF10w2 WHERE 1 = 1;
-- [reads every row] a 1 and 2, w apple and cherry, v APPLE and CHERRY
SELECT MIN(a), MAX(a), MIN(w), MAX(w), MIN(v), MAX(v) FROM TAF10w2;

-- ############################################################################################################
-- Part H: the key column of a table with one key column, whose MIN/MAX read the first or last subscript of the table's
-- global. The bounds START and ENDPOINT are tested on that subscript as the table scan tests them, in M collation.
-- YottaDB/YDBOctoVistA maps a FileMan file as START 0 ENDPOINT '$CHAR(0)': every numeric IEN after 0, and not the "B"
-- index beside them ($CHAR(0) sorts before every other string, and after every number).
SELECT XECUTE_M_CODE('set ^TAF10g(0)="header",^TAF10g(1)="a",^TAF10g(5)="b",^TAF10g("B","a",1)="",^TAF10g("B","b",5)=""');
SELECT XECUTE_M_CODE('zwrite ^TAF10g');
CREATE TABLE TAF10g (id INTEGER PRIMARY KEY START 0 ENDPOINT '$CHAR(0)', v VARCHAR PIECE 1) GLOBAL "^TAF10g(keys(""id""))" READONLY;
-- The rows: 1|a and 5|b
SELECT * FROM TAF10g;
-- [one value] 1 and 5
SELECT MIN(id), MAX(id) FROM TAF10g;
-- [reads every row] the same, through every row
SELECT MIN(id), MAX(id) FROM TAF10g WHERE 1 = 1;
-- The same with STARTINCLUDE: 0 is a row
CREATE TABLE TAF10gi (id INTEGER PRIMARY KEY START 0 STARTINCLUDE ENDPOINT '$CHAR(0)', v VARCHAR PIECE 1) GLOBAL "^TAF10g(keys(""id""))" READONLY;
-- The rows: 0|header, 1|a and 5|b
SELECT * FROM TAF10gi;
-- [one value] 0 and 5
SELECT MIN(id), MAX(id) FROM TAF10gi;
-- [reads every row] the same, through every row
SELECT MIN(id), MAX(id) FROM TAF10gi WHERE 1 = 1;
-- A VARCHAR key column with numeric-looking values. M orders the subscripts 3, 9, 10, A (numbers first); SQL orders the
-- values '10', '3', '9', 'A'.
SELECT XECUTE_M_CODE('set ^TAF10h(3)="c",^TAF10h(9)="i",^TAF10h(10)="j",^TAF10h("A")="a"');
SELECT XECUTE_M_CODE('zwrite ^TAF10h');
CREATE TABLE TAF10h (k VARCHAR PRIMARY KEY, v VARCHAR PIECE 1) GLOBAL "^TAF10h(keys(""k""))" READONLY;
-- The rows, in M order: 3|c, 9|i, 10|j and A|a
SELECT * FROM TAF10h;
-- [one value] 10 and A
SELECT MIN(k), MAX(k) FROM TAF10h;
-- [reads every row] the same, through every row
SELECT MIN(k), MAX(k) FROM TAF10h WHERE 1 = 1;
-- The same with ENDPOINT 10, which in M collation keeps 3, 9 and 10
CREATE TABLE TAF10hp (k VARCHAR PRIMARY KEY ENDPOINT 10, v VARCHAR PIECE 1) GLOBAL "^TAF10h(keys(""k""))" READONLY;
-- The rows, in M order: 3|c, 9|i and 10|j
SELECT * FROM TAF10hp;
-- [one value] 10 and 9
SELECT MIN(k), MAX(k) FROM TAF10hp;
-- [reads every row] the same, through every row
SELECT MIN(k), MAX(k) FROM TAF10hp WHERE 1 = 1;
-- A key column whose range has one value (START 1 STARTINCLUDE ENDPOINT 1): its subscript is in the cross references,
-- like that of any other key column (YDBAIM#106), so MIN/MAX read the first or last value of each cross reference
CREATE TABLE TAF10s1v (a INTEGER START 1 STARTINCLUDE ENDPOINT 1, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TAF10s(keys(""a""),keys(""b""))" READONLY;
-- The rows: 1|1|m and 1|9|z
SELECT * FROM TAF10s1v;
-- [one value] a 1 and 1, b 1 and 9, v m and z
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s1v;
-- [reads every row] the same, through every row
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v) FROM TAF10s1v WHERE 1 = 1;

-- ############################################################################################################
-- Part I: a WHERE that the plan uses up, by fixing a key column to its value or by reading a cross reference, leaves no
-- condition to check on each row. MIN/MAX are still those of the rows the WHERE keeps, not of the table: the plan
-- reads those rows. Each query is followed by the same query with AND 1 = 1, a condition that is left to check on each row.
-- TAF10a has the rows 1|5|p, 1|7|q, 2|3|r and 2|4| (v NULL); TAF10u2 has 1|banana|BANANA, 2|apple|APPLE and
-- 3|cherry|CHERRY.
-- [reads the rows with a = 2] 4 (7 in the table)
SELECT MAX(b) FROM TAF10a WHERE a = 2;
-- [reads the rows with a = 2] the same, with a condition checked on each row
SELECT MAX(b) FROM TAF10a WHERE a = 2 AND 1 = 1;
-- [reads the rows with b = 5] p (r in the table)
SELECT MAX(v) FROM TAF10a WHERE b = 5;
-- the same, with a condition checked on each row
SELECT MAX(v) FROM TAF10a WHERE b = 5 AND 1 = 1;
-- [reads the rows with a = 1] 5 and 7 (3 and 7 in the table)
SELECT MIN(b), MAX(b) FROM TAF10a WHERE a = 1;
-- the same, with a condition checked on each row
SELECT MIN(b), MAX(b) FROM TAF10a WHERE a = 1 AND 1 = 1;
-- [reads the rows with that a] a sub-query for each row of the table: 1|7, 1|7, 2|4 and 2|4
SELECT x.a, (SELECT MAX(y.b) FROM TAF10a y WHERE y.a = x.a) FROM TAF10a x;
-- A table with one key column. [reads the rows with w = 'apple'] 2 (3 in the table)
SELECT MAX(id) FROM TAF10u2 WHERE w = 'apple';
-- the same, with a condition checked on each row
SELECT MAX(id) FROM TAF10u2 WHERE w = 'apple' AND 1 = 1;
-- [reads the row with id = 1] banana (cherry in the table)
SELECT MAX(w) FROM TAF10u2 WHERE id = 1;
-- the same, with a condition checked on each row
SELECT MAX(w) FROM TAF10u2 WHERE id = 1 AND 1 = 1;
-- [one value] the statement's own MIN/MAX has no WHERE: 3 and 7. Its sub-query, which fixes a key column, reads its
-- own row: p.
SELECT MIN(b), MAX(b), (SELECT v FROM TAF10a WHERE a = 1 AND b = 5) FROM TAF10a;
