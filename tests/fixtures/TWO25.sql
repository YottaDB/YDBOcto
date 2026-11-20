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
-- Part A: the first key column is limited to one value: "a" is 1 (START 1 STARTINCLUDE ENDPOINT 1). The table has the
-- rows of ^TWO25a(1,b), and not those of ^TWO25a(2,b).
SELECT XECUTE_M_CODE('set ^TWO25a(1,1)="11|p1",^TWO25a(1,2)="12|q2",^TWO25a(1,3)="13|p3",^TWO25a(2,1)="21|p4",^TWO25a(2,2)="22|q5",^TWO25a(2,3)="23|p6"');
SELECT XECUTE_M_CODE('zwrite ^TWO25a');
CREATE TABLE TWO25a (a INTEGER START 1 STARTINCLUDE ENDPOINT 1, b INTEGER, v INTEGER PIECE 1, s VARCHAR PIECE 2, PRIMARY KEY (a, b)) GLOBAL "^TWO25a(keys(""a""),keys(""b""))" READONLY;
-- [no key column xref] the rows of the table: 1|1|11|p1, 1|2|12|q2 and 1|3|13|p3
SELECT * FROM TWO25a;
-- A condition on "b", the key column after "a"
-- [xref on b] 1|2|12|q2
SELECT * FROM TWO25a WHERE b = 2;
-- [xref on b] 1|1|11|p1 and 1|3|13|p3
SELECT * FROM TWO25a WHERE b IN (1, 3);
-- [xref on b] 1|2|12|q2 and 1|3|13|p3
SELECT * FROM TWO25a WHERE b > 1 ORDER BY b;
-- [xref on b] 1|3|13|p3
SELECT * FROM TWO25a WHERE b > 1 ORDER BY b DESC LIMIT 1;
-- [xref on b] no row: b = 4 is not in the table
SELECT * FROM TWO25a WHERE b = 4;
-- A condition on "v" and on "s", which are not key columns
-- [xref on v] 1|2|12|q2
SELECT * FROM TWO25a WHERE v = 12;
-- [xref on v] 1|2|12|q2 and 1|3|13|p3
SELECT * FROM TWO25a WHERE v >= 12;
-- [xref on v] no row: v = 22 is in ^TWO25a(2,2), which is not a row of the table
SELECT * FROM TWO25a WHERE v = 22;
-- [xref on s] 1|1|11|p1 and 1|3|13|p3
SELECT * FROM TWO25a WHERE s LIKE 'p%';
-- [xref on s] the two last rows by s: 1|2|12|q2 and 1|3|13|p3
SELECT * FROM TWO25a ORDER BY s DESC LIMIT 2;
-- A join whose ON fixes "b" of the second table
-- [xref on b and v] 12|12
SELECT x.v, y.v FROM TWO25a x INNER JOIN TWO25a y ON y.b = x.b WHERE x.v = 12;
-- [no key column xref] "a" and "b" both have a condition: the row is read from ^TWO25a. 1|2|12|q2
SELECT * FROM TWO25a WHERE a = 1 AND b = 2;
-- [one value] 1 and 1, 1 and 3, 11 and 13, p1 and q2
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(v), MAX(v), MIN(s), MAX(s) FROM TWO25a;

-- ############################################################################################################
-- Part B: a VARCHAR first key column that is limited to one value: "a" is 'abc'
SELECT XECUTE_M_CODE('set ^TWO25b("abb",1)="x",^TWO25b("abc",1)="y",^TWO25b("abc",2)="z",^TWO25b("abd",1)="w"');
SELECT XECUTE_M_CODE('zwrite ^TWO25b');
CREATE TABLE TWO25b (a VARCHAR START '"abc"' STARTINCLUDE ENDPOINT '"abc"', b INTEGER, s VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TWO25b(keys(""a""),keys(""b""))" READONLY;
-- [no key column xref] the rows of the table: abc|1|y and abc|2|z
SELECT * FROM TWO25b;
-- [xref on b] abc|1|y
SELECT * FROM TWO25b WHERE b = 1;
-- [xref on s] abc|2|z
SELECT * FROM TWO25b WHERE s = 'z';
-- [xref on s] no row: 'x' is in ^TWO25b("abb",1), which is not a row of the table
SELECT * FROM TWO25b WHERE s = 'x';
-- [xref on a] abc|1|y and abc|2|z
SELECT * FROM TWO25b WHERE a LIKE 'ab%';
-- [one value] abc and abc, 1 and 2, y and z
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(s), MAX(s) FROM TWO25b;

-- ############################################################################################################
-- Part C: the last key column is limited to one value: "b" is 2. The table has the rows of ^TWO25c(a,2).
SELECT XECUTE_M_CODE('set ^TWO25c(1,1)="k",^TWO25c(1,2)="l",^TWO25c(2,2)="m",^TWO25c(3,2)="l",^TWO25c(3,3)="n"');
SELECT XECUTE_M_CODE('zwrite ^TWO25c');
CREATE TABLE TWO25c (a INTEGER, b INTEGER START 2 STARTINCLUDE ENDPOINT 2, s VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TWO25c(keys(""a""),keys(""b""))" READONLY;
-- [no key column xref] the rows of the table: 1|2|l, 2|2|m and 3|2|l
SELECT * FROM TWO25c;
-- [xref on b] the three rows
SELECT * FROM TWO25c WHERE b = 2;
-- [xref on b] no row: b = 3 is in ^TWO25c(3,3), which is not a row of the table
SELECT * FROM TWO25c WHERE b = 3;
-- [xref on s] 1|2|l and 3|2|l
SELECT * FROM TWO25c WHERE s = 'l';
-- [xref on s] no row: 'n' is in ^TWO25c(3,3), which is not a row of the table
SELECT * FROM TWO25c WHERE s = 'n';
-- [one value] 1 and 3, 2 and 2, l and m
SELECT MIN(a), MAX(a), MIN(b), MAX(b), MIN(s), MAX(s) FROM TWO25c;

-- ############################################################################################################
-- Part D: the only key column is limited to one value: "id" is 8. The table has one row.
SELECT XECUTE_M_CODE('set ^TWO25d(7)="g",^TWO25d(8)="h",^TWO25d(9)="i"');
SELECT XECUTE_M_CODE('zwrite ^TWO25d');
CREATE TABLE TWO25d (id INTEGER PRIMARY KEY START 8 STARTINCLUDE ENDPOINT 8, s VARCHAR PIECE 1) GLOBAL "^TWO25d(keys(""id""))" READONLY;
-- [no key column xref] the row of the table: 8|h
SELECT * FROM TWO25d;
-- [xref on s] 8|h
SELECT * FROM TWO25d WHERE s = 'h';
-- [xref on s] no row: 'i' is in ^TWO25d(9), which is not a row of the table
SELECT * FROM TWO25d WHERE s = 'i';
-- [one value] 8 and 8, h and h
SELECT MIN(id), MAX(id), MIN(s), MAX(s) FROM TWO25d;

-- ############################################################################################################
-- Part E: START and ENDPOINT are the same value, without STARTINCLUDE. START is then not a value of "a", so the table
-- has no row.
SELECT XECUTE_M_CODE('set ^TWO25e(1,1)="11|p1",^TWO25e(1,2)="12|q2",^TWO25e(2,1)="21|p4"');
SELECT XECUTE_M_CODE('zwrite ^TWO25e');
CREATE TABLE TWO25e (a INTEGER START 1 ENDPOINT 1, b INTEGER, v INTEGER PIECE 1, s VARCHAR PIECE 2, PRIMARY KEY (a, b)) GLOBAL "^TWO25e(keys(""a""),keys(""b""))" READONLY;
-- [no key column xref] no row
SELECT * FROM TWO25e;
-- [xref on b] no row
SELECT * FROM TWO25e WHERE b = 2;
-- [xref on v] no row
SELECT * FROM TWO25e WHERE v = 12;
-- [one value] no value: every MIN and MAX is NULL
SELECT MIN(b), MAX(b), MIN(v), MAX(v) FROM TWO25e;
