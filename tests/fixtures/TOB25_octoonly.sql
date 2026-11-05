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
-- Part E (Octo only: READONLY tables on globals set in M)
-- The START, STARTINCLUDE, ENDPOINT and END bounds of a VARCHAR key column of a READONLY table. Four globals
-- hold the same nodes, one per bound. A key column's cross reference holds every subscript of the global, so the walks
-- test the bounds on each value they read. M orders the subscripts 9 10 B D DA E H HA K (numbers first); SQL orders
-- the values 10 9 B D DA E H HA K.
SELECT XECUTE_M_CODE('for g="^TOB25s","^TOB25i","^TOB25p","^TOB25e" for k=10,9,"B","D","DA","E","H","HA","K" set @g@(k)=k');
SELECT XECUTE_M_CODE('zwrite ^TOB25s');

-- START "D" (exclusive): the rows are DA E H HA K
CREATE TABLE TOB25s (k VARCHAR PRIMARY KEY START '"D"', v VARCHAR PIECE 1) GLOBAL "^TOB25s(keys(""k""))" READONLY;
-- The table scan, in M order: DA E H HA K
SELECT k FROM TOB25s;
-- [no sort, ascending walk] DA E H HA K
SELECT k FROM TOB25s ORDER BY k;
-- [no sort, descending walk] K HA
SELECT k FROM TOB25s ORDER BY k DESC LIMIT 2;
-- [no sort, ascending walk] D is the START, so not a row: DA
SELECT k FROM TOB25s WHERE k LIKE 'D%' ORDER BY k;

-- START "D" STARTINCLUDE: the rows are D DA E H HA K
CREATE TABLE TOB25i (k VARCHAR PRIMARY KEY START '"D"' STARTINCLUDE, v VARCHAR PIECE 1) GLOBAL "^TOB25i(keys(""k""))" READONLY;
-- The table scan: D DA E H HA K
SELECT k FROM TOB25i;
-- [no sort, descending walk] K HA H E DA D
SELECT k FROM TOB25i ORDER BY k DESC;
-- [no sort, ascending walk] D DA
SELECT k FROM TOB25i WHERE k LIKE 'D%' ORDER BY k;

-- ENDPOINT "H": the rows are 9 10 B D DA E H
CREATE TABLE TOB25p (k VARCHAR PRIMARY KEY ENDPOINT '"H"', v VARCHAR PIECE 1) GLOBAL "^TOB25p(keys(""k""))" READONLY;
-- The table scan, in M order: 9 10 B D DA E H
SELECT k FROM TOB25p;
-- [no sort, ascending walk] in SQL order: 10 9 B D DA E H
SELECT k FROM TOB25p ORDER BY k;
-- [no sort, descending walk] H E
SELECT k FROM TOB25p ORDER BY k DESC LIMIT 2;
-- [no sort, ascending walk] HA is past the ENDPOINT: H
SELECT k FROM TOB25p WHERE k LIKE 'H%' ORDER BY k;

-- END true past "H": the rows are 9 10 B D DA E H
CREATE TABLE TOB25e (k VARCHAR PRIMARY KEY END "(keys(""k"")="""")!(keys(""k"")]""H"")", v VARCHAR PIECE 1) GLOBAL "^TOB25e(keys(""k""))" READONLY;
-- The table scan: 9 10 B D DA E H
SELECT k FROM TOB25e;
-- [no sort, descending walk] H E DA D B 9 10
SELECT k FROM TOB25e ORDER BY k DESC;
-- [no sort, ascending walk] H
SELECT k FROM TOB25e WHERE k LIKE 'H%' ORDER BY k;

-- A node with an empty string subscript (the region allows null subscripts). The FOR loop over the table's global ends
-- at "", so the table scan never returns that node as a row: the walks of the cross reference leave it out too.
SELECT XECUTE_M_CODE('set ^TOB25n("")="e",^TOB25n("K")="f",^TOB25n(10)="g",^TOB25m(1,"")="e",^TOB25m(1,"K")="f",^TOB25m(2,10)="g"');
SELECT XECUTE_M_CODE('zwrite ^TOB25n,^TOB25m');
CREATE TABLE TOB25n (k VARCHAR PRIMARY KEY, v VARCHAR PIECE 1) GLOBAL "^TOB25n(keys(""k""))" READONLY;
-- The table scan, in M order: (10,g) (K,f)
SELECT k, v FROM TOB25n;
-- [no sort, ascending walk] (10,g) (K,f)
SELECT k, v FROM TOB25n ORDER BY k;
-- [no sort, descending walk] (K,f) (10,g)
SELECT k, v FROM TOB25n ORDER BY k DESC;
-- The same for a second key column
CREATE TABLE TOB25m (a INTEGER, k VARCHAR, v VARCHAR PIECE 1, PRIMARY KEY (a, k)) GLOBAL "^TOB25m(keys(""a""),keys(""k""))" READONLY;
-- The table scan: (1,K,f) (2,10,g)
SELECT a, k, v FROM TOB25m;
-- [no sort, ascending walk] (2,10,g) (1,K,f)
SELECT a, k, v FROM TOB25m ORDER BY k, a;
-- [no sort, descending walk] (1,K,f) (2,10,g)
SELECT a, k, v FROM TOB25m ORDER BY k DESC, a DESC;

-- START (exclusive) on the first key column, walked in descending order under the cross reference of b. The walk
-- ends at the START value, and also at any value before it (YDBOcto#1195; TOB28 tests the walk of the table's
-- global), as the walk does not always meet the START value: under b = 3 of the cross reference of b there is no
-- a = 1 (START 1), no node has a = 0 (START 0), and no row has a = 3 (START 3).
SELECT XECUTE_M_CODE('set ^TOB25x(1,7)="a",^TOB25x(2,3)="b",^TOB25x(2,5)="f",^TOB25x(4,3)="e",^TOB25x(5,3)="g"');
SELECT XECUTE_M_CODE('zwrite ^TOB25x');
CREATE TABLE TOB25x1 (a INTEGER START 1, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TOB25x(keys(""a""),keys(""b""))" READONLY;
CREATE TABLE TOB25x0 (a INTEGER START 0, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TOB25x(keys(""a""),keys(""b""))" READONLY;
CREATE TABLE TOB25x3 (a INTEGER START 3, b INTEGER, v VARCHAR PIECE 1, PRIMARY KEY (a, b)) GLOBAL "^TOB25x(keys(""a""),keys(""b""))" READONLY;
-- The table scan, START 1: (2,3,b) (2,5,f) (4,3,e) (5,3,g)
SELECT a, b, v FROM TOB25x1;
-- [no sort, descending walk] (5,3,g) (4,3,e) (2,3,b)
SELECT a, b, v FROM TOB25x1 WHERE b = 3 ORDER BY a DESC;
-- [sort] (5,3,g) (4,3,e) (2,3,b) (2,5,f)
SELECT a, b, v FROM TOB25x1 ORDER BY b, a DESC;
-- [sort] (2,5,f) (5,3,g) (4,3,e)
SELECT a, b, v FROM TOB25x1 ORDER BY b DESC, a DESC LIMIT 3;
-- [no sort, descending walk] START 0: (5,3,g) (4,3,e) (2,3,b)
SELECT a, b, v FROM TOB25x0 WHERE b = 3 ORDER BY a DESC;
-- The table scan, START 3: (4,3,e) (5,3,g)
SELECT a, b, v FROM TOB25x3;
-- [no sort, descending walk] START 3: (5,3,g) (4,3,e)
SELECT a, b, v FROM TOB25x3 WHERE b = 3 ORDER BY a DESC;
-- [no sort, descending walk] START 3: no rows
SELECT a, b, v FROM TOB25x3 WHERE b = 5 ORDER BY a DESC;

-- The same with a VARCHAR second key column, whose cross reference the ORDER BY walks: under s = "D" there is no g = 1
SELECT XECUTE_M_CODE('set ^TOB25y(1,"B")=1,^TOB25y(2,"B")=2,^TOB25y(2,"D")=3,^TOB25y(3,"D")=4');
SELECT XECUTE_M_CODE('zwrite ^TOB25y');
CREATE TABLE TOB25y (g INTEGER START 1, s VARCHAR, v INTEGER PIECE 1, PRIMARY KEY (g, s)) GLOBAL "^TOB25y(keys(""g""),keys(""s""))" READONLY;
-- The table scan: (2,B,2) (2,D,3) (3,D,4)
SELECT g, s, v FROM TOB25y;
-- [no sort, descending walk] (3,D,4) (2,D,3) (2,B,2)
SELECT g, s, v FROM TOB25y ORDER BY s DESC, g DESC;
-- [no sort, ascending walk of s, descending walk of g under each s] (2,B,2) (3,D,4) (2,D,3)
SELECT g, s, v FROM TOB25y ORDER BY s, g DESC;
-- [no sort, ascending walk of s, descending walk of g under each s] (3,D,4) (2,D,3)
SELECT g, s, v FROM TOB25y WHERE s LIKE 'D%' ORDER BY s, g DESC;

-- ############################################################################################################
-- Part F (Octo only: READONLY tables on globals set in M)
-- A DATE key column after the first, of a READONLY table. Its cross reference holds each date converted to
-- UNIX time, and after it the subscripts of the global in their stored form. The walk goes through the converted
-- values, so in date order, and reads k1 and k2 in their stored form from under each one. 2024-01-15 is stored in two
-- forms: "2024-01-15" (k1 = 2) and "2024-1-15" (k1 = 3).
SELECT XECUTE_M_CODE('set ^TOB25d(1,"2024-03-01")="a",^TOB25d(1,"2023-12-31")="b",^TOB25d(2,"2024-01-15")="c",^TOB25d(3,"2024-1-15")="d"');
SELECT XECUTE_M_CODE('zwrite ^TOB25d');
CREATE TABLE TOB25d (k1 INTEGER, k2 DATE, v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TOB25d(keys(""k1""),keys(""k2""))" READONLY;
-- The table scan, in M order: (1,2023-12-31,b) (1,2024-03-01,a) (2,2024-01-15,c) (3,2024-01-15,d)
SELECT k1, k2, v FROM TOB25d;
-- [no sort, ascending walk] (1,2023-12-31,b) (2,2024-01-15,c) (3,2024-01-15,d) (1,2024-03-01,a)
SELECT k1, k2, v FROM TOB25d ORDER BY k2;
-- [no sort, descending walk] (1,2024-03-01,a) then both forms of 2024-01-15
SELECT k1, k2, v FROM TOB25d ORDER BY k2 DESC LIMIT 3;
-- [no sort, ascending walk] k1 is read under each date, in order: as for ORDER BY k2
SELECT k1, k2, v FROM TOB25d ORDER BY k2, k1;
-- [no sort, ascending walk] v > 'b' is checked on each row of the walk: (2,2024-01-15,c)
SELECT k1, k2, v FROM TOB25d WHERE v > 'b' ORDER BY k2 LIMIT 1;

-- START '"2024-"' applies to the stored form: "2023-12-31" is before "2024-", so it is not a row.
-- (START '"2024"' would not do: "2024" is a canonical number, and M orders every number before every string.)
SELECT XECUTE_M_CODE('merge ^TOB25ds=^TOB25d');
CREATE TABLE TOB25ds (k1 INTEGER, k2 DATE START '"2024-"', v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TOB25ds(keys(""k1""),keys(""k2""))" READONLY;
-- The table scan: (1,2024-03-01,a) (2,2024-01-15,c) (3,2024-01-15,d)
SELECT k1, k2, v FROM TOB25ds;
-- [no sort, ascending walk] (2,2024-01-15,c) (3,2024-01-15,d) (1,2024-03-01,a)
SELECT k1, k2, v FROM TOB25ds ORDER BY k2;

-- ############################################################################################################
-- Part G (Octo only: READONLY tables on globals set in M)
-- A BOOLEAN key column after the first, of a READONLY table. Its cross reference holds 0 or 1, converted from
-- the stored form: TRUE is stored as 1 (k1 = 1) and as "Y" (k1 = 2), FALSE as 0 (k1 = 1) and as "f" (k1 = 3).
SELECT XECUTE_M_CODE('set ^TOB25t(1,1)="a",^TOB25t(1,0)="b",^TOB25t(2,"Y")="c",^TOB25t(3,"f")="d"');
SELECT XECUTE_M_CODE('zwrite ^TOB25t');
CREATE TABLE TOB25t (k1 INTEGER, k2 BOOLEAN, v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TOB25t(keys(""k1""),keys(""k2""))" READONLY;
-- The table scan, in M order: (1,f,b) (1,t,a) (2,t,c) (3,f,d)
SELECT k1, k2, v FROM TOB25t;
-- [no sort, ascending walk] (1,f,b) (3,f,d) (1,t,a) (2,t,c)
SELECT k1, k2, v FROM TOB25t ORDER BY k2;
-- [no sort, descending walk] (1,t,a) (2,t,c)
SELECT k1, k2, v FROM TOB25t ORDER BY k2 DESC LIMIT 2;

-- ############################################################################################################
-- Part H (Octo only: READONLY tables on globals set in M)
-- The first key column, DATE and BOOLEAN, of a READONLY table. Its FOR loop goes through the stored forms,
-- whose M order is not that of the values ("2024-1-15" is after "2024-03-01"), so the ORDER BY walks the cross
-- reference instead of sorting.
SELECT XECUTE_M_CODE('set ^TOB25d1("2024-03-01")="a",^TOB25d1("2023-12-31")="b",^TOB25d1("2024-1-15")="c"');
SELECT XECUTE_M_CODE('zwrite ^TOB25d1');
CREATE TABLE TOB25d1 (k DATE PRIMARY KEY, v VARCHAR PIECE 1) GLOBAL "^TOB25d1(keys(""k""))" READONLY;
-- The table scan, in M order: 2023-12-31 2024-03-01 2024-01-15
SELECT k, v FROM TOB25d1;
-- [no sort, ascending walk] 2023-12-31 2024-01-15 2024-03-01
SELECT k, v FROM TOB25d1 ORDER BY k;
-- [no sort, descending walk] 2024-03-01
SELECT k, v FROM TOB25d1 ORDER BY k DESC LIMIT 1;

SELECT XECUTE_M_CODE('set ^TOB25t1(0)="a",^TOB25t1("Y")="b"');
SELECT XECUTE_M_CODE('zwrite ^TOB25t1');
CREATE TABLE TOB25t1 (k BOOLEAN PRIMARY KEY, v VARCHAR PIECE 1) GLOBAL "^TOB25t1(keys(""k""))" READONLY;
-- The table scan: f t
SELECT k, v FROM TOB25t1;
-- [no sort, descending walk] t f
SELECT k, v FROM TOB25t1 ORDER BY k DESC;

-- ############################################################################################################
-- Part I (Octo only: READONLY tables on globals set in M)
-- ORDER BY k2, k1 on PRIMARY KEY (k1, k2), where k1, the first key column, is a READONLY BOOLEAN and is the second
-- ORDER BY item. A walk of the cross reference of k2 would give the order of k2. But under each value of k2 it lists k1
-- in its stored form, in M order: TRUE stored as 1 comes before FALSE stored as "f" (YDBOcto#1179). That is not the
-- order of k1, so the ORDER BY has to sort in any case. A cross reference is built for an ORDER BY only when its walk
-- removes the sort, so none is built here: the queries below read the table's global ("xref: none") and sort.
SELECT XECUTE_M_CODE('set ^TOB25bk(1,"x")="a",^TOB25bk("f","x")="b",^TOB25bk(0,"y")="c",^TOB25bk("Y","y")="d"');
SELECT XECUTE_M_CODE('zwrite ^TOB25bk');
CREATE TABLE TOB25bk (k1 BOOLEAN, k2 VARCHAR, v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TOB25bk(keys(""k1""),keys(""k2""))" READONLY;
-- The table scan, in M order: (f,y,c) (t,x,a) (t,y,d) (f,x,b)
SELECT k1, k2, v FROM TOB25bk;
-- [sort] No cross reference of k2, as k1 is not in boolean order under each k2: (f,x,b) (t,x,a) (f,y,c) (t,y,d)
SELECT k1, k2, v FROM TOB25bk ORDER BY k2, k1;
-- [sort] the same, with a LIMIT: (f,x,b) (t,x,a)
SELECT k1, k2, v FROM TOB25bk ORDER BY k2, k1 LIMIT 2;
-- [sort] the same after a non-key column: (t,x,a) (f,x,b) (f,y,c) (t,y,d)
SELECT k1, k2, v FROM TOB25bk ORDER BY v, k1;
