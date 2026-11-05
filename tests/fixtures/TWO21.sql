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
-- Part A: ^GXRF("DACN1",accession,id). A constant subscript before the keys; "id" is the second key column.
-- ^GXRF("DACN2",...) is another list kept in the same global; it is not part of the table.
SELECT XECUTE_M_CODE('zwrite ^GXRF');
CREATE TABLE gxrf (accession VARCHAR KEY NUM 0, id INTEGER KEY NUM 1, val VARCHAR PIECE 1)
  GLOBAL "^GXRF(""DACN1"",keys(""accession""),keys(""id""))" READONLY;
-- [no key column xref] the rows of the table: A|75, A|85, A|95, B|76, B|86, B|96, C|77, C|87 and C|97 (not DACN2).
SELECT * FROM gxrf;

-- [xref on id] B|76. Not the DACN2 node with id 76.
SELECT * FROM gxrf WHERE id = 76;
-- [xref on id] A|75 and A|85; there is no id 99.
-- [sort] IN gives id several values: the rows come out for each value in turn, so not in ORDER BY order.
SELECT * FROM gxrf WHERE id IN (75,85,99) ORDER BY accession, id;
-- [xref on id] 76, 77, 85, 86: the walk starts at 76; the other bound is checked on each row.
-- [sort] the range walks several values of id: the rows come out for each value in turn, so not in ORDER BY order.
SELECT * FROM gxrf WHERE id >= 76 AND id < 87 ORDER BY accession, id;
-- [xref on id] C|77 and A|85, the first two rows of the walk. The walk stops once it has them (see Part L).
SELECT * FROM gxrf WHERE id > 76 LIMIT 2;
-- [xref on id] no row.
SELECT * FROM gxrf WHERE id = 99;
-- [no key column xref] accession, the first key column, is fixed in place, so the loop before id runs once
-- and id is fixed in place too. In either order of the two conditions.
SELECT * FROM gxrf WHERE id = 76 AND accession = 'B';
SELECT * FROM gxrf WHERE accession = 'B' AND id = 76;
-- [no key column xref] a condition on the first key column alone: the global serves it.
SELECT * FROM gxrf WHERE accession = 'C' ORDER BY id;

-- ############################################################################################################
-- Part B: ^T3(a,b,c), three key columns. "b" is a VARCHAR key whose values 9 and 10 are canonical numbers, which
-- M stores as numbers; AIM indexes a string column as "#"_value, so that '10' and 'A10' are told apart as strings.
SELECT XECUTE_M_CODE('zwrite ^T3');
CREATE TABLE t3 (a INTEGER KEY NUM 0, b VARCHAR KEY NUM 1, c INTEGER KEY NUM 2, v VARCHAR PIECE 1)
  GLOBAL "^T3(keys(""a""),keys(""b""),keys(""c""))" READONLY;
-- [no key column xref] the rows of the table, in M order: 1|9|1|a, 1|10|2|b, 2|10|1|c, 2|A10|3|d and 3|9|3|e.
SELECT * FROM t3;

-- [xref on b] the middle key: a comes from the cross reference, c from ^T3. 1|10|2|b and 2|10|1|c.
SELECT * FROM t3 WHERE b = '10' ORDER BY a, c;
-- [xref on b] 1|9|1|a, 2|A10|3|d and 3|9|3|e.
-- [sort] IN gives b several values: the rows come out for each value in turn, so not in ORDER BY order.
SELECT * FROM t3 WHERE b IN ('9','A10') ORDER BY a, c;
-- [xref on c] the last key: a and b both come from the cross reference. 2|A10|3|d and 3|9|3|e.
SELECT * FROM t3 WHERE c = 3 ORDER BY a;
-- [xref on b] then c is fixed in place, under the cross reference. 2|10|1|c.
SELECT * FROM t3 WHERE b = '10' AND c = 1;
-- [xref on b] the same plan with the conditions in the other order. c = 1 comes first but does not take the cross
-- reference, as b, a key column before it, has a condition too: c is fixed in place. b = '10' then takes it, and c
-- stays fixed under it. 2|10|1|c.
SELECT * FROM t3 WHERE c = 1 AND b = '10';

-- ############################################################################################################
-- Part C: ^STK(g,k), with k START 0 ENDPOINT 5. START is exclusive: the rows are k = 1 through 5, so ^STK(1,0)
-- and ^STK(2,6) are not rows. AIM indexes the subscripts 0 through 5 (its ranges include both ends), so the walk
-- of the cross reference must skip 0 as the table scan does.
SELECT XECUTE_M_CODE('zwrite ^STK');
CREATE TABLE stk (g INTEGER KEY NUM 0, k INTEGER KEY NUM 1 START 0 ENDPOINT 5, v VARCHAR PIECE 1)
  GLOBAL "^STK(keys(""g""),keys(""k""))" READONLY;

-- [no key column xref] the table scan, for comparison: 1|3, 2|3 and 2|5.
SELECT * FROM stk;
-- [xref on k] no row: 0 is START, which is not part of the table.
SELECT * FROM stk WHERE k = 0;
-- [no key column xref] the same, with k fixed in place under g = 1: no row.
SELECT * FROM stk WHERE g = 1 AND k = 0;
-- [xref on k] the same three rows as the table scan. Without a LIMIT, every matching row is read anyway, and the cross
-- reference reads only those.
-- [sort] the range walks several values of k: the rows come out for each value in turn, so not in ORDER BY order.
SELECT * FROM stk WHERE k >= 0 ORDER BY g, k;
-- [no key column xref] the same with a LIMIT: the FOR loops of g, and of k under each g, give the ORDER BY order, so
-- there is no sort, and the LIMIT stops them after two rows: 1|3 and 2|3.
SELECT * FROM stk WHERE k >= 0 ORDER BY g, k LIMIT 2;
-- [no key column xref] the same for an IN, which also gives k several values: 1|3 and 2|3.
SELECT * FROM stk WHERE k IN (3, 5) ORDER BY g, k LIMIT 2;
-- [xref on k] one value: the g under it come out in order, so the cross reference is still used with a LIMIT. 1|3.
SELECT * FROM stk WHERE k = 3 ORDER BY g LIMIT 1;
-- [xref on k] 1|3 and 2|3.
SELECT * FROM stk WHERE k = 3 ORDER BY g;
-- [xref on k] no row: 6 is past ENDPOINT.
SELECT * FROM stk WHERE k = 6;

-- ############################################################################################################
-- Part D: ^ANC(a,b), whose row node holds no value: column n is one level below it, at ^ANC(a,b,"N"). A row exists
-- when the row node or ^ANC(a,b,"N") has a value, so the cross reference watches that level too (the depth, 3).
-- ^ANC(2,6) is a row through the value of its own node; its n is NULL.
SELECT XECUTE_M_CODE('zwrite ^ANC');
CREATE TABLE anc (a INTEGER KEY NUM 0, b INTEGER KEY NUM 1, n VARCHAR GLOBAL "^ANC(keys(""a""),keys(""b""),""N"")")
  GLOBAL "^ANC(keys(""a""),keys(""b""))" READONLY;

-- [no key column xref] the table scan, for comparison: 1|5|one, 2|5|two, 2|6| and 4|8|eight.
SELECT * FROM anc;
-- [xref on b] 1|5|one and 2|5|two: rows whose node has no value of its own.
SELECT * FROM anc WHERE b = 5 ORDER BY a;
-- [xref on b] 2|6|: a row through its own value only.
SELECT * FROM anc WHERE b = 6;

-- ############################################################################################################
-- Part E: two tables joined, each through the cross reference of its own second key column.
-- [xref on id] [xref on b] B|1 and B|2.
-- [sort] the ORDER BY is on anc, whose loops run inside those of gxrf (the accessions under id 76).
SELECT g.accession, x.a FROM gxrf g, anc x WHERE g.id = 76 AND x.b = 5 ORDER BY x.a;
-- A JOIN on the first key column, with a range on the second key column of the outer table: the ON compares stk.g, but
-- fixes anc.a, whose FOR loop comes later, to each stk.g. It does not fix stk.g, so the range on stk.k still walks the
-- cross reference of k. stk has 1|3, 2|3 and 2|5; anc has 1|5, 2|5, 2|6 and 4|8.
-- [xref on k] 1|3|5, 2|3|5 and 2|3|6.
SELECT s.g, s.k, x.b FROM stk s INNER JOIN anc x ON x.a = s.g WHERE s.k >= 3 AND s.k < 5;
-- [no key column xref] the same rows, the tables the other way round: the ON now fixes stk.g, whose FOR loop comes
-- later, to each anc.a, so the loop before k runs once, and k is walked under it in ^STK.
SELECT s.g, s.k, x.b FROM anc x INNER JOIN stk s ON s.g = x.a WHERE s.k >= 3 AND s.k < 5;
-- [no key column xref] the same rows, with the range before the join condition in the ON: the range on stk.k is seen
-- first, and s.g = x.a, not processed yet, fixes stk.g to each anc.a (anc's FOR loop comes first), so k is again walked
-- under it in ^STK.
SELECT s.g, s.k, x.b FROM anc x INNER JOIN stk s ON s.k >= 3 AND s.k < 5 AND s.g = x.a;
-- A range on stk.k with an ORDER BY of stk's key columns and a LIMIT, as in Part C, with anc first in the FROM list.
-- [no key column xref] anc is fixed to its one row 1|5, so its FOR loops run once: stk's FOR loops still give the
-- ORDER BY order, with no sort, and stop after two rows. 1|5|1|3 and 1|5|2|3.
SELECT x.a, x.b, s.g, s.k FROM anc x, stk s WHERE x.a = 1 AND x.b = 5 AND s.k >= 0 ORDER BY s.g, s.k LIMIT 2;
-- [no key column xref] the same, with stk.g also fixed, by the ON, to anc.a = 2: 2|5|2|3 and 2|5|2|5.
SELECT x.a, x.b, s.g, s.k FROM anc x INNER JOIN stk s ON s.g = x.a WHERE x.a = 2 AND x.b = 5 AND s.k >= 0 ORDER BY s.g, s.k LIMIT 2;
-- [sort] anc is not fixed: its FOR loops run outside stk's, so stk's do not give the ORDER BY order. The ON fixes
-- stk.g to each anc.a, so k is walked under it in ^STK, and the rows are sorted: 1|3 and 2|3.
SELECT s.g, s.k FROM anc x INNER JOIN stk s ON s.g = x.a WHERE s.k >= 0 ORDER BY s.g, s.k LIMIT 2;

-- ############################################################################################################
-- Part F: a JOIN whose ON condition fixes a key column to a column of the other table. For each stk row, the cross
-- reference of t3.c is probed with that row's k: only k = 3 has rows in t3 (c = 3: 2|A10|3 and 3|9|3), under each of
-- the two stk rows with k = 3.
-- [xref on c] 1|3|2|A10|3, 1|3|3|9|3, 2|3|2|A10|3 and 2|3|3|9|3.
-- [sort] the ORDER BY items are columns of two tables (s.g, then t.a).
SELECT s.g, s.k, t.a, t.b, t.c FROM stk s INNER JOIN t3 t ON t.c = s.k ORDER BY s.g, t.a;

-- ############################################################################################################
-- Part G: ^TRL(a,b,"X"), a constant subscript after the last key column: the row node is ^TRL(a,b,"X"), so ^TRL(3,5)
-- is not a row. b SKIP '7': the rows with b = 7 are not part of the table.
SELECT XECUTE_M_CODE('zwrite ^TRL');
CREATE TABLE trl (a INTEGER KEY NUM 0, b INTEGER KEY NUM 1 SKIP '7', v VARCHAR PIECE 1)
  GLOBAL "^TRL(keys(""a""),keys(""b""),""X"")" READONLY;
-- [no key column xref] the table scan, for comparison: 1|5|p and 2|5|q.
SELECT * FROM trl;
-- [xref on b] 1|5|p and 2|5|q: not 3|5, whose "X" node does not exist.
SELECT * FROM trl WHERE b = 5 ORDER BY a;
-- [xref on b] no row: 7 is skipped.
SELECT * FROM trl WHERE b = 7;
-- [xref on b] 1|5|p and 2|5|q.
-- [sort] IN gives b several values: the rows come out for each value in turn, so not in ORDER BY order.
SELECT * FROM trl WHERE b IN (5, 7) ORDER BY a;
-- The same table with SKIPCONDITION instead of SKIP: an M expression that is true for the subscripts to skip.
CREATE TABLE trc (a INTEGER KEY NUM 0, b INTEGER KEY NUM 1 SKIPCONDITION "keys(""b"")=7", v VARCHAR PIECE 1)
  GLOBAL "^TRL(keys(""a""),keys(""b""),""X"")" READONLY;
-- [no key column xref] the table scan: 1|5|p and 2|5|q.
SELECT * FROM trc;
-- [xref on b] no row: 7 is skipped.
SELECT * FROM trc WHERE b = 7;
-- [xref on b] 1|5|p and 2|5|q.
-- [sort] IN gives b several values: the rows come out for each value in turn, so not in ORDER BY order.
SELECT * FROM trc WHERE b IN (5, 7) ORDER BY a;
-- A SKIPCONDITION of b that names a, the key column before it: the rows with a = 1 are not part of the table. In the
-- cross reference of b, a comes after b, so the condition is tested once a is read from under each b.
CREATE TABLE trd (a INTEGER KEY NUM 0, b INTEGER KEY NUM 1 SKIPCONDITION "keys(""a"")=1", v VARCHAR PIECE 1)
  GLOBAL "^TRL(keys(""a""),keys(""b""),""X"")" READONLY;
-- [no key column xref] the table scan: 2|5|q and 2|7|r.
SELECT * FROM trd;
-- [xref on b] 2|5|q: not 1|5|p.
SELECT * FROM trd WHERE b = 5;
-- [xref on b] 2|5|q and 2|7|r.
SELECT * FROM trd WHERE b IN (5, 7) ORDER BY b;
-- [xref on b] a range: 2|5|q and 2|7|r.
SELECT * FROM trd WHERE b >= 5 ORDER BY b;
-- [xref on trd.b] a RIGHT JOIN: 2|5 of trd with 1|5 of trl, and 2|7 of trd with NULL, which the second half finds by
-- walking trd in key order.
-- [sort] the second half of a RIGHT JOIN outputs the rows without a match after all the others.
SELECT x.a, x.b, t.a, t.b FROM trl x RIGHT JOIN trd t ON x.a = 1 AND t.b = 5 ORDER BY t.a, t.b;

-- ############################################################################################################
-- Part H: ITERATOR tables (the functions are in TWO21.m). An ITERATOR decides which subscripts of the global are rows,
-- and may skip some. The cross reference indexes every stored subscript and does not call the ITERATOR, so a table with
-- an ITERATOR never uses the cross reference of a key column: the key is fixed in place under the ITERATOR's loops.
SELECT XECUTE_M_CODE('zwrite ^ITR');
CREATE TABLE itr (a INTEGER ITERATOR "$$a^TWO21", b INTEGER ITERATOR "$$b^TWO21", v VARCHAR PIECE 1, PRIMARY KEY (a, b))
  GLOBAL "^ITR(keys(""a""),keys(""b""))";
-- [no key column xref] the rows of the table: 1|5|x, 2|5|y and 2|6|z.
SELECT * FROM itr;
-- [no key column xref] 1|5|x and 2|5|y.
SELECT * FROM itr WHERE b = 5;
-- The same global, with an ITERATOR on a that skips a = 2: the only row is 1|5|x.
CREATE TABLE itrs (a INTEGER ITERATOR "$$aodd^TWO21", b INTEGER ITERATOR "$$b^TWO21", v VARCHAR PIECE 1, PRIMARY KEY (a, b))
  GLOBAL "^ITR(keys(""a""),keys(""b""))";
-- [no key column xref] the rows of the table: 1|5|x.
SELECT * FROM itrs;
-- [no key column xref] 1|5|x, not 2|5|y: ^ITR(2,5) is a stored subscript, which a cross reference of b would give.
SELECT * FROM itrs WHERE b = 5;
CREATE TABLE itrv (a INTEGER ITERATOR "$$a^TWO21" VIRTUAL, b INTEGER ITERATOR "$$b^TWO21", v VARCHAR PIECE 1,
  PRIMARY KEY (a, b)) GLOBAL "^ITR(keys(""a""),keys(""b""))";
-- [no key column xref] the rows of the table, through the ITERATOR functions: 1|5|x, 2|5|y and 2|6|z.
SELECT * FROM itrv;
-- [no key column xref] a is VIRTUAL: 1|5|x and 2|5|y, through the ITERATOR functions.
SELECT * FROM itrv WHERE b = 5;
-- Three keys: b is fixed in place under the ITERATOR of a, and the ITERATOR of c is called with the keys in key order,
-- $$c3^TWO21(a,b,c).
SELECT XECUTE_M_CODE('zwrite ^ITR3');
CREATE TABLE itr3 (a INTEGER ITERATOR "$$a3^TWO21", b INTEGER ITERATOR "$$b3^TWO21", c INTEGER ITERATOR "$$c3^TWO21",
  v VARCHAR PIECE 1, PRIMARY KEY (a, b, c)) GLOBAL "^ITR3(keys(""a""),keys(""b""),keys(""c""))";
-- [no key column xref] the rows of the table: 1|5|1|p, 1|5|2|q, 2|5|1|r and 2|6|1|s.
SELECT * FROM itr3;
-- [no key column xref] 1|5|1|p, 1|5|2|q and 2|5|1|r.
SELECT * FROM itr3 WHERE b = 5;

-- ############################################################################################################
-- Part I: OUTER JOINs, on gxrf and anc as loaded above (anc: 1|5|one, 2|5|two, 2|6| and 4|8|eight).
-- [xref on gxrf.id] a LEFT JOIN whose WHERE fixes the second key of the left table: B|76|1.
SELECT g.accession, g.id, x.a FROM gxrf g LEFT JOIN anc x ON x.a = 1 WHERE g.id = 76;
-- [xref on anc.b] a LEFT JOIN whose ON fixes the second key of the right table: each accession A row with the two
-- anc rows of b = 5.
SELECT g.id, x.a, x.n FROM gxrf g LEFT JOIN anc x ON x.b = 5 WHERE g.accession = 'A' ORDER BY g.id, x.a;
-- [xref on anc.b] no anc row has b = 7: each accession A row with NULLs.
SELECT g.id, x.a, x.n FROM gxrf g LEFT JOIN anc x ON x.b = 7 WHERE g.accession = 'A' ORDER BY g.id;
-- [xref on anc.b] a RIGHT JOIN whose ON fixes the second key of the right table: the anc rows of b = 5 with id 76, and
-- the other anc rows, 2|6 and 4|8, with NULL, which the second half finds by walking anc in key order.
-- [sort] the second half of a RIGHT JOIN outputs the rows without a match after all the others.
SELECT g.id, x.a, x.b FROM gxrf g RIGHT JOIN anc x ON g.id = 76 AND x.b = 5 ORDER BY x.a, x.b;
-- [xref on anc.b] the same as a FULL JOIN: also the gxrf rows other than id 76, with NULL.
-- [sort] as for the RIGHT JOIN: the rows without a match come after all the others.
SELECT g.id, x.a, x.b FROM gxrf g FULL JOIN anc x ON g.id = 76 AND x.b = 5 ORDER BY g.id, x.a, x.b;

-- ############################################################################################################
-- Part J: a table kept in a local variable, lvk(a,b), set in this process. Only a table in a global has an AIM
-- cross reference, so b is fixed in place.
SELECT XECUTE_M_CODE('set lvk(1,5)="a",lvk(2,5)="b",lvk(2,6)="c"');
CREATE TABLE lvk (a INTEGER KEY NUM 0, b INTEGER KEY NUM 1, v VARCHAR PIECE 1) GLOBAL "lvk(keys(""a""),keys(""b""))" READONLY;
-- [no key column xref] the rows of the table: 1|5|a, 2|5|b and 2|6|c.
SELECT * FROM lvk;
-- [no key column xref] 1|5|a and 2|5|b.
SELECT * FROM lvk WHERE b = 5 ORDER BY a;

-- ############################################################################################################
-- Part K: the START, STARTINCLUDE, ENDPOINT and END bounds of a key column that takes its values from the cross
-- reference of another key column, or from the table's global after it. ^BS..^BC(a,b,c): a = 0 to 3, b = 5 and 6,
-- c = 0 to 2, in every combination. AIM indexes all of them, so the bounds have to be applied to the values the cross
-- reference gives.
SELECT XECUTE_M_CODE('for g="^BS","^BI","^BP","^BE","^BC" for a=0:1:3 for b=5,6 for c=0:1:2 set @g@(a,b,c)=a_b_c');
CREATE TABLE bs (a INTEGER KEY NUM 0 START 1, b INTEGER KEY NUM 1, c INTEGER KEY NUM 2, v VARCHAR PIECE 1)
  GLOBAL "^BS(keys(""a""),keys(""b""),keys(""c""))" READONLY;
CREATE TABLE bi (a INTEGER KEY NUM 0 START 2 STARTINCLUDE, b INTEGER KEY NUM 1, c INTEGER KEY NUM 2, v VARCHAR PIECE 1)
  GLOBAL "^BI(keys(""a""),keys(""b""),keys(""c""))" READONLY;
CREATE TABLE bp (a INTEGER KEY NUM 0 ENDPOINT 1, b INTEGER KEY NUM 1, c INTEGER KEY NUM 2, v VARCHAR PIECE 1)
  GLOBAL "^BP(keys(""a""),keys(""b""),keys(""c""))" READONLY;
CREATE TABLE be (a INTEGER KEY NUM 0 END "(keys(""a"")="""")!(keys(""a"")>1)", b INTEGER KEY NUM 1, c INTEGER KEY NUM 2, v VARCHAR PIECE 1)
  GLOBAL "^BE(keys(""a""),keys(""b""),keys(""c""))" READONLY;
CREATE TABLE bc (a INTEGER KEY NUM 0, b INTEGER KEY NUM 1, c INTEGER KEY NUM 2 START 0 ENDPOINT 1, v VARCHAR PIECE 1)
  GLOBAL "^BC(keys(""a""),keys(""b""),keys(""c""))" READONLY;
-- [no key column xref] the rows of each table, by its bounds: bs has a = 2 and 3 (START 1 is exclusive), bi a = 2
-- and 3 (START 2 STARTINCLUDE), bp a = 0 and 1 (ENDPOINT 1), be a = 0 and 1 (END is true past 1), each with b = 5 and
-- 6 and c = 0, 1 and 2 (12 rows); bc has c = 1 only (START 0 ENDPOINT 1), with every a and b (8 rows).
SELECT * FROM bs;
SELECT * FROM bi;
SELECT * FROM bp;
SELECT * FROM be;
SELECT * FROM bc;
-- [xref on bs.b] START 1 on a (exclusive): a = 2 and 3, each with c = 0, 1 and 2.
SELECT a, c FROM bs WHERE b = 5 ORDER BY a, c;
-- [xref on bs.b] the same, through IN and through a range.
-- [sort] an IN list or a range can give b several values, so their rows are sorted, even here, where only b = 5
-- matches: only = fixes a key to one value, whose FOR loop runs once (YDBOcto#1161).
SELECT a, c FROM bs WHERE b IN (5) ORDER BY a, c;
-- [xref on bs.b] [sort] as above: the range can give b several values.
SELECT a, c FROM bs WHERE b > 4 AND b < 6 ORDER BY a, c;
-- [xref on bi.b] START 2 STARTINCLUDE on a: a = 2 and 3.
SELECT a, c FROM bi WHERE b = 5 ORDER BY a, c;
-- [xref on bp.b] ENDPOINT 1 on a: a = 0 and 1.
SELECT a, c FROM bp WHERE b = 5 ORDER BY a, c;
-- [xref on be.b] END on a, "" or a > 1 ends the table: a = 0 and 1.
SELECT a, c FROM be WHERE b = 5 ORDER BY a, c;
-- [xref on bc.b] START 0 ENDPOINT 1 on c, after b: c = 1 only, for each a.
SELECT a, c FROM bc WHERE b = 5 ORDER BY a, c;
