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
-- Part A (crosschecked against PostgreSQL)
-- A VARCHAR primary key. The table's global holds it in M collation, canonical numbers first
-- (2, 9, 10, 12, 13, 120, 123, then 12A, A123, B7), which is not SQL's order. Its cross reference holds "#"_value,
-- which is. So the ORDER BY and the LIKE prefix walk the cross reference instead of sorting or checking every row.
drop table if exists TOB25a;
create table TOB25a (k varchar primary key);
insert into TOB25a values ('9'),('10'),('123'),('A123'),('B7'),('12'),('120'),('12A'),('13'),('2');
-- The rows of the table, in M order: 2 9 10 12 13 120 123 12A A123 B7
select * from TOB25a;
-- [no sort, ascending walk] 10 12 120 123 12A 13 2 9 A123 B7
select k from TOB25a order by k;
-- [no sort, descending walk] B7 A123 9 2 13 12A 123 120 12 10
select k from TOB25a order by k desc;
-- [no sort, descending walk] the LIMIT stops the walk after 3 rows: B7 A123 9
select k from TOB25a order by k desc limit 3;
-- [no sort, ascending walk] only the values that start with 12: 12 120 123 12A
select k from TOB25a where k like '12%' order by k;
-- [no sort, descending walk] from the end of the 12 prefix: 12A 123
select k from TOB25a where k like '12%' order by k desc limit 2;
-- [no sort, ascending walk] 10 12 120 123
select k from TOB25a where k like '1%' order by k limit 4;

-- ############################################################################################################
-- Part B (crosschecked against PostgreSQL)
-- Two INTEGER keys (x, y). The FOR loop of x is outside the one of y in the table's global, so an ORDER BY y
-- walks the cross reference of y, and x comes from the cross reference under each y.
drop table if exists TOB25b;
create table TOB25b (x integer, y integer, v varchar, primary key (x, y));
insert into TOB25b values (1,30,'P'),(1,50,'Q'),(2,10,'P'),(3,30,'Q'),(3,40,'P');
-- The rows of the table: (1,30,P) (1,50,Q) (2,10,P) (3,30,Q) (3,40,P)
select * from TOB25b;
-- [no sort, ascending walk] (2,10) (1,30) (3,30) (3,40) (1,50)
select x, y from TOB25b order by y, x;
-- [no sort, descending walk] (1,50) (3,40) (3,30) (1,30) (2,10)
select x, y from TOB25b order by y desc, x desc;
-- [no sort, ascending walk] (2,10) (1,30)
select x, y from TOB25b order by y limit 2;
-- [no sort, ascending walk] With a LIMIT, the ORDER BY chooses: the walk of y, and v = 'P' is checked on each row.
-- (2,10,P) (1,30,P)
select x, y, v from TOB25b where v = 'P' order by y limit 2;
-- [sort] Without a LIMIT, the WHERE chooses: the cross reference of v, then a sort. (2,10,P) (1,30,P) (3,40,P)
select x, y, v from TOB25b where v = 'P' order by y;
-- [no sort] x is fixed to one value, so its FOR loop runs once and the table's global gives the order of y
-- (YDBOcto#1161). No walk. (3,30) (3,40)
select x, y from TOB25b where x = 3 order by y;
-- [no sort, ascending walk] With a LIMIT, a row comparison in an ON condition does not stop the walk of y, as it
-- compares no single key column. (2,10) (1,30) (3,30)
select b1.x, b1.y from TOB25b b1 inner join TOB25b b2 on b1.* = b2.* order by b1.y limit 3;
-- [sort] With a GROUP BY, the rows are output from the groups, not in the order of the walk: the range on y walks its
-- cross reference, and the ORDER BY still sorts. (50,1) (40,1) (30,2) (10,1)
select y, count(*) from TOB25b where y > 0 group by y order by y desc;

-- ############################################################################################################
-- Part C (crosschecked against PostgreSQL)
-- Three keys (s, t, q), as in specimen, test code, sequence: a WHERE on t and an ORDER BY s.
drop table if exists TOB25c;
create table TOB25c (s varchar, t varchar, q integer, primary key (s, t, q));
insert into TOB25c values ('9','PSA',1),('10','PSA',1),('10','CBC',2),('123','CBC',1),('A123','PSA',2),('B7','PSA',1);
-- The rows of the table, in M order: (9,PSA,1) (10,CBC,2) (10,PSA,1) (123,CBC,1) (A123,PSA,2) (B7,PSA,1)
select * from TOB25c;
-- [no sort, descending walk] With the LIMIT, the walk of s backwards; t = 'PSA' is checked under each s.
-- (B7,PSA,1) (A123,PSA,2)
select s, t, q from TOB25c where t = 'PSA' order by s desc limit 2;
-- [sort] Without a LIMIT, the WHERE chooses: the cross reference of t, then a sort on s.
-- (B7,PSA,1) (A123,PSA,2) (9,PSA,1) (10,PSA,1)
select s, t, q from TOB25c where t = 'PSA' order by s desc;
-- [sort] A LIKE prefix on the second key column walks its cross reference; the ORDER BY then sorts.
-- (10,CBC,2) (123,CBC,1)
select s, t, q from TOB25c where t like 'C%' order by s, t, q;
-- [sort] s fixed; ORDER BY t, a VARCHAR key column, whose M collation is not SQL's order. (10,CBC,2) (10,PSA,1)
select s, t, q from TOB25c where s = '10' order by t, q;
-- [sort] s fixed: no cross reference, the LIKE is checked on each row under s. (10,CBC,2)
select s, t, q from TOB25c where s = '10' and t like 'C%' order by s, t, q;
-- [sort] With a GROUP BY, as in Part B, on a VARCHAR key column. (CBC,2) (PSA,4)
select t, count(*) from TOB25c where t < 'Q' group by t order by t;
-- [no sort, ascending walk] With a LIMIT, the ORDER BY chooses the walk of s before the WHERE is looked at. The range
-- on q, a later key column, then finds the table already walking a cross reference, and walks q in the table's global
-- under each s and t.
-- (10,CBC,2) (A123,PSA,2)
select s, t, q from TOB25c where q > 1 order by s limit 2;
-- [no sort, ascending walk] The same for an IN on q. (10,CBC,2) (A123,PSA,2)
select s, t, q from TOB25c where q in (2, 3) order by s limit 2;

-- ############################################################################################################
-- Part D (crosschecked against PostgreSQL)
-- A LEFT JOIN on a walked table that has one key column and a non-key column. Its only key is the cross
-- reference key, whose value the NULL check of a LEFT JOIN reads before it reads the non-key column (YDBOcto#1128).
drop table if exists TOB25f;
create table TOB25f (k varchar primary key, v varchar);
insert into TOB25f values ('9','NINE'),('10','TEN'),('123','ONE23'),('B7','BSEVEN'),('7','SEVEN');
-- The rows of the table, in M order: (7,SEVEN) (9,NINE) (10,TEN) (123,ONE23) (B7,BSEVEN)
select * from TOB25f;
-- [no sort, descending walk] B7 9 7 123, each with the t of its TOB25c row. 7 has no TOB25c row, so its t is NULL:
-- the LEFT JOIN outputs it, through the NULL check of the walked key, with an empty t (how Octo shows NULL).
select f.k, f.v, c.t from TOB25f f left join TOB25c c on c.s = f.k order by f.k desc limit 4;
-- An UPDATE whose LIKE prefix walks the cross reference of the table's only key column updates the row in the table.
-- (10,X) (123,X) and the others unchanged.
update TOB25f set v = 'X' where k like '1%';
select * from TOB25f order by k;
-- The same when the UPDATE changes the key column itself: B7 becomes QB7, which LIKE 'Q%' then finds, and LIKE 'B%'
-- does not.
update TOB25f set k = 'Q' || k where k like 'B%';
select * from TOB25f where k like 'Q%';
select * from TOB25f where k like 'B%';
-- And back, so that the table is as before for the next run of these statements: (10,TEN) (123,ONE23) (B7,BSEVEN)
update TOB25f set k = 'B7' where k like 'QB%';
update TOB25f set v = 'TEN' where k like '10%';
update TOB25f set v = 'ONE23' where k like '12%';
select * from TOB25f order by k;
