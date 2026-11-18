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

-- TOB27 : OCTO1193 : GROUP BY with an ORDER BY on the grouped column returns its rows in ORDER BY order
-- With a GROUP BY, a HAVING or an aggregate function, the rows are output from the groups after the FOR loops over the
-- table end, so the order of those FOR loops is not the order of the output. The ORDER BY was removed all the same when
-- the FOR loops alone would have given its order, and the rows came out unsorted.

drop table if exists TOB27a;
create table TOB27a (x integer, y integer, v varchar, primary key (x, y));
insert into TOB27a values (1,30,'Pa'),(1,50,'Qa'),(2,10,'Pb'),(3,30,'Qa'),(3,40,'Pa');
drop table if exists TOB27b;
create table TOB27b (id integer primary key, v varchar);
insert into TOB27b values (-20,'b'),(-5,'a'),(3,'b'),(7,'b'),(10,'c');

-- A range on the grouped non-key column, which walks its cross reference: Nikon, Killer, Cool, Burn
select lastname, count(*) from names where lastname < 'zz' group by lastname order by lastname desc;
-- The same on a table with two key columns: (Qa,2) (Pb,1) (Pa,2)
select v, count(*) from TOB27a where v < 'R' group by v order by v desc;
-- A LIKE prefix on the grouped column, which walks its cross reference too: (Pb,1) (Pa,2)
select v, count(*) from TOB27a where v like 'P%' group by v order by v desc;
-- No cross reference: GROUP BY and ORDER BY on the first key column. (3,2) (2,1) (1,2)
select x, count(*) from TOB27a group by x order by x desc;
-- and on the second key column, with the first fixed to one value. (40,1) (30,1)
select y, count(*) from TOB27a where x = 3 group by y order by y desc;
-- Ascending too, as the groups are not output in number order. (-20,1) (-5,1) (3,1) (7,1) (10,1)
select id, count(*) from TOB27b group by id order by id;
-- With a LIMIT, the first rows of that order: (-20,1) (-5,1)
select id, count(*) from TOB27b group by id order by id limit 2;
-- With a HAVING: (10,1) (7,1) (3,1) (-5,1) (-20,1)
select id, count(*) from TOB27b group by id having count(*) = 1 order by id desc;
-- An aggregate function in the ORDER BY: b has 3 rows, then a and c with 1 each, in the order of v
select v, count(*) from TOB27b group by v order by count(*) desc, v;
