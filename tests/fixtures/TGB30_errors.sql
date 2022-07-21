#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- `table.*` with alias usage
select n1.* as x from names n1 group by x;

-- Ambiguous column error as same alias represents to column references
select id as id1, firstname as id1 from names order by id1;

-- Following works in Postgres but not in Octo because of #855
select id as id1, id as id1 from names group by id1;


-- inner query alias in an expression of outer query
SELECT ALL last_name as ln1 FROM customers c GROUP BY last_name ORDER BY EXISTS (SELECT 1 FROM customers n1 ORDER BY count(ln1=n1.last_name)=1);

-- alias usage in an expression
select lastname as ln1 from names group by ln1 || 'test';

-- For alias in GROUP BY priority of association is alias or names in JOINS first then select list alias
-- Following query demonstrates this by generating a ERR_GROUP_BY_OR_AGGREGATE_FUNCTION for lastname
-- Simlar query with n1.ln1 works fine and is included in other fixture of this subtest
select lastname as ln1 from names , (select id as ln1 from names)n2 group by ln1;

-- Expressions in GROUP BY
select count(firstname) as cnt from names group by cnt;
select firstname || 'test', lastname as conct from names group by conct;
select firstname || 'test' as conct, lastname from names group by conct;

-- Ambiguity usecases
select max(lastname) as alias, firstname as alias, max(id) as alias from names group by alias;
select 'test'||firstname as alias, firstname as alias, 'last' || firstname as alias from names group by alias;

-- Test alias name ambiguity in FROM/JOIN column list
select 1, 2 from names n1, names n2 group by firstname;

-- Test column name ambiguity in SELECT column list
select 1 as ambiguous, 2 as ambiguous from names group by ambiguous;

-- Misc
select (select lastname from names group by fn), firstname as fn from names;
select firstname as fn from names n1 order by exists (select lastname from names n2 group by n1.fn);
select lastname as ln1 from names , (select id as ln1 from names)n2 order by exists (select lastname from names n2 group by n2.ln1);
select firstname from (select lastname as ln1,firstname from names)n1 order by exists (select n2.firstname ||'test' from (select firstname as ln1 from names) n2 group by ln1);
select firstname from (select lastname as ln1,firstname from names)n1 order by exists (select n2.firstname ||'test' from (select firstname from names) n2 group by ln1);
select firstname from (select lastname as ln1,firstname from names)n1 order by exists (select n2.firstname from (select firstname from names) n2 group by ln1);
select (select id from names n2 limit 1) as sub from names group by sub;
