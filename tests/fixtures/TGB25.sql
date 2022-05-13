#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- So far have discovered 3 types
-- 1) Constant sub query without from clause
-- 	Ex: (select 1)
-- 2) Variable sub query without from clause
--	Ex: (select firstname)
-- 3) Complete sub query with from clause and limit 1
--	Ex: (select 1 from names limit 1)
--	Ex: (select firstname from names limit 1)

-- Below queries test GroupBy in sub-queries using expression having one or more columns from outer queries
-- Both will end up having an empty GroupBy list. Octo will consider it as if GroupBy was never used.
-- This deviates from Postgres behavior as it still considers GroupBy exists and issues GroupBy related error
SELECT id,firstname FROM names n1 WHERE id IN (SELECT n2.id FROM names n2 group by n1.id=0);
SELECT id,firstname FROM names n1 WHERE id IN (SELECT n2.id=0 FROM names n2 group by n1.id=0);

-- Below queries generate error saying subqueries are not allowed in GROUP BY
select (select id from names limit 1) from names group by 1;
select (select id from names) from names group by 1;
-- The following query where limit is not used in the subquery of GroupBy should work in Octo with the latest expression changes as we invoke sub_query_check() in generate_physical_plan() for GroupBy as well.
-- But need to test how it would work with other clauses. Will it work equivalent to `firstname` usage instead of `(select firstname)`?
select (select 2) from names group by (select firstname);
select (select 2), count(firstname) from names group by (select 3);
select (select 1) from names group by (select firstname); -- Should work
select (select 2) from names group by 1; -- Should work
select (select firstname) from names group by (select firstname); -- Should work
select (select lastname) from names group by (select firstname); -- ungrouped names.lastname error
select (select firstname) from names group by (select 1 from names limit 1); -- ungrouped names.firstname error
select (select firstname) from names group by (select 1 from names limit 1),firstname; -- ungrouped names.firstname error
select (select firstname) from names group by (select 1 from names); -- ungrouped names.firstname error
select (select 1 from names) from names group by (select 1 from names); -- multiple rowed subquery error
-- Having
select (select firstname) from names group by (select firstname) having (select firstname)!='Zero'; -- Should work
select (select firstname) from names group by (select firstname) having firstname !='Zero'; -- names.firstname of having clause must appear in GroupBy error
select (select firstname) from names group by (select firstname) having (select lastname) !='Zero'; -- subquery uses ungrouped names.lastname from outer query error
select firstname from names group by (select firstname);
select 1 from names group by firstname!=(select firstname from names);
select 1 from names group by not (select firstname from names);

-- All queries with subqueries in GroupBy will generate ERR_GROUP_BY_SUB_QUERY
-- Following queries use cast operation in subquery and this used to generate assert failure
-- during qualify_statement's call to hash_canonical_query.
select 1 from names n1 group by n1.id NOT IN (select 1 union select NULL::integer union select 2);
select n1.id from names n1 group by n1.id NOT IN (select 1 union select NULL::integer union select 2);
select n1.id NOT IN (1,2) from names n1 group by n1.id NOT IN (select 1 union select NULL::integer union select 2);
select n1.id NOT IN (select 1 union select NULL::integer union select 2) from names n1 group by n1.id NOT IN (select 1 union select NULL::integer union select 2);

-- Following use unary operation by `exists` usage. As subqueries are not enabled in GroupBy the query fails with ERR_GROUP_BY_SUB_QUERY error.
select 1 from names group by exists(select id from names);
select 1,count(firstname) from names group by exists(select id from names);
select exists(select firstname from names),count(firstname) from names group by exists(select firstname from names);
select firstname,count(firstname) from names group by exists(select firstname from names);

-- Following queries use NOT IN operation with subquery usage
select 1 from names n1 group by n1.id NOT IN (select 1 union select 3 union select 2);
select n1.id from names n1 group by n1.id NOT IN (select 1 union select 3 union select 2); -- error case
select n1.id NOT IN (1,2,3) from names n1 group by n1.id NOT IN (select 1 union select 3 union select 2); -- error case
select n1.id NOT IN (select 1 union select 3 union select 2) from names n1 group by n1.id NOT IN (select 1 union select 3 union select 2);

-- misc
select lastname in (select 'Cool') from names group by lastname in (select 'Cool') having lastname in (select 'Cool');
