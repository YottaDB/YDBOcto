#################################################################
#                                                               #
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

--Views with Joins
--		Joins with views
create view TCV008v1 as select id,firstname,lastname from names;
select * from TCV008v1,TCV008v1 as n2;
select * from TCV008v1 inner join TCV008v1 n2 on TCV008v1.firstname=n2.firstname;
select * from TCV008v1 left join TCV008v1 n2 on TCV008v1.firstname=n2.firstname;
select * from TCV008v1 right join TCV008v1 n2 on TCV008v1.firstname=n2.firstname;
select * from TCV008v1 full outer join TCV008v1 n2 on TCV008v1.firstname=n2.firstname;
select * from TCV008v1 natural join TCV008v1 n2;
-- fixed key optimization
select * from TCV008v1 where id = 2;
-- Cross reference example
select * from TCV008v1 where lastname = 'Cool';
-- fixed column optimization
SELECT * FROM TCV008v1 AS t1
CROSS JOIN TCV008v1 AS t2
WHERE t1.id = t2.id;
-- Example from Octo doc which performs dnf boolean expression expansion
select * from TCV008v1 where lastName = 'Cool' AND (firstName = 'Zero' OR lastName = 'Burn');
--		   Joins with tables
drop view TCV008v1;
create view TCV008v1 as select id,firstname,lastname from names;
select * from TCV008v1, names as n2;
select * from TCV008v1 inner join names n2 on TCV008v1.firstname = n2.firstname;
select * from TCV008v1 natural join names n2;
--		   Joins with other views
drop view TCV008v1;
create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select column1 as id, column2 as firstname, column3 as lastname from (values(6,'Test','Even')) n1;
select * from TCV008v1,TCV008v2;
select * from TCV008v1 inner join TCV008v2 on TCV008v1.firstname!=TCV008v2.firstname;
select * from TCV008v1 natural join TCV008v2;
--		   Joins with different kind of views
drop view TCV008v2;
create view TCV008v3 as values(6,'Test','Even');
select * from TCV008v1,TCV008v3;
select * from TCV008v1 inner join TCV008v3 on TCV008v1.firstname!=TCV008v3.column2;
select * from TCV008v1 natural join TCV008v3;
select * from names,TCV008v3;
select * from names n1 inner join TCV008v3 on n1.firstname != TCV008v3.column2;
select * from names natural join TCV008v3;
--		   Joins within the view itself
drop view TCV008v3;
drop view TCV008v1;
create view TCV008v1 as select n1.id,n1.firstname,n1.lastname from names n1,names n2;
create view TCV008v2 as select n1.id,n1.firstname,n1.lastname from names n1 left join names n2 on n1.firstname = n2.firstname;
create view TCV008v3 as select n1.* from names n1 right join names n2 on n1.firstname = n2.firstname;
create view TCV008v4 as select n1.* from names n1 natural join names n2;
create view TCV008v5 as select n1.* from names n1 inner join names n2 on n1.firstname = n2.firstname;
create view TCV008v6 as select n1.* from names n1 full outer join names n2 on n1.firstname = n2.firstname;
select * from TCV008v1;
select * from TCV008v2;
select * from TCV008v3;
select * from TCV008v4;
select * from TCV008v5;
select * from TCV008v6;
drop view TCV008v1;
drop view TCV008v2;
drop view TCV008v3;
drop view TCV008v4;
drop view TCV008v5;
drop view TCV008v6;
-- Subqueries
create view TCV008v1 as select firstname,count(id) as aggr from names group by firstname;
create view TCV008v2 as select n1.id,n1.firstname,n1.lastname from names n1 left join names n2 on n1.firstname = n2.firstname;
select firstname from TCV008v1 group by firstname having firstname != 'Cool' order by exists (select * from TCV008v1 where firstname != 'Zero' order by exists (select * from TCV008v1)); --sort-needed-check
select * from TCV008v1 order by exists (select * from TCV008v1 order by exists(select * from TCV008v1)); --sort-needed-check
select * from TCV008v2 order by exists (select * from TCV008v2 order by exists(select * from TCV008v2)); --sort-needed-check
select * from TCV008v1 order by exists (select * from TCV008v1 order by exists(select * from TCV008v1 order by exists (select * from TCV008v1 order by exists (select * from TCV008v1)))); --sort-needed-check
drop view TCV008v1;
drop view TCV008v2;

create view TCV008v as select * from names;
select n1.lastname,count(n1.firstname) from TCV008v n1 group by n1.lastname HAVING 'Zero' in (select n2.firstname from TCV008v n2 where 1 = count(n1.firstname));
select (select firstname) from TCV008v group by firstname;
drop view TCV008v;


-- Joins in view definition
-- join participants are table and view
create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select TCV008v1.id as TCV008v1_id,TCV008v1.firstname as TCV008v1_firstname, TCV008v1.lastname as TCV008v1_lastname, n2.id,n2.firstname,n2.lastname from TCV008v1,names as n2;
select * from TCV008v2;
select * from TCV008v2,TCV008v2 as n3; -- query with 1296 rows in result, only run it once
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select TCV008v1.id as TCV008v1_id,TCV008v1.firstname as TCV008v1_firstname, TCV008v1.lastname as TCV008v1_lastname, n2.id,n2.firstname,n2.lastname from TCV008v1 inner join names as n2 on TCV008v1.firstname=n2.firstname;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select TCV008v1.id as TCV008v1_id,TCV008v1.firstname as TCV008v1_firstname, TCV008v1.lastname as TCV008v1_lastname, n2.id,n2.firstname,n2.lastname from TCV008v1 left join names as n2 on TCV008v1.firstname=n2.firstname;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select TCV008v1.id as TCV008v1_id,TCV008v1.firstname as TCV008v1_firstname, TCV008v1.lastname as TCV008v1_lastname, n2.id,n2.firstname,n2.lastname from TCV008v1 right join names as n2 on TCV008v1.firstname=n2.firstname;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select TCV008v1.id as TCV008v1_id,TCV008v1.firstname as TCV008v1_firstname, TCV008v1.lastname as TCV008v1_lastname, n2.id,n2.firstname,n2.lastname from TCV008v1 full outer join names as n2 on TCV008v1.firstname=n2.firstname;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select TCV008v1.id as TCV008v1_id,TCV008v1.firstname as TCV008v1_firstname, TCV008v1.lastname as TCV008v1_lastname, n2.id,n2.firstname,n2.lastname from TCV008v1 natural join names as n2;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

-- participants are view and view
create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select TCV008v1.id as TCV008v1_id,TCV008v1.firstname as TCV008v1_firstname, TCV008v1.lastname as TCV008v1_lastname, n2.id,n2.firstname,n2.lastname from TCV008v1,TCV008v1 as n2;
select * from TCV008v2;
select * from TCV008v2,TCV008v2 as n3; -- query with 1296 rows in result, only run it once
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select TCV008v1.id as TCV008v1_id,TCV008v1.firstname as TCV008v1_firstname, TCV008v1.lastname as TCV008v1_lastname, n2.id,n2.firstname,n2.lastname from TCV008v1 inner join TCV008v1 as n2 on TCV008v1.firstname=n2.firstname;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select TCV008v1.id as TCV008v1_id,TCV008v1.firstname as TCV008v1_firstname, TCV008v1.lastname as TCV008v1_lastname, n2.id,n2.firstname,n2.lastname from TCV008v1 left join TCV008v1 as n2 on TCV008v1.firstname=n2.firstname;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select TCV008v1.id as TCV008v1_id,TCV008v1.firstname as TCV008v1_firstname, TCV008v1.lastname as TCV008v1_lastname, n2.id,n2.firstname,n2.lastname from TCV008v1 right join TCV008v1 as n2 on TCV008v1.firstname=n2.firstname;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select TCV008v1.id as TCV008v1_id,TCV008v1.firstname as TCV008v1_firstname, TCV008v1.lastname as TCV008v1_lastname, n2.id,n2.firstname,n2.lastname from TCV008v1 full outer join TCV008v1 as n2 on TCV008v1.firstname=n2.firstname;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select TCV008v1.id as TCV008v1_id,TCV008v1.firstname as TCV008v1_firstname, TCV008v1.lastname as TCV008v1_lastname, n2.id,n2.firstname,n2.lastname from TCV008v1 natural join TCV008v1 as n2;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

-- participants are view and VALUES
create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select * from TCV008v1,(VALUES(1,'first','last')) as n2;
select * from TCV008v2;
select * from TCV008v2,TCV008v2 as n3; -- query with 1296 rows in result, only run it once
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select * from TCV008v1 inner join (VALUES(1,'Zero','Cool')) as n2 on TCV008v1.firstname=n2.column2;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select * from TCV008v1 left join (VALUES(1,'Zero','Cool')) as n2 on TCV008v1.firstname=n2.column2;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select * from TCV008v1 right join (VALUES(1,'Zero','Cool')) as n2 on TCV008v1.firstname=n2.column2;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select * from TCV008v1 full outer join (VALUES(1,'Zero','Cool')) as n2 on TCV008v1.firstname=n2.column2;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select * from TCV008v1 natural join (VALUES(1,'Zero','Cool')) as n2;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

-- participants are view and SET Operation
create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select * from TCV008v1, (select 1 as n2_id,'first' as n2_firstname,'last' as n2_lastname union select 2,'Zero','Cool') as n2;
select * from TCV008v2;
select * from TCV008v2,TCV008v2 as n3; -- query with 1296 rows in result, only run it once
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select * from TCV008v1 inner join  (select 1 as n2_id,'first' as n2_firstname,'last' as n2_lastname union select 2,'Zero','Cool') as n2 on TCV008v1.firstname=n2.n2_firstname;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select * from TCV008v1 left join  (select 1 as n2_id,'first' as n2_firstname,'last' as n2_lastname union select 2,'Zero','Cool') as n2 on TCV008v1.firstname=n2.n2_firstname;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select * from TCV008v1 right join (select 1 as n2_id,'first' as n2_firstname,'last' as n2_lastname union select 2,'Zero','Cool') as n2 on TCV008v1.firstname=n2.n2_firstname;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select * from TCV008v1 full outer join (select 1 as n2_id,'first' as n2_firstname,'last' as n2_lastname union select 2,'Zero','Cool') as n2 on TCV008v1.firstname=n2.n2_firstname;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v1 as select id,firstname,lastname from names;
create view TCV008v2 as select * from TCV008v1 natural join (select 1 as n2_id,'first' as n2_firstname,'last' as n2_lastname union select 2,'Zero','Cool') as n2;
select * from TCV008v2;
drop view TCV008v2;
drop view TCV008v1;

create view TCV008v as select * from names;
select (select id from TCV008v limit 1), (select firstname from TCV008v limit 1);
select (select id from TCV008v limit 1), (select TCV008v.firstname from TCV008v,TCV008v as n1 limit 1);
select (select id from TCV008v limit 1), (select TCV008v.firstname from TCV008v inner join TCV008v as n1 on (TCV008v.firstname=n1.firstname) limit 1);
select (select id from TCV008v limit 1), (select TCV008v.firstname from TCV008v full outer join TCV008v as n1 on (TCV008v.firstname=n1.firstname) limit 1);
select (select id from TCV008v limit 1), (select TCV008v.firstname from TCV008v left outer join TCV008v as n1 on (TCV008v.firstname=n1.firstname) limit 1);
select (select id from TCV008v limit 1), (select TCV008v.firstname from TCV008v left outer join TCV008v as n1 on (TCV008v.firstname=n1.firstname) where(n1.id=1) limit 1);
drop view TCV008v;

-- Fixed key optimization
-- select * from TCV008v1 where id = 2;
create view TCV008v as select * from names where id=2;
select * from TCV008v;
select * from TCV008v where id = 2;
drop view TCV008v;
-- Cross reference example
-- select * from TCV008v1 where lastname = 'Cool';
create view TCV008v as select * from names where lastname = 'Cool';
select * from TCV008v;
select * from TCV008v where lastname = 'Cool';
drop view TCV008v;
-- Fixed column optimization
-- SELECT * FROM TCV008v1 AS t1
-- CROSS JOIN TCV008v1 AS t2
-- WHERE t1.id = t2.id;
create view TCV008v(id1,firstname1,lastname1,id2,firstname2,lastname2) as select * from names as t1 cross join names as t2 where t1.id = t2.id;
select * from TCV008v;
select * from TCV008v as t1 cross join TCV008v as t2 where t1.id1 = t2.id1;
drop view TCV008v;
-- Example from Octo doc which performs dnf boolean expression expansion
-- select * from TCV008v1 where lastName = 'Cool' AND (firstName = 'Zero' OR lastName = 'Burn');
create view TCV008v as select * from names where lastname = 'Cool' AND (firstname = 'Zero' OR lastname = 'Burn');
select * from TCV008v;
select * from TCV008v where lastname = 'Cool' AND (firstname = 'Zero' OR lastname = 'Burn');
drop view TCV008v;
