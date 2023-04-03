#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Subquery within view definition
-- Single subquery
create view TCV015_1v1 as select * from (select id,firstname,lastname from names) n1;
select * from TCV015_1v1;
drop view TCV015_1v1;

create view TCV015_1v1 (v1_n1_id,v1_n1_firstname,v1_n1_lastname,v1_n2_id,v1_n2_firstname,v1_n2_lastname) as select * from (select id, firstname, lastname from names)n1, (select id, firstname,lastname from names)n2;
select * from TCV015_1v1;
drop view TCV015_1v1;

create view TCV015_1v1 (v1_n1_id,v1_n1_firstname,v1_n1_lastname,v1_n2_id,v1_n2_firstname,v1_n2_lastname) as select * from (select id, firstname, lastname from names)n1, (select id, firstname,lastname from names)n2 where n2.id < 3;
select * from TCV015_1v1;
drop view TCV015_1v1;

create view TCV015_1v1 as select n1.id as v1_id,n1.firstname as v1_firstname,n1.lastname as v1_lastname,n2.id as v1_n2_id,n2.firstname as v1_n2_firstname,n2.lastname as v1_n2_lastname from (select * from names) n1,(select * from names) n2;
select * from TCV015_1v1;
drop view TCV015_1v1;

create view TCV015_1v1 (v1_n1_id,v1_n1_firstname,v1_n1_lastname,v1_n2_id,v1_n2_firstname,v1_n2_lastname) as select * from (select * from names) n1,(select * from names) n2;
create view TCV015_1v2 as select n1.id as v1_id,n1.firstname as v1_firstname,n1.lastname as v1_lastname,n2.id as v1_n2_id,n2.firstname as v1_n2_firstname,n2.lastname as v1_n2_lastname from (select * from names) n1 left join (select * from names) n2 on n1.firstname = n2.firstname;
create view TCV015_1v3 as select n1.id as v1_id,n1.firstname as v1_firstname,n1.lastname as v1_lastname,n2.id as v1_n2_id,n2.firstname as v1_n2_firstname,n2.lastname as v1_n2_lastname from (select * from names) n1 right join (select * from names) n2 on n1.firstname = n2.firstname;
create view TCV015_1v4 as select n1.id as v1_id,n1.firstname as v1_firstname,n1.lastname as v1_lastname,n2.id as v1_n2_id,n2.firstname as v1_n2_firstname,n2.lastname as v1_n2_lastname from (select * from names) n1 natural join (select * from names) n2;
create view TCV015_1v5  as select n1.id as v1_id,n1.firstname as v1_firstname,n1.lastname as v1_lastname,n2.id as v1_n2_id,n2.firstname as v1_n2_firstname,n2.lastname as v1_n2_lastname from (select * from names) n1 inner join (select * from names) n2 on n1.firstname = n2.firstname;
create view TCV015_1v6 as select n1.id as v1_id,n1.firstname as v1_firstname,n1.lastname as v1_lastname,n2.id as v1_n2_id,n2.firstname as v1_n2_firstname,n2.lastname as v1_n2_lastname from (select * from names) n1 full outer join (select * from names) n2 on n1.firstname = n2.firstname;
select * from TCV015_1v1;
select * from TCV015_1v2;
select * from TCV015_1v3;
select * from TCV015_1v4;
select * from TCV015_1v5;
select * from TCV015_1v6;
drop view TCV015_1v1;
drop view TCV015_1v2;
drop view TCV015_1v3;
drop view TCV015_1v4;
drop view TCV015_1v5;
drop view TCV015_1v6;

create view TCV015_1v2 as select n1.* from (select * from names) n1 left join (select * from names) n2 on n1.firstname = n2.firstname where n1.lastName = 'Cool' AND (n1.firstName = 'Zero' OR n1.lastName = 'Burn');
select * from TCV015_1v2;
drop view TCV015_1v2;
create view TCV015_1v1 as select  n1.id as v1_id,n1.firstname as v1_firstname,n1.lastname as v1_lastname,n2.id as v1_n2_id,n2.firstname as v1_n2_firstname,n2.lastname as v1_n2_lastname from (select * from names) n1 left join (select * from names) n2 on n1.firstname = n2.firstname where n1.lastName = 'Cool' AND (n1.firstName = 'Zero' OR n1.lastName = 'Burn');
select * from TCV015_1v1;
drop view TCV015_1v1;

-- Multiple nested subqueries
create view TCV015_1v1 as select * from (select id,firstname,lastname from (select id,firstname,lastname from (select id,firstname,lastname from (select id,firstname,lastname from names)n1)n2)n3)n4;
select * from TCV015_1v1;
select firstname from TCV015_1v1 group by firstname;
select firstname from TCV015_1v1 group by firstname having count(lastname)<2;
drop view TCV015_1v1;

