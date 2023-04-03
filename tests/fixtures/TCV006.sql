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

-- View definition
create view TCV006_1v1 as select id,firstname,lastname from names;
select TCV006_1v1.firstname from TCV006_1v1 group by TCV006_1v1.firstname;
select TCV006_1v1.firstname,count(TCV006_1v1.lastname) from TCV006_1v1 group by TCV006_1v1.firstname;
select TCV006_1v1.lastname from TCV006_1v1 group by 1;
select TCV006_1v1.lastname as test from TCV006_1v1 group by test;

-- Group BY expressions
select 'Cool'!=lastname from TCV006_1v1 group by 'Cool'!=lastname;
select 'Cool'!=lastname from TCV006_1v1 group by lastname;
select id between id and id+1 from TCV006_1v1 group by id between id and id+1;
drop view TCV006_1v1;

-- multi-level query and groupby/aggregate
create view TCV006_1v as select * from names;
select n1.lastname,count(n1.firstname) from TCV006_1v n1 group by n1.lastname HAVING 'Zero' in (select n2.firstname from TCV006_1v n2 where 1 = count(n1.firstname));
select (select firstname) from TCV006_1v group by firstname;
drop view TCV006_1v;

-- Views with aggregate
create view TCV006_1v1 as select count(firstname) from names;
select * from TCV006_1v1;
-- select TCV006_1v1.`count` from TCV006_1v1; commented as this doesn't work in postgres
drop view TCV006_1v1;

create view TCV006_1v1 (v1_count) as select count(firstname) from names;
select count(v1_count) from TCV006_1v1;
drop view TCV006_1v1;

create view TCV006_1v1 as select count(firstname) as aggr from names;
select * from TCV006_1v1;
select count(aggr) from TCV006_1v1;
drop view TCV006_1v1;

-- Views with group by
create view TCV006_1v1 as select firstname,count(id) as aggr from names group by firstname;
select * from TCV006_1v1;
select firstname, count(aggr) from TCV006_1v1 group by firstname;
select firstname from TCV006_1v1 group by firstname having firstname != 'Cool' order by exists (select * from TCV006_1v1 where firstname != 'Zero' order by exists (select * from TCV006_1v1)); --sort-needed-check
select * from TCV006_1v1,TCV006_1v1 as n2;
select * from TCV006_1v1 left join TCV006_1v1 n2 on TCV006_1v1.firstname != NULL;
select * from TCV006_1v1 left join TCV006_1v1 n2 on TCV006_1v1.firstname != NULL where n2.aggr=2;
drop view TCV006_1v1;
