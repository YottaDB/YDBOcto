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
create view v1 as select id,firstname,lastname from names;
select v1.firstname,v1.lastname from v1 group by v1.firstname; -- ERROR
select v1.firstname,count(v1.lastname) from v1; -- ERROR
select count(v1.lastname) from v1 group by 1; -- ERROR
select v1.lastname,v1.firstname from group by 1; -- ERROR
select v1.lastname as test, v1.firstname from group by test; -- ERROR
drop view v1;

-- multi-level query and groupby/aggregate
create view c1 as select * from customers;
create view o1 as select * from orders;
select (select lastname) fron v group by firstname; -- ERROR
drop view c1;
drop view o1;

-- CREATE views with group by
create view v1 as select firstname,count(id) as aggr from names group by firstname;
select firstname,aggr from v1 group by firstname; -- error
drop view v1;
