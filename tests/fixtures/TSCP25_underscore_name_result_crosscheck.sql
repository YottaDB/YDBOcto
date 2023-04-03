#################################################################
#                                                              #
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.      #
# All rights reserved.                                         #
#                                                              #
#      This source code contains the intellectual property     #
#      of its copyright holder(s), and is made available       #
#      under a license.  If you do not know the terms of       #
#      the license, please stop and do not read further.       #
#                                                              #
#################################################################

-- TSCP25 : OCTO993 : Allow underscore as starting character in identifiers
select id as _ from names group by _ order by _;
select id as __ from names group by __ order by __;

drop table if exists _test1;
create table _test1 (_id1 integer);
insert into _test1 values(1);
insert into _test1(_id1) values(2);
select _id1 from _test1;
select _id1 as _ from _test1 group by _ order by _;
drop table if exists _test1;

drop table if exists __;
create table __ (__ integer);
insert into __ values(1);
insert into __(__) values(2);
select __ from __;
select __ as ___ from __ group by ___ order by ___;
drop table if exists __;

drop table if exists _test2;
create table _test2 (_id1 integer constraint _constraint check(_id1<1));
insert into _test2 values(0);
select * from _test2;
drop table if exists _test2;
