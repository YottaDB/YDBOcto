#################################################################
#                                                               #
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

drop table if exists _test1 keepdata;
create table _test1 (_id1 integer);
insert into _test1 values(1);
insert into _test1(_id1) values(2);
select _id1 as _ from _test1 group by _ order by _;

drop table if exists _test2 keepdata;
create table _test2 (_id1 integer primary key, _fullname text extract "$PIECE(^names(keys(""_id1"")),""|"",1)_$PIECE(^names(keys(""_id1"")),""|"",2)") GLOBAL "^names";
select * from _test2;
