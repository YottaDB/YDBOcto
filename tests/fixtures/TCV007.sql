#################################################################
#                                                               #
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.  #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

drop view if exists TCV007_1v1;
create view TCV007_1v1 as select id,firstname,lastname from names;
select TCV007_1v1.firstname from TCV007_1v1 order by TCV007_1v1.firstname;
select TCV007_1v1.firstname from TCV007_1v1 order by TCV007_1v1.lastname;
select TCV007_1v1.firstname from TCV007_1v1 order by 1;
select TCV007_1v1.firstname as test from TCV007_1v1 order by test;
select distinct TCV007_1v1.lastname,TCV007_1v1.firstname from TCV007_1v1 order by TCV007_1v1.lastname;
select distinct TCV007_1v1.lastname,TCV007_1v1.firstname from TCV007_1v1 order by TCV007_1v1.firstname,TCV007_1v1.lastname;
drop view TCV007_1v1;
drop view if exists TCV007_1namesv1;
create view TCV007_1namesv1 as select * from names;
select distinct (select n1.lastname from TCV007_1namesv1 n1 limit 1) from TCV007_1namesv1 order by (select n1.lastname from TCV007_1namesv1 n1 limit 1);
select distinct (select 1 from TCV007_1namesv1 n1 limit 1) from TCV007_1namesv1 order by (select 1 from TCV007_1namesv1 n1 limit 1);
select distinct (select 1 from TCV007_1namesv1 limit 1) from TCV007_1namesv1 order by (select 1 from TCV007_1namesv1 limit 1);
drop view TCV007_1namesv1;
drop view if exists TCV007_1namesv2;
create view TCV007_1namesv2 as select 1;
select distinct (select 1 from TCV007_1namesv2 limit 1) order by (select 1 from TCV007_1namesv2 limit 1);
drop view TCV007_1namesv2;
