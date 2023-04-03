#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

drop view if exists TCV032_1namesv1;
create view TCV032_1namesv1 as select * from names;
select array(values(1)) from TCV032_1namesv1 group by firstname;
select array(select 1 union all select 1) from TCV032_1namesv1 group by firstname;
select array(select 1 union select 2 union all select 1) from TCV032_1namesv1 group by firstname;
select array(select 1 union select 2) from TCV032_1namesv1 group by firstname;
select array(select firstname from TCV032_1namesv1 order by id) from TCV032_1namesv1;
select array(select firstname from TCV032_1namesv1 order by id), array(select lastname from TCV032_1namesv1) from TCV032_1namesv1;
select array(select id from TCV032_1namesv1) from TCV032_1namesv1;
select array(values(1)) from TCV032_1namesv1 group by firstname;
select array(values(1), (2), (3)) from TCV032_1namesv1;
select concat('my array: ', array(select firstname from TCV032_1namesv1 order by id)::varchar) from TCV032_1namesv1;
select concat('my array: ', array(select lastname from TCV032_1namesv1 order by id)::varchar) from TCV032_1namesv1;
select lastname from TCV032_1namesv1 group by 1 having array(select firstname from TCV032_1namesv1)=array(select firstname from TCV032_1namesv1) order by 1;
select array(select firstname from TCV032_1namesv1) from TCV032_1namesv1 group by firstname;
select array(select firstname from TCV032_1namesv1) from TCV032_1namesv1 group by lastname having array(select firstname from TCV032_1namesv1)=array(select firstname from TCV032_1namesv1) order by array(select firstname from TCV032_1namesv1);
select array(select firstname from TCV032_1namesv1) from TCV032_1namesv1 group by lastname having array(select firstname from TCV032_1namesv1)=array(select firstname from TCV032_1namesv1) order by lastname;
select array(select firstname from TCV032_1namesv1) from TCV032_1namesv1 group by lastname having array(select firstname from TCV032_1namesv1)=array(select firstname from TCV032_1namesv1);
-- Following query is commented as it cannot run with the cross check function.
-- Since other test cases in this fixture are similar in views usage its okay to have this commented for now.
-- select array(select firstname from TCV032_1namesv1) from TCV032_1namesv1 group by lastname having array(select lastname from TCV032_1namesv1)=lastname order by lastname;
select array(select firstname from TCV032_1namesv1) from TCV032_1namesv1 group by lastname;
select array(values(1)) from TCV032_1namesv1 group by firstname;
select array(select 1 union select 2) from TCV032_1namesv1 group by firstname;
drop view if exists TCV032_1namesv1;
