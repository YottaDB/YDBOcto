#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select n1.id,n2.id from names n1 inner join names n2 on (n1.id = (select n3.id from names n3 where n1.id = n3.id));
select n1.id,n2.id from names n1 left  join names n2 on (n1.id = (select n3.id from names n3 where n1.id = n3.id));
select n1.id,n2.id from names n1 right join names n2 on (n1.id = (select n3.id from names n3 where n1.id = n3.id));
select n1.id,n2.id from names n1 inner join names n2 on (1 = (select n3.id from names n3 where n1.id = n3.id));
select n1.id,n2.id from names n1 left  join names n2 on (1 = (select n3.id from names n3 where n1.id = n3.id));
select n1.id,n2.id from names n1 right join names n2 on (1 = (select n3.id from names n3 where n1.id = n3.id));
-- Query using OUTER JOIN and sub-queries in ON clause AND WHERE clause
select n1.id,n2.id from names n1 inner join names n2 on (n1.id = (select n3.id from names n3 where n1.id = n3.id)) WHERE n1.id = n2.id + (select 1) + (select 1 from names n4 where n4.id = n2.id);
select n1.id,n2.id from names n1 left  join names n2 on (n1.id = (select n3.id from names n3 where n1.id = n3.id)) WHERE n1.id = n2.id + (select 1) + (select 1 from names n4 where n4.id = n2.id);
select n1.id,n2.id from names n1 right join names n2 on (n1.id = (select n3.id from names n3 where n1.id = n3.id)) WHERE n1.id = n2.id + (select 1) + (select 1 from names n4 where n4.id = n2.id);
select n1.id,n2.id from names n1 full  join names n2 on (n1.id = (select n3.id from names n3 where n1.id = n3.id)) WHERE n1.id = n2.id + (select 1) + (select 1 from names n4 where n4.id = n2.id);
-- Below are queries from octo199.sql that could not be tested because of #361 so they are included in octo361.sql
select n1.id,n2.id from names n1 left  join names n2 on n1.id = n2.id AND EXISTS (select * from names);
select n1.id,n2.id from names n1 left  join names n2 on n1.id = n2.id AND NOT EXISTS (select * from names);
select n1.id,n2.id from names n1 left  join names n2 on n1.id = n2.id OR EXISTS (select * from names where n1.id < n2.id);
select n1.id,n2.id from names n1 left  join names n2 on n1.id = n2.id OR NOT EXISTS (select * from names where n1.id < n2.id);
select n1.id,n2.id from names n1 left  join names n2 on EXISTS (select * from names);
select n1.id,n2.id from names n1 left  join names n2 on NOT EXISTS (select * from names);
select n1.id,n2.id from names n1 left  join names n2 on EXISTS (select * from names where n1.id < n2.id);
select n1.id,n2.id from names n1 left  join names n2 on NOT EXISTS (select * from names where n1.id < n2.id);
select n1.id,n2.id from names n1 right join names n2 on n1.id = n2.id AND EXISTS (select * from names);
select n1.id,n2.id from names n1 right join names n2 on n1.id = n2.id AND NOT EXISTS (select * from names);
select n1.id,n2.id from names n1 right join names n2 on n1.id = n2.id OR EXISTS (select * from names where n1.id < n2.id);
select n1.id,n2.id from names n1 right join names n2 on n1.id = n2.id OR NOT EXISTS (select * from names where n1.id < n2.id);
select n1.id,n2.id from names n1 right join names n2 on EXISTS (select * from names);
select n1.id,n2.id from names n1 right join names n2 on NOT EXISTS (select * from names);
select n1.id,n2.id from names n1 right join names n2 on EXISTS (select * from names where n1.id < n2.id);
select n1.id,n2.id from names n1 right join names n2 on NOT EXISTS (select * from names where n1.id < n2.id);
