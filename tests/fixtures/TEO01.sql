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

select EXISTS (select * from names) from names;
select NOT EXISTS (select * from names) from names;
select EXISTS (select n2.id from names n2 where n2.id < n1.id AND n1.id <= 3 limit 1) from names n1;
select NOT EXISTS (select n2.id from names n2 where n2.id < n1.id AND n1.id <= 3 limit 1) from names n1;
select EXISTS (select n2.id from names n2 where n2.id < n1.id AND n1.id <= 3 limit 0) from names n1;
select n1.id,n2.id from names n1 inner join names n2 on n1.id = n2.id AND EXISTS (select * from names);
select n1.id,n2.id from names n1 inner join names n2 on n1.id = n2.id AND NOT EXISTS (select * from names);
select n1.id,n2.id from names n1 inner join names n2 on n1.id = n2.id OR EXISTS (select * from names where n1.id < n2.id);
select n1.id,n2.id from names n1 inner join names n2 on n1.id = n2.id OR NOT EXISTS (select * from names where n1.id < n2.id);
select n1.id,n2.id from names n1 inner join names n2 on EXISTS (select * from names);
select n1.id,n2.id from names n1 inner join names n2 on NOT EXISTS (select * from names);
select n1.id,n2.id from names n1 inner join names n2 on EXISTS (select * from names where n1.id < n2.id);
select n1.id,n2.id from names n1 inner join names n2 on NOT EXISTS (select * from names where n1.id < n2.id);
select * from names n1 where EXISTS (select n2.id from names n2 where n2.id = n1.id + 2);
select * from names n1 where NOT EXISTS (select n2.id from names n2 where n2.id = n1.id + 2);
select * from names n1 where n1.id > 2  OR NOT EXISTS (select n2.id from names n2 where n2.id = n1.id + 2);
select * from names n1 where n1.id > 2  OR     EXISTS (select n2.id from names n2 where n2.id = n1.id + 2);
select * from names n1 where n1.id > 2 AND NOT EXISTS (select n2.id from names n2 where n2.id = n1.id + 2);
select * from names n1 where n1.id > 2 AND     EXISTS (select n2.id from names n2 where n2.id = n1.id + 2);
select * from names n1 where NOT EXISTS (select n2.id from names n2 where n2.id = n1.id + 2) OR  n1.id > 2;
select * from names n1 where     EXISTS (select n2.id from names n2 where n2.id = n1.id + 2) OR  n1.id > 2;
select * from names n1 where NOT EXISTS (select n2.id from names n2 where n2.id = n1.id + 2) AND n1.id > 2;
select * from names n1 where     EXISTS (select n2.id from names n2 where n2.id = n1.id + 2) AND n1.id > 2;
select * from (select * from (select * from names n1 where EXISTS (select n2.id from names n2 where n2.id = n1.id + 2)) n3) n4;
select * from (select * from (select * from names n1 where NOT EXISTS (select n2.id from names n2 where n2.id = n1.id + 2)) n3) n4;
select * from names n1 where NOT((n1.id = 1) OR EXISTS (select * from names n2 where n2.id != 2));
select * from names n1 where NOT((id = 1) OR NOT EXISTS (select * from names n2 where n2.id != 2));
select * from names n1 where EXISTS (select n2.id from names n2 intersect select n3.id from names n3);
select * from names n1 where EXISTS (select n2.id from names n2 intersect all select n3.id from names n3);
select * from names n1 where EXISTS (select n2.id from names n2 union select n3.id from names n3);
select * from names n1 where EXISTS (select n2.id from names n2 union all select n3.id from names n3);
select * from names n1 where EXISTS (select n2.id from names n2 except select n3.id from names n3);
select * from names n1 where EXISTS (select n2.id from names n2 except all select n3.id from names n3);
select * from names n1 where NOT EXISTS (select n2.id from names n2 intersect select n3.id from names n3);
select * from names n1 where NOT EXISTS (select n2.id from names n2 intersect all select n3.id from names n3);
select * from names n1 where NOT EXISTS (select n2.id from names n2 union select n3.id from names n3);
select * from names n1 where NOT EXISTS (select n2.id from names n2 union all select n3.id from names n3);
select * from names n1 where NOT EXISTS (select n2.id from names n2 except select n3.id from names n3);
select * from names n1 where NOT EXISTS (select n2.id from names n2 except all select n3.id from names n3);
