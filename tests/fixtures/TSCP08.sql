#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Single asterisk at the beginning of select column list
select * from names;
select *,n1.id from names n1;
select *,n1.id,n2.id from names n1, names n2;
-- Single asterisk at different positions in select column list
select n1.id,*,n2.id from names n1, names n2;
select n1.id,n2.id,* from names n1, names n2;
-- Multiple asterisk usage
select n1.id,*,n2.id,*,n1.id from names n1, names n2;
select * from (select *,n1.id,* from names n1, names n2) n3;
select * from (select * from names n1, names n2) n3;
select * from (select * from names n1 natural join names n2) n3;
select * from (select * from names n1 right join names n2 on n2.id=n1.id) n3;
select * from (select * from names n1 left join names n2 on n2.id=n1.id) n3;
select * from (select * from names n1 inner join names n2 on n2.id=n1.id) n3;
select * from (select * from names n1 union select * from names n2) n3;
select * from (select * from names n1 union all select * from names n2) n3;
select * from (select * from names intersect select * from names) n3;
