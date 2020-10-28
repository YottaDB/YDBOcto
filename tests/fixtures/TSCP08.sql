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

-- tablename.* usage
select n1.* from names n1;
select n1.*,n1.id from names n1;
select n1.id,n1.* from names n1;
select n1.id,n1.*,n1.id from names n1;
select n2.* from names n1, names n2;
select n1.id,n1.*,n1.id,n1.* from names n1;
select n1.id,n2.*,n2.id,n2.* from names n1, names n2;
select * from (select n1.* from names n1) n2;

-- ORDER BY tablename.* usage
select n1.* from names n1 order by n1.*;
select n1.firstname, n1.lastname from names n1 order by n1.*;
select firstname, lastname from names n1 order by n1.*;
select n1.* from (select id,firstname from names)n1 order by n1.*;
select n1.* from (select * from names)n1 order by n1.*;
select * from (values (1,5), (2,4), (3,3), (4,2),(5,1)) as tbl1 order by tbl1.*;
select * from (values (1,5), (2,4), (3,3), (4,2),(5,1)) as tbl1 order by tbl1.* desc;
select * from (values (1,5,1), (2,4,2), (3,3,1), (4,2,2),(5,1,1)) as tbl1 order by tbl1.*;
select * from (values (1,5,1), (2,4,2), (3,3,1), (4,2,2),(5,1,1)) as tbl1 order by tbl1.* desc;
select * from (values (1,4,1), (2,5,2), (3,3,1), (4,2,2),(5,1,1)) as tbl1 order by tbl1.*;
select * from (values (1,4,1), (2,5,2), (3,3,1), (4,2,2),(5,1,1)) as tbl1 order by tbl1.* desc;
select * from (values (1,1,1), (1,1,2), (1,2,2), (2,1,1), (1,2,1), (2,1,2), (2,2,2), (2,2,1)) as tbl1 order by tbl1.*;
select * from (values (1,1,1), (1,1,2), (1,2,2), (2,1,1), (1,2,1), (2,1,2), (2,2,2), (2,2,1)) as tbl1 order by tbl1.* desc;
select * from (select firstname,lastname,id from names)n1 order by n1.*,n1.*;
select * from (select firstname,lastname,id from names)n1 order by n1.id,n1.*;
select * from (select firstname,lastname,id from names)n1 order by n1.*,n1.id;


-- More easily understandable natural join case query referred from - https://www.w3resource.com/sql/joins/natural-join.php
-- Asterisk in select column list
select * from(
(select 1 as it_id,'Chex Mix' as it_name, 'Pcs' as it_unit, 16 as cmp_id) union
(select 6 as it_id,'Cheezit' as it_name,'Pcs' as it_unit, 15 as cmp_id) union
(select 2 as it_id,'BNBiscuit' as it_name,'Pcs' as it_unit, 15 as cmp_id) union
(select 3 as it_id,'MightyMunch' as it_name,'Pcs' as it_unit, 17 as cmp_id) union
(select 4 as it_id,'Potrice' as it_name,'Pcs' as it_unit, 15 as cmp_id) union
(select 5 as it_id,'Jafacake' as it_name,'Pcs' as it_unit, 18 as cmp_id)
)n1 natural join(
(select 18 as cmp_id,'orderall' as cmp_name,'boston' as cmp_city) union
(select 15 as cmp_id,'jackhillltd' as cmp_name,'london' as cmp_city) union
(select 16 as cmp_id,'Akasfood' as cmp_name,'delhi' as cmp_city) union
(select 17 as cmp_id,'foodies' as cmp_name,'london' as cmp_city) union
(select 19 as cmp_id,'sipnbite' as cmp_name,'newyork' as cmp_city)
) n2;

-- TABLENAME.ASTERISK in select column list
select n2.* from (
(select 1 as it_id,'Chex Mix' as it_name, 'Pcs' as it_unit, 16 as cmp_id) union
(select 6 as it_id,'Cheezit' as it_name,'Pcs' as it_unit, 15 as cmp_id) union
(select 2 as it_id,'BNBiscuit' as it_name,'Pcs' as it_unit, 15 as cmp_id) union
(select 3 as it_id,'MightyMunch' as it_name,'Pcs' as it_unit, 17 as cmp_id) union
(select 4 as it_id,'Potrice' as it_name,'Pcs' as it_unit, 15 as cmp_id) union
(select 5 as it_id,'Jafacake' as it_name,'Pcs' as it_unit, 18 as cmp_id)
)n1 natural join(
(select 18 as cmp_id,'orderall' as cmp_name,'boston' as cmp_city) union
(select 15 as cmp_id,'jackhillltd' as cmp_name,'london' as cmp_city) union
(select 16 as cmp_id,'Akasfood' as cmp_name,'delhi' as cmp_city) union
(select 17 as cmp_id,'foodies' as cmp_name,'london' as cmp_city) union
(select 19 as cmp_id,'sipnbite' as cmp_name,'newyork' as cmp_city)
) n2;

-- TABLENAME.ASTERISK in order by
