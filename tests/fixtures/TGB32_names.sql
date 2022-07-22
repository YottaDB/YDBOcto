#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- #775
SELECT firstname,COUNT(firstname) FROM (select * from names n1) n3 GROUP BY firstname;

-- #819
select n1.lastname,count(n1.firstname) from names n1 group by n1.lastname HAVING 'Zero' in (select n2.firstname from names n2 where 1 = count(n1.firstname));
select 1 from names n1 having exists (select 1 from names n2 where count(n1.firstname)=1);
select COUNT(n1.firstname) from names n1 ORDER BY EXISTS (select count(n1.firstname));
select EXISTS (select count(n1.firstname)) from names n1;
select (select count(n1.firstname)) from names n1;
select 1 from names n1 order by EXISTS (select count(n1.firstname));
select 1 from names n1 HAVING EXISTS (select count(n1.firstname));

-- #820
select (select firstname) from names group by firstname;

-- #870
SELECT t1.id FROM names t1 GROUP BY t1.id HAVING 0 = t1.id;
SELECT t1.id FROM names t1 GROUP BY t1.id HAVING 0 = (SELECT t1.id);
SELECT t1.id FROM names t1 GROUP BY t1.id HAVING 0 IN (SELECT t1.id);

-- Misc
select 1 from (VALUES('te','st')) n1 group by n1.* having TRUE in (select n1.* = n2.* from (select 'tes','t')n2) ;
select 1 from (VALUES('te','st')) n1 group by n1.* having FALSE in (select n1.* = n2.* from (select 'tes','t')n2) ;
