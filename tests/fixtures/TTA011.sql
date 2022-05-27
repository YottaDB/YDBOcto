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

select max(n1.firstname) from (select id,firstname,lastname from names n2) n1 group by n1.* order by n1.*;
select max(n1.firstname) from (select firstname,lastname from names n2) n1 group by n1.* order by n1.*;
select max(n1.firstname) from (select firstname from names n2) n1 group by n1.* order by n1.*;
SELECT 1 FROM names n1 GROUP BY n1.* ORDER BY n1.*;
SELECT 1 FROM names n1 GROUP BY n1.* HAVING n1.* is not null ORDER BY n1.*;
SELECT t1.id, MAX(t1.lastName) FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL GROUP BY t2.*, t1.id ORDER BY t2.*,t1.id;
SELECT t1.id, MAX(t1.lastName) FROM names t1 LEFT JOIN (SELECT lastName FROM names group by lastname) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL GROUP BY t2.*, t1.id ORDER BY t2.*,t1.id;
SELECT 1 FROM names n1 GROUP BY n1.* HAVING n1.* is null ORDER BY n1.*;
select count(1) from names n1 group by n1.* order by n1.*;
select distinct n1.* from names n1 order by n1.firstname,n1.lastname,n1.id;
select distinct n1.* from names n1 order by n1.id,n1.firstname;
select distinct n1.* from names n1 order by n1.id,n1.lastname;
select distinct n1.* from names n1 order by n1.firstname,n1.lastname;
