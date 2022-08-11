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

-- Below query is to validate new logic to discard outer query columns during logical plan generation works fine.
select 1 from names n1 group by firstname having exists(select n1.firstname from names n2 group by 1,n2.firstname);
select 1 from names n1 group by firstname having exists(select n1.firstname from names n2 group by 1,n2.firstname,1);
select 1 from names n1 group by firstname having exists(select n1.firstname from names n2 group by 1,1,1);
select 1 from names n1 group by firstname having exists(select n1.firstname from names n2 group by 1,1,1 having 1=1);

-- Below query ensures when outer query doesn't have a grouping element its column usage in inner query GROUP BY is considered valid
select 1 from names n1 order by exists(select 1 from names n2 group by n1.firstname);

-- Below query ensures that non deletion of outer query column in inner query GROUP BY doesn't effect its next item in the list
select 1 from names n1 group by n1.firstname having exists(select n2.lastname from names n2 group by n1.firstname,n2.lastname);

-- Below queries tests outer query column reference in inner query GROUP BY clause where the inner query is part of WHERE clause
select 1 from names n1 where exists(select 1 from names n2 group by n1.firstname) group by firstname;
select 1 from names n1 where exists(select 1 from names n2 group by n1.firstname) having 1=1;

-- Misc
select 1 from names n1 group by n1.firstname having exists(select 1 from names n2 group by 'test'||n2.firstname,n1.firstname||('test'||n2.firstname));
select 1 from names n1 GROUP BY n1.firstname having exists(select 1 from names n2 group by n1.firstname||n2.firstname);
select 1 from names n1 GROUP BY n1.firstname having exists(select 1 from names n2 group by n1.firstname||'test');
select 1 = (select count(n1.firstname) from names n2 group by n1.firstname) from names n1 group by n1.firstname;
select exists(select 1 from names n2 group by n1.firstname) from names n1 group by firstname;
