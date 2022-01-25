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

select firstname from names n1 where EXISTS (select 1 from names n2 order by count(n1.firstname));
select firstname from names n1 where count(firstname||lastname) < 1;
select firstname from names n1 where EXISTS (select 1 from names n2 order by exists(select 1 from names n2 order by count(n1.firstname)));
select firstname from names n1 where EXISTS (select 1 from names n2 order by count(n1.firstname||n1.lastname));
select firstname from names n1 where EXISTS (select 1 from names n2 order by exists(select 1 from names n2 order by count(n1.firstname||n1.lastname)));
select firstname from names n1 where EXISTS (select 1 from names n2 order by count(n1.firstname||n2.lastname));
select all 1 from names c where exists (select 1 from names n2 group by n2.id having count(c.id) > 2) GROUP BY c.id;
select 1 from names n0 group by firstname having EXISTS (select firstname from names n1 where EXISTS (select 1 from names n2 order by count(n1.firstname)));
