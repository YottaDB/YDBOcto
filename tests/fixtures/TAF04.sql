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

select firstname from names n1 where EXISTS (select 1 from names n2 order by count(n2.firstname));
select firstname from names n1 where EXISTS (select 1 from names n2 order by count(n2.firstname||n2.lastname));
select firstname from names n1 where EXISTS (select 1 from names n2 order by exists(select 1 from names n3 order by count(n3.firstname||n3.lastname)));
select firstname from names n1 where EXISTS (select 1 from names n2 order by count(n1.firstname||n2.lastname));
-- The following query demonstrate that aggregate column usage where the outer query to which the column belongs to isn't processing a WHERE clause is valid
select 1 from names n0 group by firstname having EXISTS (select firstname from names n1 where EXISTS (select 1 from names n2 order by count(n0.firstname)));
select 1 from names n0 group by firstname having EXISTS (select firstname from names n1 where EXISTS (select 1 from names n2 order by count(n2.firstname)));

