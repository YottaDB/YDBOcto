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

-- Queries from issue description
select 1 from names n1 having exists(select 1 from names n2 group by n1.firstname);
select 1 from names n1 having exists(select 1 from names n2 group by n1.firstname||'test');

-- Following queries validate #874 in the context of GROUP BY column numbers
select 1 from names n1 having exists(select n1.firstname from names n2 group by 1);
select 1 from names n1 GROUP BY n1.firstname||'test' having exists(select n1.firstname||'test' from names n2 group by 1);
select 1 from names n1 having exists(select n1.firstname||'test' from names n2 group by 1);

-- Misc
select 1 from names n1 having exists(select 1 from names n2 group by 'test'||n2.firstname,n1.firstname||('test'||n2.firstname));
select 1 from names n1 GROUP BY n1.firstname||'test' having exists(select 1 from names n2 group by n1.firstname||n2.firstname);
select 1 from names n1 GROUP BY n1.firstname||'test' having exists(select 1 from names n2 group by n1.firstname||'test');
select 1 = (select count(n1.firstname || 'abcd') from names n2 group by n1.firstname || 'abcd') from names n1 group by n1.firstname || 'abcd';
select count(n1.firstname) from names n1 order by exists(select count(n1.firstname) from names n2 group by n1.firstname);
select 1 from names n1 order by exists(select count(n1.firstname) from names n2 group by n1.firstname);
