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

select n1.column1 = n2.column1 from (select 1,'test' as column1 union select NULL,'test') n1, (select NULL as column1,'test' union select 1,'test') n2;
select n1.column1 IN (n2.column1) from (select 1,'test' as column1 union select NULL,'test') n1, (select NULL as column1,'test' union select 1,'test') n2;

select n1.* in (n2.*) from (select 1 union select NULL union select NULL) n1, (select NULL union select 'test') n2 group by n1.*,n2.* order by n1.*;

select n1.* = n2.* from (select 1 union select NULL union select NULL) n1, (select NULL union select 'test') n2 group by n1.*,n2.* order by n1.*;

select n1.column1 in (n2.column1) from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select n1.column1 = n2.column1 from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select n1.column1 > n2.column1 from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select n1.column1 < n2.column1 from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select n1.column1 <= n2.column1 from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select n1.column1 >= n2.column1 from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;

select n2.column1 >= 1 from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;

select n2.* >= 'test' from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select 'test' >= n2.* from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select 1 >= n2.* from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select n2.column1 >= n2.* from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select n2.* >= n2.column1 from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select n1.column1 >= n2.* from (select 1 as column1 union select NULL) n1, (select NULL as column1 union select 'test') n2;
select n1.* in (NULL, NULL,n2.*,NULL, NULL) from (select 1 union select NULL union select NULL) n1, (select NULL union select 'test') n2 group by n1.*,n2.* order by n1.*;

--Misc queries which ensure set_operation change doesn't break the other query types
select n2.column1 >= n2.* from (select NULL as column1) n2;
select n2.column1 >= n2.* from (select 1 as column1) n2;
select n2.column1 >= n2.* from (select 'test' as column1) n2;
select n2.* >= n2.column1 from (select 'test' as column1) n2;
