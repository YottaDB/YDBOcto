#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select count(n1.*) from names1col n1;
select count(DISTINCT n1.*) from names1col n1;
select count(n1.*) from names1col n1 group by n1.*;
select count(n1.*),n1.id,count(n1.*) from names1col n1 group by n1.*,n1.*,n1.id;
select count(n1.*),n1.id,count(n1.*) from names1col n1 group by n1.*,n1.id,n1.*;
SELECT DISTINCT COUNT(alias1.*) FROM names1col AS names1col  LEFT OUTER JOIN names1col AS alias1 ON (('Lord' >= NULL::varchar) OR NOT (('Lord' != NULL::varchar) OR NOT (names1col.id > alias1.id))) WHERE (NOT (3 = names1col.id)) GROUP BY alias1.*;
select count(ALL n1.*) from names1col n1;
select count(n1.*) from names1col n1 group by n1.* having (count(n1.*)>0);
select count(*) from (select 1 from names1col n1, names1col n11 group by n1.*, n11.*) tbl1;
select count(*) from (select 1 from names1col n11, names1col n1 group by n1.*, n11.*) tbl1;
select n1.id,n1.*,n1.id from names1col n1;
select * from (select n1.* from names1col n1) n2;
select (select count(names1col.*) from names1col) from names1col;
select n1.* from names1col n1 order by n1.*;
select id from names1col n1 where exists (select n1.*);
select * from names1col n1 where exists (select id from names1col where id=n1.id order by n1.*);
select id from names1col where exists (select names1col.*);

-- The following is valid because names1col has only one column
select (select names1col.*) from names1col;
