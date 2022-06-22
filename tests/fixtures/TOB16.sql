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

select * from names n1 where (n1.firstname in (select n2.firstname from names n2 order by n1.id, n2.firstname));
select * from names n1 where (n1.firstname in (select n2.firstname from names n2 order by n2.firstname, n1.id));
select * from names n1 where (n1.firstname in (select n2.firstname from names n2 order by n2.firstname,n1.id, n2.firstname));
-- Following query validates query execution when both order by columns are removed
select * from names n1 where (n1.firstname in (select n2.firstname from names n2 order by n1.id, n1.id));

select * from names n1 where (n1.firstname in (select n2.firstname from names n2 order by n2.firstname));
select * from names n1 where (n1.firstname in (select n2.firstname from names n2 order by n1.*, n2.firstname));
select * from names n1 where (n1.firstname in (select n2.firstname from names n2 order by n1.id));
select * from names n1 where (n1.firstname in (select n2.firstname from names n2 order by n1.id,n1.id,n1.id,n1.id));
select (select firstname from names n2 order by n1.firstname desc, n2.firstname desc limit 1) from names n1;


-- Select distinct cases
select distinct n2.firstname from names n1, names n2;
select distinct n2.firstname from names n1, names n2 order by n2.firstname;
select distinct n2.* from names n1, names n2;
select distinct n2.* from names n1, names n2 order by n2.firstname;


