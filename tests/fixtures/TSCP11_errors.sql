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

select count(n1.id),n1.* from ((select 1 as id, 'test' as name) union (select 2 as id, 'test' as name)) as n1 group by n1.id;
select count(n1.*) from ((select 1 as id, 'test' as name) union (select 2 as id, 'test' as name)) as n1 group by count(n1.*);

-- Invalid aggregate function usage
select SUM(n1.*) from customers n1;
select MIN(n1.*) from customers n1;
select MAX(n1.*) from customers n1;
select AVG(n1.*) from customers n1;

-- Type mismatch
select n1.* = n1.firstname from names n1;
select count(n1.*) from ((select 1 as id, 'test' as name) union (select 2 as id, 'test' as name)) as n1 group by n1.* having (n1.* > 1);
select count(n1.*) from ((select 1 as id, 'test' as name) union (select 2 as id, 'test' as name)) as n1 group by n1.* having (n1.*);

-- Invalid subquery usage
select (select names.*) from names;

-- Missing FROM-clause entry
select firstname from names n1 group by names.*;
select n1.* from names n2;
select firstname from names group by n2.*;
select count(n2.*) from names;
select firstname from names order by n1.*;
select firstname from names order by count(n1.*);
select firstname from names having count(n1.*);
select firstname from names having n1.*;

-- Following query verifies that similar tablenames are not mistaken to be same
select count(DISTINCT n1.*) from names n11;

-- Invalid column correlation specification
select a,b,c from (select n1.* from names n1) as abcd(a,b,c,d);
