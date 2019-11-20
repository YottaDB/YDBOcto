#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select * from (select * from names as n2) as n1;
select * from (select * from names as n2);
select * from (select * from names) as n1;
select * from (select * from names);
select * from (select firstname from names) as n1;
select * from (select firstname from names);
select * from (select invalid from names) as n1;
select * from (select invalid from names);
select * from (select n1.firstname from names) as n1;
select * from (select n1.invalid from names) as n1;
select * from (select n2.firstname from names as n2) as n1;
select * from (select n2.firstname from names as n2);
select * from (select n2.invalid from names as n2) as n1;
select * from (select n2.invalid from names as n2);
select firstname from (select * from names as n2) as n1;
select firstname from (select * from names as n2);
select firstname from (select * from names) as n1;
select firstname from (select * from names);
select firstname from (select firstname from names) as n1;
select firstname from (select firstname from names);
select firstname from (select invalid from names) as n1;
select firstname from (select invalid from names);
select firstname from (select lastname from names as n2) as n1;
select firstname from (select lastname from names);
select firstname from (select n1.firstname from names) as n1;
select firstname from (select n1.invalid from names) as n1;
select firstname from (select n2.firstname from names as n2);
select firstname from (select n2.invalid from names as n2);
select invalid from (select * from names as n2) as n1;
select invalid from (select * from names as n2);
select invalid from (select * from names) as n1;
select invalid from (select * from names);
select invalid from (select firstname from names) as n1;
select invalid from (select firstname from names);
select invalid from (select invalid from names as n2) as n1;
select invalid from (select invalid from names) as n1;
select invalid from (select invalid from names);
select invalid from (select n1.firstname from names) as n1;
select invalid from (select n1.invalid from names) as n1;
select invalid from (select n2.firstname from names as n2);
select invalid from (select n2.invalid from names as n2);
select invalid as invalid1 from (select invalid from names);
select invalid as invalid from names;
select invalid1 as invalid from names;
select invalid as invalid1 from names;
select invalid1 from (select invalid2 from names) as n1;
select invalid1 from (select invalid2 from names);
select invalid1 from (select n1.invalid2 from names) as n1;
select invalid1 from (select n2.invalid2 from names as n2);
select invalid1 from (select invalid as invalid1 from names);
select n1.firstname from (select * from names as n2) as n1;
select n1.firstname from (select * from names) as n1;
select n1.firstname from (select n1.firstname from names) as n1;
select n1.firstname from (select n1.invalid from names) as n1;
select n1.firstname from (select n2.firstname from names as n2) as n1;
select n1.firstname from (select n2.invalid from names as n2) as n1;
select n1.invalid from (select * from names as n2) as n1;
select n1.invalid from (select * from names) as n1;
select n1.invalid from (select n1.firstname from names) as n1;
select n1.invalid from (select n1.invalid from names) as n1;
select n1.invalid from (select n2.firstname from names as n2) as n1;
select n1.invalid from (select n2.invalid from names as n2) as n1;
select n1.invalid1 from (select n1.invalid2 from names) as n1;
select n1.invalid1 from (select n2.invalid2 from names as n2) as n1;
select n2.firstname from (select * from names as n2);
select n2.firstname from (select n2.firstname from names as n2);
select n2.firstname from (select n2.invalid from names as n2);
select n2.invalid from (select * from names as n2);
select n2.invalid from (select n2.firstname from names as n2);
select n2.invalid from (select n2.invalid from names as n2);
select n2.invalid1 from (select n2.invalid2 from names as n2);
select NULL as a from names order by a;
