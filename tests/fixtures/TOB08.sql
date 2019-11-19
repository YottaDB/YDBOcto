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

-- TOB09 : OCTO343 : ORDER BY expression

select * from names order by 1+1;
select * from names order by 1*1;
select * from names order by 1+id;
select * from names order by id+1;
select id,firstname,lastname from names order by id+id;
select * from names order by id%2;
select * from names order by -id;
-- ORDER BY using multiple columns with expressions
select * from names n1 left join names n2 on n1.firstname < n2.firstname order by -n1.id,n2.id%2;
-- ORDER BY plain alias names are allowed
select 1+id*2 as a from (select id from names) order by a;
-- BUT ORDER BY expressions involving alias names are not allowed (and parse errors are issued)
select 1+id*2 as a from (select id from names) order by -a;
select 1+id*2 as a from (select id from names) order by 1+a;
