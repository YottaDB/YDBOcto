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

select id as id1 from names group by id1;
select firstname || 'x' as xyz, lastname || 'x' as lastname from names group by xyz, lastname || 'x';
select 'Zero' != 'Zero' as firstname,firstname from names group by firstname;
select 'Zero' != 'Zero' as firstname from names group by firstname;

-- Following query shows the priority of alias assignment in GROUP BY. Refer to simlar query in other fixture of this subtest
-- to see a counter example.
select n2.ln1 from names , (select id as ln1 from names)n2 group by ln1;

-- Expression in GROUP BY using alias
select firstname || 'test' as conct from names group by conct;
select firstname || 'test', firstname as conct from names group by conct;

-- Ambiguity rule usecases
select 'test'||firstname as firstname, 'last' || firstname as firstname from names group by firstname;
select max(lastname) as firstname, firstname as firstname, max(id) as firstname from names group by firstname;
select 'Zero' != 'Zero' as firstname, 1 = 1 as firstname, firstname from names group by firstname;


-- Misc
select firstname from (select lastname as ln1,firstname from names)n1 order by exists (select 1 from names n2 group by n1.ln1);
select firstname from (select lastname as ln1,firstname from names)n1 order by exists (select 1 from names n2 group by ln1);
select firstname from (select lastname as ln1,firstname from names)n1 order by exists (select firstname ||'test' from (select firstname as ln1 from names) n2 group by ln1);
select firstname from (select lastname as ln1,firstname from names)n1 order by exists (select ln1 from (select firstname from names) n2 group by ln1);
