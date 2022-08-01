#################################################################
#								#
# Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- double quotes column alias
select id as "quoted" from names;
select id as "ali.co" from names;
select id as "ab12_" from names;
select id as "ab12_.asdf" from names;

-- identifier as column alias
select id as ida from names;
select ida from (select 8 as ida,'ey' as firstname, '' as lastname)n1;

-- double quoted table alias
select id from names "n1";

-- identifier as table alias
select id from names as n1;
select n1.id from names as n1;
select lastName from names group by lastName having exists (select alias1.lastName from names as alias1 group by alias1.lastName);
select * from names n1 natural join names as longeralias;
select 1 from names as n1 inner join (select n2.id from names as n2 where n2.id = 1 OR n2.id = 2) as alias2 ON (n1.id = alias2.id ) inner join names as n3 ON (n1.id = n3.id);

-- shorthand alias --
-- double quotes column alias
select id "quoted" from names;

-- identifier as column alias
select id ida from names;
select ida from (select 8 ida,'ey' as firstname, '' as lastname)n1;

-- double quoted table alias
select * from names n1 natural join names "LONGERALIAS";

-- identifier as table alias
select id from names n1;
select n1.id from names n1;
select lastName from names group by lastName having exists (select alias1.lastName from names alias1 group by alias1.lastName);
select * from names n1 natural join names longeralias;
select 1 from names n1 inner join (select n2.id from names n2 where n2.id = 1 OR n2.id = 2) alias2 ON (n1.id = alias2.id ) inner join names n3 ON (n1.id = n3.id);
