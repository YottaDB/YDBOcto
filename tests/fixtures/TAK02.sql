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

-- single quotes column alias
select id as 'quoted' from names;
select id as 'ali.co' from names;
select id as 'ab12_' from names;
select id as 'ab12_.asdf' from names;
select ida from (select 8 as 'IDA','ey' as firstname, '' as lastname)n1;

-- single quoted table alias
select id from names as 'N1';
select n1.id from names as 'N1';
select lastName from names group by lastName having exists (select alias1.lastName from names as 'ALIAS1' group by alias1.lastName);
select * from names n1 natural join names as 'longeralias';
select 1 from names as 'N1' inner join (select n2.id from names as 'N2' where n2.id = 1 OR n2.id = 2) as 'ALIAS2' ON (n1.id = alias2.id ) inner join names as 'N3' ON (n1.id = n3.id);

-- shorthand alias --
-- single quotes column alias
select id 'quoted' from names;
select ida from (select 8 'ida','ey' as firstname, '' as lastname)n1;

-- single quoted table alias
select id from names 'N1';
select n1.id from names 'N1';
select lastName from names group by lastName having exists (select alias1.lastName from names 'ALIAS1' group by alias1.lastName);
select * from names n1 natural join names 'longeralias';
select 1 from names 'N1' inner join (select n2.id from names 'N2' where n2.id = 1 OR n2.id = 2) 'ALIAS2' ON (n1.id = alias2.id ) inner join names 'N3' ON (n1.id = n3.id);

-- mixed usage of shortform, quoted and identifier alias usage
select 1 from names as 'N1' inner join (select n2.id from names "N2" where n2.id = 1 OR n2.id = 2) alias2 ON (n1.id = alias2.id ) inner join names as n3 ON (n1.id = n3.id);

-- NULL table alias usage
select id from (select 1 as id);
select ida from (select 1 as "ida");
select ida from (select 1 as 'IDA');
