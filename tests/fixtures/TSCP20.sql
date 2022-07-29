#################################################################
#                                                              #
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.      #
# All rights reserved.                                         #
#                                                              #
#      This source code contains the intellectual property     #
#      of its copyright holder(s), and is made available       #
#      under a license.  If you do not know the terms of       #
#      the license, please stop and do not read further.       #
#                                                              #
#################################################################

-- TSCP20 : OCTO500 : Support SELECT without FROM when WHERE is present

select 1 where 1 = 1;
select 'ok' where (select col1 from (select 1 as col1, NULL as col2) as n1) is null;
select 'ok' where (select col1 from (select NULL as col1, NULL as col2) as n1) is null;
select 'ok' where (select col1 from (select 1 as col1, NULL as col2) as n1) is not null;
select 'ok' where (select col1 from (select NULL as col1, NULL as col2) as n1) is not null;
select 'abcd' group by 1;
select 1 having 'abcd' = 'efgh';
select id from names n1 where firstname in (select n1.firstname having n1.firstname != 'Zero');
select id from names n1 where firstname in (select n1.firstname group by n1.id having n1.firstname = 'Zero');

-- OCTO879 : Query with only SELECT and ORDER BY should not issue a syntax error
select 1 order by 1;
select 'ok' as alias order by alias;
select 'ok' order by 1;
select id from names n1 where firstname in (select n1.firstname group by n1.id having n1.firstname = 'Zero' order by 1);
select 1 as id where 1 != 2 group by id having 1 != 2 order by 2::integer;
select 1 as id where 1 != 2 group by id having 1 != 2 order by id;
select 1 as id where 1 != 2 group by id having 1 != 2 order by 1;
