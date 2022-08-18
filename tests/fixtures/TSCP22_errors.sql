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

select * from (select * from names) n1 where n1.* in (select * from names);
select * from names n1 where n1.id in (select * from names);
select * from names n1 where n1.firstname in (select * from names);
select * from names n1 where n1.id in (select n2.* from names n2);
select * from names n1 order by n1.firstname in (select * from names);
select 1 from names n1 having n1.firstname in (select * from names);
select * from names n1 order by n1.firstname in ((select id, firstname from names) UNION (select id, firstname from names));
select * from names n1 order by n1.id in (VALUES('test','firstname'));
select * from names n1 order by n1.firstname in (((select 1, 'test')UNION(select 2, 'test')) UNION (select id, firstname from names));
select 'test' NOT IN (NULL,(select n1.id,n1.id)) from names n1;
select * from names n1 where (select * from names) in (n1.id);
select * from names n1 where (select id from names) in (n1.id);
select * from names n1 where (select * from names limit 1) in (n1.id);

-- Create function related test case
CREATE FUNCTION SAMEVALUE(INTEGER) RETURNS INTEGER AS $$samevalue^functions;
select samevalue((select id,id)) from names;

-- Join Statement related test case
-- -- Below query should issue `ERR_SUBQUERY_ONE_COLUMN` error
select 1 from names left join names n2 on (select names.firstname,n2.firstname);
-- -- Below query is to ensure that `ERR_TYPE_NOT_COMPATIBLE` error issue logic has not been modified
-- -- This query is expected to issue `ERR_TYPE_NOT_COMPATIBLE` error
select 1 from names left join names n2 on (select names.firstname);

-- value_STATEMENT-> CALCULATED_VALUE related test case
select count((select names.firstname,names.firstname)) from names;

-- value_STATEMENT -> COERCE_TYPE related test case
select (select names.firstname,names.firstname)::VARCHAR from names;

-- table_value_STATEMENT
select (VALUES(1,2));
select (VALUES((select 1,2),2));
select (VALUES((select 1,2)));
