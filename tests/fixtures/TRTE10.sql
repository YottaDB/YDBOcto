#################################################################
#                                                               #
# Copyright (c) 2025 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

-- TRTE10 : OCTO873: Verify M plan has regfreeIfAny() invocation for LIKE/SIMILAR/~ operators and none otherwise

-- -------------------------------------------------------------------------------------
-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1647#note_2631147916
-- -------------------------------------------------------------------------------------

-- Test of LP_SELECT_QUERY
select count(*) from names where firstname like 'test%';
select count(*) from names where firstname similar to 'test%';
select count(*) from names where firstname ~ 'test%';

-- Test of LP_TABLE_VALUE
values (1,(select 'a' like 'test%'));
values (1,(select 'a' similar to 'test%'));
values (1,(select 'a' ~ 'test_'));

-- Test of LP_SET_OPERATION
(select count(*) from names where firstname like 'test%') union (select count(*) from names);
(select count(*) from names) intersect (select count(*) from names where firstname similar to 'test_');
(select count(*) from names where firstname ~ 'test%') except (select count(*) from names);

-- Test of LP_VIEW
create view v1 as select * from names;
select count(*) from v1 where firstname like 'test%';
select count(*) from v1 where firstname similar to 'test%';
select count(*) from v1 where firstname ~ 'test%';

-- Test of LP_INSERT_INTO
insert into names values (6, (select 'abcd' where 'a' like 'test%'), 'efgh');
insert into names values (7, (select 'abcd' where 'a' similar to 'test%'), 'efgh');
insert into names values (8, (select 'abcd' where 'a' ~ 'test%'), 'efgh');

-- Test of LP_DELETE_FROM
delete from names where firstname like 'test%';
delete from names where firstname similar to 'test%';
delete from names where firstname ~ 'test%';

-- Test of LP_UPDATE
update names set firstname = 'a' || firstname where firstname like 'test%';
update names set firstname = 'a' || firstname where firstname similar to 'test%';
update names set firstname = 'a' || firstname where firstname ~ 'test%';

-- Test that NO regfreeIfAny() invocation is seen in M plan if there are no LIKE/SIMILAR/~ operators
-- One query below for each of the LP_ types above
select count(*) from names where firstname != 'test%';
values (1,(select 'a' != 'test_'));
(select count(*) from names where firstname != 'test%') except (select count(*) from names where firstname != 'test_');
select count(*) from v1 where firstname != 'test%';
insert into names values (9, (select 'abcd' where 'a' != 'test%'), 'efgh');
delete from names where firstname != 'test%';
update names set firstname = 'a' || firstname where firstname != 'test%';

-- Test that multiple regex usages within one query still contribute to only ONE regfreeIfAny invocation in M plan
-- One query below for each of the LP_ types above
-- Mix in CASE-INSENSITIVE tests for regex too (LP_BOOLEAN_REGEX_INSENSITIVE_*)
select count(*) from names where (firstname like 'test%') or (firstname not similar to 'test%');
values ((select 'b' like 'test_'),(select 'a' like 'test%'));
(select count(*) from names where firstname ilike 'test%') union (select count(*) from names where lastname similar to 'test%');
select count(*) from v1 where firstname like 'test%' or firstname similar to 'test_';
insert into names values (10, (select 'abcd' where 'a' ~ 'test%'), (select 'efgh' where 'a' similar to 'test%'));
delete from names where firstname like 'test%' or firstname similar to 'test_';
update names set firstname = 'a' || firstname where firstname ~* 'test%' or firstname not similar to 'test%';

