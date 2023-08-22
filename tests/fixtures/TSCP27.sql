#################################################################
#                                                              #
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.      #
# All rights reserved.                                         #
#                                                              #
#      This source code contains the intellectual property     #
#      of its copyright holder(s), and is made available       #
#      under a license.  If you do not know the terms of       #
#      the license, please stop and do not read further.       #
#                                                              #
#################################################################

select -((select 1 from names limit 1)+(select 2 from names limit 1))+3;
select -((select 1 from names limit 1)+(select 2 from names limit 1))+3;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/350#note_339756869
select ((select 1) + 2);
select +((select 1) + 2);
select -((select 1) + 2);
select ((select 1) + (select 2));
select +((select 1) + (select 2));
select -((select 1) + (select 2));
select -((select 1 from names limit 1) + (select 2 from names limit 1)) + 3;
select (select 1 from names limit 1) + (select 2 from names limit 1);
select +(select 1 from names limit 1) + (select 2 from names limit 1);
select -(select 1 from names limit 1) + (select 2 from names limit 1);
select (2 + (select 1) + 2);
select +(2 + (select 1) + 2);
select -(2 + (select 1) + 2);
