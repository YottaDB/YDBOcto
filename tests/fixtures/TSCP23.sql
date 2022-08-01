#################################################################
#                                                              #
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.      #
# All rights reserved.                                         #
#                                                              #
#      This source code contains the intellectual property     #
#      of its copyright holder(s), and is made available       #
#      under a license.  If you do not know the terms of       #
#      the license, please stop and do not read further.       #
#                                                              #
#################################################################

-- TSCP23 : OCTO519 : Support use of double quotes for referencing SQL identifiers

select 'id' from names; -- Returns 'id' VARCHAR literals

select "id a" from (select 1 as "id a")n1;
select n1."id a" from (select 1 as "id a") as n1;
select "id a"."id a" from (select 1 as "id a") as "id a";
select "1" from (select 2 as "1")n1;
select 1 from (select 2 as "1")n1;

 -- SQL keywords can be used in double-quoted identifiers
select "SELECT" from (select 1 as "SELECT") as n1;
select "FROM" from (select 2 as "FROM") as n1;
select "ABS" from (select 3 as "ABS") as n1; -- SQL function name accepted

-- Double-quoted alias accepts non-alphanumeric characters, including space
select "!@#$%^&*()_+ " from (select 1 as "!@#$%^&*()_+ ") as "{}|<>?:";
