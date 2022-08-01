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

-- Case sensitivity of double-quoted identifiers: raise error for case mismatch
select "id" from names;
select "id a" from (select 1 as "ID A")n1;

-- Returns value of `id` column for each row. Since double-quoted identifiers
-- are case sensitive, the identifier string must be capitalized to conform to
-- Octo's convention of capitalizing all identifier names internally.
select "ID" from names;

-- double quoted table alias
select id from names as "n1";
select n1.id from names as "N1";
select lastName from names group by lastName having exists (select alias1.lastName from names as "ALIAS1" group by alias1.lastName);
select 1 from names as "N1" inner join (select n2.id from names as "N2" where n2.id = 1 OR n2.id = 2) as "ALIAS2" ON (n1.id = alias2.id ) inner join names as "N3" ON (n1.id = n3.id);
select * from names as "n1 with space", names as "n1 with space";

-- double quoted table and column aliases
select * from names n1, names n2 where "N1"."FIRSTNAME" = 2;

-- double quotes column alias
select ida from (select 8 as "IDA",'ey' as firstname, '' as lastname)n1;
select * from (select id as "id space" from names) n1, (select id as "id space" from names ) n2 where "id space" = 3;

-- shorthand alias --
-- double quotes column alias
select ida from (select 8 "IDA",'ey' as firstname, '' as lastname)n1;

-- double quoted table alias
select n1.id from names "N1";
select lastName from names group by lastName having exists (select alias1.lastName from names "ALIAS1" group by alias1.lastName);
select 1 from names "N1" inner join (select n2.id from names "N2" where n2.id = 1 OR n2.id = 2) "ALIAS2" ON (n1.id = alias2.id ) inner join names "N3" ON (n1.id = n3.id);

-- mixed usage of shortform, quoted and identifier alias usage
select 1 from names as n1 inner join (select n2.id from names "N2" where n2.id = 1 OR n2.id = 2) alias2 ON (n1.id = alias2.id ) inner join names as "N3" ON (n1.id = n3.id);

-- double-quoted identifier
select * from (select "first name" from "quote names") as n1 join "quote names" as n2 on true natural join "quote names" as n3;

-- empty double-quoted identifier
SELECT id as A_id, "" AS B_id from names;
