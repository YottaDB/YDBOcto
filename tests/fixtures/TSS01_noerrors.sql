#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- All queries in this query file are valid queries that do not issue any error.

-- TSS01 : OCTO181 : Data types are not correctly propagated from nested SELECTs; Misc other queries

-- The below queries test that data types are correctly propagated from nested SELECTs
select * from names where lastname != firstname;
select * from (select * from names) n1 where lastname != firstname;
-- The below query was found at https://gitlab.com/YottaDB/DBMS/YDBOcto/issues/181#note_210480253
-- Although it was fixed by some prior commit, it is included here because it was recorded in #181
select col1 from (select id as col1 from names) n2;
select col1 from (select id::numeric as col1 from names) n2;
select (firstname = 'd' AND id = 0) from names;
select (0 = 1) and (id = 1) from names;
select (firstname = 'd' AND id::boolean) from names;
select (1::boolean AND id::boolean) from names;
select id::boolean and 0 = 1 from names;
select id::boolean and (0 = 1) from names;
select (0 = 1) and id::boolean from names;
select id::boolean and id::boolean from names;
-- The below query tests NULL usage in sub-queries. Is not directly related to #181 but was found to fail
-- while analyzing various issues related to #181.
select * from (select NULL from names) n1;
select (n1.col1::varchar = '') from (select NULL as col1 from names) n1;
select (n1.col1::varchar = NULL) from (select NULL as col1 from names) n1;
select (n1.col1 is NULL) from (select NULL as col1 from names) n1;
select (n1.col1::varchar = 0::varchar) from (select NULL as col1 from names) n1;
select (n1.col1::varchar = 'abcd') from (select NULL as col1 from names) n1;
select (n1.col1 is NULL) from (select NULL as col1 from names) n1;
select (n1.col1 is NOT NULL) from (select NULL as col1 from names) n1;
select * from names where id IS NULL;
select * from names where id IS NOT NULL;
select * from names where firstname IS NULL;
select * from names where firstname IS NOT NULL;
