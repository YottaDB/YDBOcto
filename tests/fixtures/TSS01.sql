#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- The below queries test that data types are correctly propagated from nested SELECTs
select * from names where id != firstname;
select * from (select * from names) where id != firstname;
select * from names where lastname != firstname;
select * from (select * from names) where lastname != firstname;
-- The below query was found at https://gitlab.com/YottaDB/DBMS/YDBOcto/issues/181#note_210480253
-- Although it was fixed by some prior commit, it is included here because it was recorded in #181
select col1 from (select id as col1 from names) n2;
select col1 from (select id::numeric as col1 from names) n2;
-- The below queries test that boolean expressions involving just "column" operands work (equivalent to "column != 0")
-- And issue errors if needed.
select (firstname = 'd' AND id) from names;
select (1 AND id) from names;
select (firstname = 'd' AND id = 0) from names;
select (firstname AND id) from names;
select id and 0 = 1 from names;
select id and (0 = 1) from names;
select (0 = 1) and (id = 1) from names;
select (0 = 1) and id from names;
select id and id from names;
-- The below query tests NULL usage in sub-queries. Is not directly related to #181 but was found to fail
-- while analyzing various issues related to #181.
select * from (select NULL from names);
select (col1 = '') from (select NULL as col1 from names);
select (col1 = NULL) from (select NULL as col1 from names);
select (col1 is NULL) from (select NULL as col1 from names);
select (col1 = 0) from (select NULL as col1 from names);
select (col1 = 'abcd') from (select NULL as col1 from names);
select (col1 is NULL) from (select NULL as col1 from names);
select (col1 is NOT NULL) from (select NULL as col1 from names);
select * from names where id IS NULL;
select * from names where id IS NOT NULL;
select * from names where firstname IS NULL;
select * from names where firstname IS NOT NULL;
