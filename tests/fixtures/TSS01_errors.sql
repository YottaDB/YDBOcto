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

-- All queries in this query file are invalid queries and generate an error.

-- TSS01 : OCTO181 : Data types are not correctly propagated from nested SELECTs; Misc other queries

-- The below queries test that data types are correctly propagated from nested SELECTs
select * from names where id != firstname;
select * from (select * from names) n1 where id != firstname;

-- The below queries test that boolean expressions involving just "column" operands work (equivalent to "column != 0")
-- And issue errors if needed.
select (firstname = 'd' AND id) from names;
select (1 AND id) from names;
select (firstname AND id) from names;
select id and 0 = 1 from names;
select id and (0 = 1) from names;
select (0 = 1) and id from names;
select id and id from names;
select (firstname::boolean AND id::boolean) from names;

-- The below query issues an error in Postgres but works fine in Octo hence is not included in TSS01_noerrors.sql
select (n1.col1::integer = 0) from (select NULL as col1 from names) n1;
