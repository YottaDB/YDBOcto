#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCT016 : OCTO568 : Accept non-standard integer types

CREATE TABLE dummyA (id INT8 NOT NULL);
CREATE TABLE dummyB (id BIGINT NOT NULL, age INT2, favorite_number INT4, least_favorite_number INT8);

select * from names where id::int2 = id::int4;
select id::int8 from names where id::int2 = id::bigint;

select cast(id as int2),ABS(2) from names;
select cast(id as int4),(cast(ABS(2) as text) || cast(id as text)) from names;
select cast(id as int8),(cast(ABS(2) as int4) + id) from names;
select cast(id as bigint),(cast(ABS(2) as int2) + id) from names;

DROP TABLE dummyA;
DROP TABLE dummyB;

