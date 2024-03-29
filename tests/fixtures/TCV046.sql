#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Column name is operation name
create view v1 as select EXISTS (select 1); -- EXISTS
select * from v1;

create view v2 as select abs(1); -- abs
select * from v2;

create view v3 as select 1::varchar; -- varchar
select * from v3;

create view v4 as select 1::integer; -- int4
select * from v4;

create view v5 as select id::integer from names; -- id
select * from v5;

create view v6 as select 1.244::NUMERIC(2,1); -- NUMERIC
select * from v6;

-- column name ???
create view v7 as select +id from names;
select * from v7;

create view v8 as select -id from names;
select * from v8;

create view v9 as select +1;
select * from v9;

create view v10 as select +(select id from names limit 1);
select * from v10;

create view v11 as select NOT EXISTS(select id from names);
select * from v11;

create view v12 as select NOT 1::boolean;
select * from v12;

create view v13 as select 1 + id from names;
select * from v13;

create view v14 as select count(1) + id from names group by id;
select * from v14;

create view v15 as select abs(1)+1;
select * from v15;

create view v16 as select 1::varchar || 'test';
select * from v16;

create view v17 as select id+id from names;
select * from v17;

