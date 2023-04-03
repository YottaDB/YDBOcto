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

CREATE FUNCTION PARMLESSFUNC() RETURNS VARCHAR AS $$^PARMLESSFUNC;
create view v1 as select id from names;
select * from v1;
create view v2 as select lastname from names;
select * from v2;
create view v3 as VALUES(1,2,3);
select * from v3;
create view v4 as select firstname from names union all select lastname from names;
select * from v4;
create view v5 as select 1;
select * from v5;
create view v6 as select * from v5;
select * from v6;
create view v7 as select * from v1;
select * from v7;
create view v8 as select PARMLESSFUNC();
select * from v8;

discard all;
