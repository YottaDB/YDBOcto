#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

create function samevalue(integer) returns integer as $$samevalue^functions;
create function samevalue(varchar) returns varchar as $$samevalue^functions;
create view v1 as select samevalue(1);
create view v2 as select samevalue(id) from names;
create view v3 as select samevalue(firstname) from names;
create view v4 as select samevalue(firstname) from names having samevalue(id)<4;
select * from v1;
select * from v2;
select * from v3;
select * from v4;
