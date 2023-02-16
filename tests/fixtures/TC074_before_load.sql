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

create table test (id integer check(abs(id)<3));
insert into test values(1),(2);
insert into test values(3); -- ERR_CHECK_CONSTRAINT_VIOLATION
select * from test;
drop function abs(integer); -- ERR_CANNOT_DROP_FUNCTION
