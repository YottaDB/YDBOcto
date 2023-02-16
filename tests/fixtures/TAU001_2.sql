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

select * from test; -- Table created in TAU001_1.sql exists
		    -- It has ABS usage for column ID's check constraint
drop function abs(integer); -- should result in ERR_CANNOT_DROP_FUNCTION
select * from test;
insert into test values(3); -- ERR_CHECK_CONSTRAINT_VIOLATION
