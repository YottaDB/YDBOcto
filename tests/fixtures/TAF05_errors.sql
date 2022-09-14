#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Test of ERR_AGGREGATE_FUNCTION_UPDATE error
update names set id=max(id);
update names set id=max(2) < min(1));

-- Test of ERR_AGGREGATE_FUNCTION_WHERE error in DELETE
delete from names where sum(id) = 2;
delete from names where count(*) < min(2);

-- Test of ERR_AGGREGATE_FUNCTION_WHERE error in UPDATE
update names set id = 1 where count(id) = 3;
update names set id = 1 where avg(1) = 3;
update names set id = 1 where id = (select max(id));

-- Test of ERR_AGGREGATE_FUNCTION_UPDATE error
update names set id = count(1);
update names set id = count(id);
update names set id = (select max(id));

