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

select 1 order by 2;
-- Following query doesn't generate an error because of #881. It should after the issue is implemented.
select 1 as id where 1 != 2 group by id having 1 != 2 order by 'abcd'::integer;
select 1 as id where 1 != 2 group by id having 1 != 2 order by 2;
select 1 as id where 1 != 2 group by id having 1 != 2 order by 'abcd';
