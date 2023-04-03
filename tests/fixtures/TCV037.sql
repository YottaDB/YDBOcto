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

create view TCV037_1v1 as select * from names;
create view TCV037_1v2 as select * from TCV037_1v1;
-- Following query should generate only three plans
select * from TCV037_1v1 inner join TCV037_1v2 on (1=1) cross join TCV037_1v1 n1;
