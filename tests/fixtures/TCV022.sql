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

create view TCV022_1v1 as select 1;
select * from TCV022_1v1;
create view TCV022_1v2 as select * from names;
select * from TCV022_1v2;
create view TCV022_1v3 as select 1 from TCV022_1v1,TCV022_1v2;
select * from TCV022_1v3;
