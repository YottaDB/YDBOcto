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

drop view if exists TCV038_1v1;
create view TCV038_1v1 as select NULL;
select * from TCV038_1v1;

drop view if exists TCV038_1v2;
create view TCV038_1v2 as select array(select NULL from names);
select * from TCV038_1v2;

drop view if exists TCV038_1v3;
create view TCV038_1v3 as select array(select NULL);
select * from TCV038_1v3;

drop view if exists TCV038_1v3;
drop view if exists TCV038_1v2;
drop view if exists TCV038_1v1;
