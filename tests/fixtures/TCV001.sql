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

create view TCV001_1v as select lastname from names;
select * from TCV001_1v;
select TCV001_1v.* from TCV001_1v;
select lastname from TCV001_1v;
select TCV001_1v.lastname from TCV001_1v;
drop view TCV001_1v;

  -- Primary key usage
create view TCV001_1v1 as select id from names;
select * from TCV001_1v1;
drop view TCV001_1v1;

create view TCV001_1v1 as select id,lastname from names;
select * from TCV001_1v1;
drop view TCV001_1v1;
