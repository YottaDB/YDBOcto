#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

create table test (id integer primary key, dob date(zut)) global "^test(keys(""id""))";
select * from test; -- 1|1088-10-03
select date_to_fileman(dob) from test; -- ERR_INVALID_DATE_TIME_VALUE
