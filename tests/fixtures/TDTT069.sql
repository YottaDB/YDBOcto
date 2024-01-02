#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

create table testfileman(id int primary key, dob date(fileman)) global "^testfileman(keys(""id""))" readonly;
-- original query from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1404#note_1672408831 is
-- `select start_date,stop_date from order1 where start_date = timestamp'12-31-2015 00:00:00';`
select * from testfileman where dob = timestamp '06-17-2023 00:00:00';
-- original query from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1404#note_1672411664 is
-- `select start_date,stop_date from order1 where start_date between date'01-01-2015' and date'01-01-2016';`
select * from testfileman where dob between date'01-01-2023' and date'01-01-2024';

