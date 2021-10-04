#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TUT009 : OCTO579 : Test that UPDATE correctly updates duplicate rows on table with no primary key columns

drop table if exists TUT009;
create table TUT009 (id INTEGER, firstname VARCHAR);
insert into TUT009 values (1234, 'First'), (1234, 'First'), (5678, 'Last'), (5678, NULL), (NULL, NULL), (NULL, NULL);
update TUT009 set id = id * 2, firstname = firstname || '#' where id = 1234 OR firstname is NULL;
select * from TUT009;

