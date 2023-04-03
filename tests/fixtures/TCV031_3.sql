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

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1369101616
create view v as select n1.id, n1.firstname, n1.lastname from names n1 inner join names n2 on n1.id = n2.id;
create view v1 as select * from v order by firstname;
select * from octoonerowtable, v1, v;
