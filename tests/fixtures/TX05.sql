#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TX05 : OCTO657 : Octo-AIM regression where NULLs are missed because data that doesn't exist isn't indexed
-- See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/898#note_647885316 for details
CREATE TABLE x (id INTEGER PRIMARY KEY, val VARCHAR GLOBAL "^x(keys(""ID""),1)") GLOBAL "^x" AIMTYPE 1;
select id, val from x where val is NULL;
