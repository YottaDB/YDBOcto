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

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1404#note_1875565109
select * from names where firstname = (select firstname from names order by firstname limit 1);
