#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TBT11 : OCTO498 : Test that BOOLEAN_IS plans show an empty LP_WHERE clause and an LP_KEY_FIX key
-- See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1092#note_932705081 for more information.

select * from names where firstname is null;
