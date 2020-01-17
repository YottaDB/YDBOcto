#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSS13 : OCTO353 : Sub-query with a SELECT column list that is of BOOLEAN type does not work as a boolean operand in the parent query

select * from names where id = 0 OR (select id = 2);

