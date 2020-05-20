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

-- TSS25 : OCTO510 : LVUNDEF error when both sub query and parent query use parent query column with = operator in WHERE clause

select n1.id from names n1 where n1.id in (select n2.id from names n2 where n1.id = n2.id) and n1.id in (1,2,3,1);

