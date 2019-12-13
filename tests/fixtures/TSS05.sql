#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/OR its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSS05 : OCTO404 : Assert failure when referencing inherited sub-query columns in parent query WHERE clause

SELECT * FROM (select * from names) n1 WHERE n1.id < 4;
SELECT DISTINCT * FROM (select * from names) n1 WHERE n1.id < 4;

