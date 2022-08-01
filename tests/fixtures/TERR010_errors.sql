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

-- TERR010 : Rocto prefixes query errors in its log

select ABS(-id)+(select firstname from names limit 1) as absid from names;
SELECT A.id,A.firstName,B.id,B.firstName,(SELECT C.firstName FROM NAMES AS C WHERE C.firstName = B.firstName) FROM NAMES A INNER JOIN NAMES B on (A.firstName = B.firstName);
