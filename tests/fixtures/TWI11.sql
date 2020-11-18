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

-- TWI11 : OCTO515 : 'Unknown state reached' error when IN has a malformed comma-separated list of values

SELECT * FROM names WHERE id IN ();
SELECT * FROM names WHERE id IN (1,);
SELECT * FROM names WHERE id IN (1,2,);
SELECT * FROM names WHERE id IN (,);
SELECT * FROM names WHERE id IN (,1,);
SELECT * FROM names WHERE id IN (,,);
SELECT * FROM names WHERE id IN (,,1);
SELECT * FROM names WHERE id IN (1,,2,);

