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

-- TAS03 : Test ERR_SUBQUERY_ONE_COLUMN error for ARRAY constructor with subquery returning more than one column

SELECT ARRAY(VALUES(1,2,3)) from names;
SELECT ARRAY(SELECT id, id from names);
SELECT ARRAY(SELECT id, id from names UNION SELECT id, id from names);

