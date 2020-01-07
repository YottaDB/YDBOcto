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

-- TBEE04 : OCTO427 : Assert failure when sub-query and outer query both have OR conditions with = operator

SELECT * FROM (SELECT * FROM names WHERE id = 1 OR firstname = 'Zero') n1 WHERE lastname = 'Burn' OR id = 0;
SELECT * FROM (SELECT * FROM (SELECT * FROM names WHERE id = 1 OR firstname = 'Zero') n1 WHERE lastname = 'Burn' OR id = 0) n1 WHERE lastname = 'Burn' OR id = 0;

