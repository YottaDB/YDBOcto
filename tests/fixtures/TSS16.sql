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

-- TSS16 : OCTO445 : Assertion failed in generate_physical_plan.c with sub-queries and multiple INNER JOINs

SELECT n1.id FROM names n1 INNER JOIN (SELECT n2.firstname FROM names n2 WHERE (n2.firstname <= 'a') OR (n2.id >= 3)) AS alias2 ON (n1.firstname >= alias2.firstname) INNER JOIN (SELECT n3.lastname FROM names n3) AS alias3 ON ('b' < alias3.lastname);

