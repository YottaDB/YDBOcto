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

-- TSS15 : OCTO444 : <Problem resolving owner for deferred plan; undefined behavior> error with sub-queries, IN clause and multiple joins

SELECT 1 FROM (SELECT id FROM names WHERE id = 1 OR id = 2) n1 INNER JOIN names n2 ON (n1.id = n2.id);
SELECT 1 FROM names n1 INNER JOIN (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.id = 2) alias2 ON (n1.id = alias2.id ) INNER JOIN names n3 ON (n1.id = n3.id);
SELECT 1 FROM names n1 INNER JOIN (SELECT n2.lastname FROM names n2 WHERE n2.id = 6 OR n2.id = 7) alias2 ON (n1.firstname = alias2.lastname) INNER JOIN names n3 ON (n1.firstname != n3.lastname);
SELECT 1 FROM names n1 INNER JOIN (SELECT n2.lastname FROM names n2 WHERE n2.firstname IN ('John', 'George')) alias2 ON (n1.firstname = alias2.lastname) INNER JOIN names n3 ON (n1.firstname != n3.lastname);

