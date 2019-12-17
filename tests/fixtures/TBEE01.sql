#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TBEE01 : 3-way LEFT JOIN with 20 OR usages in BOOLEAN EXPRESSION should expand to only 27 plans (not 540 plans)

SELECT *
FROM names n1
LEFT JOIN names n2 ON n1.id = n2.id
LEFT JOIN names n3 ON n2.id = n3.id
LEFT JOIN names n4 ON n3.id = n4.id
WHERE    (n1.id = 1) OR (n1.id = 2) OR (n1.id = 3) OR (n1.id = 4) OR (n1.id = 5)
      OR (n2.id = 1) OR (n2.id = 2) OR (n2.id = 3) OR (n2.id = 4) OR (n2.id = 5)
      OR (n3.id = 1) OR (n3.id = 2) OR (n3.id = 3) OR (n3.id = 4) OR (n3.id = 5)
      OR (n4.id = 1) OR (n4.id = 2) OR (n4.id = 3) OR (n4.id = 4) OR (n4.id = 5);

