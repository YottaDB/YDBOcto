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

-- TBT04 : Test NOT with boolean comparison as operand from failed QueryGenerator case

SELECT DISTINCT nullnames.id, nullnames.exempt FROM nullnames  CROSS JOIN nullnames AS alias1 WHERE (NOT ('Acid' > 'Lord') OR (NOT (nullnames.salary < 25000.01)) OR (NOT (false < false))) ORDER BY nullnames.exempt, nullnames.id;
