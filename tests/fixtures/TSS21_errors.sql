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

-- All queries in this query file are invalid queries and generate an error.

-- TSS21 : OCTO492 : SIG-11 from a query that also issues a <Subquery must return only one column> error

-- Below are queries that did generate a SIG-11 without the OCTO492 code fix
SELECT (SELECT 1,2);
SELECT 1 FROM names ORDER BY (SELECT 1,2);

-- Below are queries that did not SIG-11 previously but are very similar to the above queries that did SIG-11 so are included here
SELECT 1 FROM names WHERE 1 = (SELECT 1,2);

