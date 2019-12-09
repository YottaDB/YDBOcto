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

-- TJOCO6 : OCTO393 : <Unknown column> error when valid column name alias from parent query is used in ON clause of sub-query

SELECT (SELECT 1 FROM names n2 INNER JOIN names n3 ON n3.id = n1.id LIMIT 1) from NAMES n1;

