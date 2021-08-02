#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSS26 : OCTO727 : LVUNDEF error when running a query with sub-queries

SELECT (SELECT n3.id) FROM names N1 inner join NAMES n2 ON TRUE INNER JOIN names n3 ON TRUE;

