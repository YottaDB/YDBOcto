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

-- TGB08 : OCTO456 : SIG-11 in Octo when using aggregate functions in sub-queries in WHERE clause that also uses OR operator" {

SELECT id FROM names WHERE EXISTS (SELECT COUNT(*) FROM names n2 GROUP BY n2.lastName) AND ((id < 3) OR (id > 3)) ORDER BY id;

