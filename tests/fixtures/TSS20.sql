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

-- TSS20 : OCTO482 : Test valid queries that should not issue <Misc FROM-clause entry> error

SELECT (SELECT * FROM (SELECT * FROM (SELECT * FROM (SELECT DISTINCT n1.id FROM names n1 WHERE n1.id = n5.id) n2) n3) n4) FROM names n5;

