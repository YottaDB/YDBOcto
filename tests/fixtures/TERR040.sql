#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TERR040 : OCTO793 : Test syntax highlighting correct when preceding comment includes inline comment

SELECT NULL FROM names GROUP BY NULL; -- non integer constants in GroupBy are not allowed
SELECT NULL FROM names GROUP BY NULL HAVING NULL!='hello'; -- non integer constants in GroupBy are not allowed
