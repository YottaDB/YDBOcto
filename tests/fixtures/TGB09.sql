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

-- TGB09 : OCTO457 : Assertion failed when GROUP BY is also used in a sub-query inside HAVING clause

SELECT lastName FROM names GROUP BY lastName HAVING EXISTS (SELECT alias1.lastName FROM names alias1 GROUP BY alias1.lastName);

