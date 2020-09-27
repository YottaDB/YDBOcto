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

-- TIJ07 : Verify that ON clause is not moved to the WHERE clause even in case of NO OUTER JOINs

select n1.id,n2.id,n3.id,n4.id from names n1 inner join names n2 on n2.id < n1.id inner join names n3 on n3.id < n2.id inner join names n4 on n4.id < n3.id;

