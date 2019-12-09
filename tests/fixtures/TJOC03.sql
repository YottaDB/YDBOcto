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

-- TJOC03 : OCTO305 LEFT JOIN of subquery containing UNION operator

select * from names n1
left join (
        select * from names where id = 1
        UNION
        select * from names where id = 2
) n2
on n1.id = n2.id;

