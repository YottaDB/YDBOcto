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

-- TNK03 : OCTO336 : ORDER BY ASC should sort NULL values AFTER non-NULL values and BEFORE for DESC

select c2.customer_id from orders o1 left join customers c2 on o1.customer_id = c2.customer_id order by c2.customer_id;

