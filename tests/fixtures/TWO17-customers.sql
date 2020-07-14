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
SELECT c.customer_id
	FROM orders o RIGHT JOIN customers c ON '1' = c.zipcode
	WHERE o.order_id IS NOT NULL;
SELECT o1.order_id,c2.customer_id
	FROM orders o1 LEFT JOIN customers c2 ON c2.customer_id = o1.customer_id
	WHERE (c2.first_name IS NOT NULL);
