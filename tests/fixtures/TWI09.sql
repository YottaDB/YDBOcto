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

-- TWI09 : OCTO407 : IN operator returns incorrect results when used with LEFT JOIN

SELECT ALL orders.customer_id, orders.order_id, orders.order_date, orders.order_amount FROM orders LEFT JOIN customers ON (orders.order_id = customers.customer_id) WHERE orders.order_amount IN ('$25.50', '$124.00', '$78.50');

select all n1.id,n2.id from names n1 right join names n2 on (n1.firstname = n2.lastname) where n1.id IN (1,2);

