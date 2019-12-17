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

-- TBEE03 : OCTO408 : LVUNDEF error when LEFT JOIN is used in the FROM clause and OR operator is used in the WHERE clause

SELECT n1.id,n2.id FROM names n1 LEFT JOIN names n2 ON (n1.firstname = n2.lastname) WHERE (1 = 1) OR (1 = 1);
SELECT orders.order_id FROM orders LEFT OUTER JOIN customers ON (orders.order_amount = customers.zipcode) WHERE NOT ('$25.50' > orders.order_amount) OR NOT (orders.customer_id <= 3) OR (orders.customer_id <= 3) ORDER BY orders.order_id;

