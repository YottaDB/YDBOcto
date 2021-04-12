#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TOJ10 : OCTO706 : customers schema : Incorrect query results when RIGHT/FULL JOIN is used with OR in WHERE clause

SELECT o3.order_id,o4.order_id FROM customers c2 RIGHT JOIN orders o3 on TRUE RIGHT JOIN orders o4 ON o4.customer_id = c2.customer_id WHERE (c2.zipcode = '22902') OR (o4.order_id >= 1);

