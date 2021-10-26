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

-- TGB14 : OCTO766 : Test HAVING clause of sub query using GROUP BY column of outer query
SELECT ALL c.customer_id FROM customers c WHERE EXISTS (SELECT 1 FROM orders GROUP BY order_id having c.customer_id > 2) GROUP BY customer_id;

SELECT ALL customers.customer_id FROM customers  WHERE EXISTS (SELECT MIN(ALL alias4.order_amount) as order_amount, alias4.order_id FROM orders alias4 WHERE ((alias4.order_date||'03/14/1760')<='07/21/179507/04/1776') GROUP BY alias4.order_id, customers.customer_id having customers.customer_id>1) GROUP BY customers.customer_id;
