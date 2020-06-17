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

-- TOJ08 : OCTO534 : customers schema : Incorrect query results when LEFT/RIGHT/FULL JOIN is used with OR in WHERE clause
SELECT customers.state, alias1.customer_id, customers.last_name FROM customers  INNER JOIN orders AS alias1 ON ((customers.customer_id < alias1.order_id)) LEFT JOIN customers AS alias2 ON ((customers.last_name = 'Mount Vernon') AND ((customers.last_name = alias2.state))) WHERE ((1 = 1) OR (1 = 1));

