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

-- THQ05 : OCTO513 : Two different CROSS JOIN queries incorrectly hash to the same plan resulting in incorrect output

SELECT COUNT(customers.zipcode) FROM customers CROSS JOIN orders AS alias1 CROSS JOIN customers AS alias2;
SELECT COUNT(customers.zipcode) FROM customers CROSS JOIN orders AS alias1 CROSS JOIN orders    AS alias2;

