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

-- TGB13 : OCTO516 : SIG-11 when HAVING clause uses an aggregate function with the IN operator and multiple values


SELECT 1 FROM customers HAVING COUNT(*) < 1;

SELECT 1 FROM customers GROUP BY customer_id HAVING COUNT(customer_id) IN (4,5);

SELECT ALL alias1.city, COUNT(ALL customers.address), customers.last_name FROM customers  INNER JOIN (SELECT ALL alias1.city FROM customers alias1) AS alias1 ON ((customers.city > alias1.city) OR NOT ((customers.email > alias1.city)) OR (customers.email < ANY (SELECT ALL alias3.first_name FROM customers alias3 WHERE EXISTS (SELECT alias4.zipcode, alias4.customer_id FROM customers alias4) ORDER BY alias3.first_name LIMIT 1)) OR NOT (customers.customer_id < ANY (SELECT DISTINCT alias5.customer_id FROM orders alias5 ORDER BY alias5.customer_id LIMIT 1))) WHERE customers.last_name BETWEEN 'Washington' AND 'Washington' GROUP BY alias1.city, customers.last_name HAVING COUNT(DISTINCT customers.address) IN (1, 3) ORDER BY COUNT(ALL customers.address), alias1.city, customers.last_name;

