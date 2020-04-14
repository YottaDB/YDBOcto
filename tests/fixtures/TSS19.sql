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

-- TSS19 : OCTO489 : Identical column names in tables at different query levels should not cause Ambiguous column name error

SELECT c1.customer_id FROM customers c1 WHERE c1.customer_id = (SELECT customer_id FROM customers c2 LIMIT 1);
SELECT * FROM customers c1 WHERE c1.customer_id = (SELECT c2.customer_id FROM customers c2 WHERE c2.customer_id IN (SELECT customer_id FROM orders o2) LIMIT 1);

-- The below query should ORDER BY the "lastname" column in the table since that is what has been renamed AS "firstname"
-- in the select column list. Column name references for ORDER BY should always be satisfied only by the SELECT column list
-- and not by the column names in the tables involved in the FROM/JOIN list.
SELECT customer_id AS customer_id, first_name AS customer_id, last_name as first_name from customers ORDER BY first_name;

-- In the below query, even though input column name "customer_id" is renamed as "cid" in the SELECT column list,
-- the ORDER BY using "customer_id" should still succeed.
SELECT customer_id AS cid FROM customers ORDER BY customer_id desc;

