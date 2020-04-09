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

-- TAF02 : OCTO311 : Test aggregate function handling of NULL

SELECT c1.customer_id,COUNT(o2.order_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,MIN(o2.order_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,MAX(o2.order_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,AVG(o2.order_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,SUM(o2.order_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,COUNT(DISTINCT o2.order_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,MIN(DISTINCT o2.order_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,MAX(DISTINCT o2.order_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,AVG(DISTINCT o2.order_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,SUM(DISTINCT o2.order_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;

SELECT c1.customer_id,COUNT(o2.order_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,MIN(o2.order_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,MAX(o2.order_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,AVG(o2.order_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,SUM(o2.order_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,COUNT(DISTINCT o2.order_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,MIN(DISTINCT o2.order_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,MAX(DISTINCT o2.order_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,AVG(DISTINCT o2.order_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,SUM(DISTINCT o2.order_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;

SELECT c1.customer_id,COUNT(o2.order_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,MIN(o2.order_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,MAX(o2.order_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,AVG(o2.order_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,SUM(o2.order_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,COUNT(DISTINCT o2.order_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,MIN(DISTINCT o2.order_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,MAX(DISTINCT o2.order_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,AVG(DISTINCT o2.order_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;
SELECT c1.customer_id,SUM(DISTINCT o2.order_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY c1.customer_id;

SELECT o2.order_id,COUNT(c1.customer_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,MIN(c1.customer_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,MAX(c1.customer_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,AVG(c1.customer_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,SUM(c1.customer_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,COUNT(DISTINCT c1.customer_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,MIN(DISTINCT c1.customer_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,MAX(DISTINCT c1.customer_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,AVG(DISTINCT c1.customer_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,SUM(DISTINCT c1.customer_id) FROM customers c1 LEFT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;

SELECT o2.order_id,COUNT(c1.customer_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,MIN(c1.customer_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,MAX(c1.customer_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,AVG(c1.customer_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,SUM(c1.customer_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,COUNT(DISTINCT c1.customer_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,MIN(DISTINCT c1.customer_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,MAX(DISTINCT c1.customer_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,AVG(DISTINCT c1.customer_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,SUM(DISTINCT c1.customer_id) FROM customers c1 RIGHT JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;

SELECT o2.order_id,COUNT(c1.customer_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,MIN(c1.customer_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,MAX(c1.customer_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,AVG(c1.customer_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,SUM(c1.customer_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,COUNT(DISTINCT c1.customer_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,MIN(DISTINCT c1.customer_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,MAX(DISTINCT c1.customer_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,AVG(DISTINCT c1.customer_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;
SELECT o2.order_id,SUM(DISTINCT c1.customer_id) FROM customers c1 FULL JOIN orders o2 ON c1.customer_id = o2.customer_id GROUP BY o2.order_id;

