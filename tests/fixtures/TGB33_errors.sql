#################################################################
#								#
# Copyright (c) 2020-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Columns belonging to outer and inner query in HAVING/ORDER BY/SELECT (Error)
-- ORDER BY
SELECT ALL 1 FROM customers c GROUP BY last_name ORDER BY EXISTS (SELECT 1 FROM customers n1 ORDER BY count(c.first_name=n1.last_name)=1);
SELECT ALL 1 FROM customers c GROUP BY last_name ORDER BY EXISTS (SELECT 1 FROM customers n1 ORDER BY count(n1.last_name=c.first_name)=1);
SELECT ALL 1 FROM customers c GROUP BY last_name ORDER BY EXISTS (SELECT 1 FROM customers n1 ORDER BY count(n1.last_name=c.first_name)=1);
SELECT ALL 1 FROM customers c GROUP BY first_name ORDER BY EXISTS (SELECT 1 FROM customers n1 ORDER BY count(c.last_name=n1.first_name)=1);

-- HAVING
SELECT ALL 1 FROM customers c GROUP BY last_name HAVING EXISTS (SELECT 1 FROM customers n1 ORDER BY count(c.first_name=n1.last_name)=1);
SELECT ALL 1 FROM customers c GROUP BY last_name HAVING EXISTS (SELECT 1 FROM customers n1 ORDER BY count(n1.last_name=c.first_name)=1);
SELECT ALL 1 FROM customers c GROUP BY last_name HAVING EXISTS (SELECT 1 FROM customers n1 ORDER BY count(n1.last_name=c.first_name)=1);
SELECT ALL 1 FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM customers n1 ORDER BY count(c.last_name=n1.first_name)=1);

-- SELECT
SELECT ALL EXISTS (SELECT 1 FROM customers n1 ORDER BY count(c.first_name=n1.last_name)=1) FROM customers c GROUP BY last_name;
SELECT ALL EXISTS (SELECT 1 FROM customers n1 ORDER BY count(n1.last_name=c.first_name)=1) FROM customers c GROUP BY last_name;
SELECT ALL EXISTS (SELECT 1 FROM customers n1 ORDER BY count(n1.last_name=c.first_name)=1) FROM customers c GROUP BY last_name;
SELECT ALL EXISTS (SELECT 1 FROM customers n1 ORDER BY count(c.last_name=n1.first_name)=1) FROM customers c GROUP BY first_name;

-- #807
-- Below should error because non-grouped column is used in an aggregate expression
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count(c.first_name=n1.first_name)=1);
SELECT ALL 1 FROM customers c having EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name order by count(c.first_name=n1.first_name)=1);
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM customers n1 having EXISTS(SELECT 1 from orders n2 group by n2.order_id having count((c.first_name=n1.last_name)=(n2.order_date='test'))=1));
SELECT ALL first_name FROM customers c HAVING EXISTS (SELECT 1 FROM customers n1 having EXISTS(SELECT 1 from orders n2 group by n2.order_id having count(c.first_name=n1.first_name)=1));

-- #806 and #807
SELECT ALL 1 FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM customers n1 where count(c.first_name=n1.first_name)=1 GROUP BY n1.last_name);

SELECT ALL 1 FROM customers c GROUP BY last_name HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.first_name having count(c.first_name=n1.first_name)=1);
SELECT ALL 1 FROM customers c GROUP BY last_name HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count(c.first_name=n1.first_name)=1);
SELECT ALL 1 FROM customers c GROUP BY last_name HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count(c.first_name=n1.last_name)=1);
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having EXISTS(SELECT 1 from orders n2 group by n2.order_id having count((c.first_name||n1.first_name)=order_amount)=1));
SELECT ALL 1 FROM customers c GROUP BY last_name HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count(c.first_name=n1.first_name)=1);
SELECT ALL 1 FROM customers c GROUP BY last_name HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name having count(c.first_name=n1.last_name)=1);

-- Edge cases to verify working of aggregate function in inner query having outer query columns when outer query has GROUP BY
SELECT ALL last_name FROM customers c GROUP BY last_name ORDER BY EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.last_name ORDER BY count(c.first_name=n1.last_name)=1);
SELECT ALL last_name FROM customers c ORDER BY EXISTS (SELECT first_name FROM customers n1 ORDER BY count(c.first_name=n1.last_name)=1);
SELECT ALL last_name FROM customers c ORDER BY EXISTS (SELECT first_name FROM customers n1 ORDER BY count(c.first_name=c.last_name)=1);
SELECT ALL last_name FROM customers c ORDER BY EXISTS (SELECT last_name FROM customers n1 GROUP BY last_name ORDER BY count(c.first_name=c.last_name)=1);

-- Outer GROUP BY having an expression and inner query referring to the outer query column using the same expression in GROUP BY
SELECT ALL 1 FROM customers c GROUP BY first_name||'test' HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.first_name having (c.first_name||'test')=n1.first_name);
SELECT ALL 1 FROM customers c GROUP BY first_name||'test' HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.first_name having count((c.first_name||'test')=n1.first_name)=2);
SELECT ALL 1 FROM customers c GROUP BY first_name||'test' HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.first_name order by (c.first_name||'test'));
SELECT ALL 1 FROM customers c GROUP BY first_name||last_name HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.first_name order by c.first_name||c.last_name);

-- TableAsterisk query
SELECT ALL first_name FROM customers c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM customers n1 GROUP BY n1.first_name having EXISTS(SELECT 1 from orders n2 group by n2.order_id having count((c.* = n1.*) = TRUE)=1));
