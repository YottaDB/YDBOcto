#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Subqueries
-- multi-level query and groupby/aggregate
create view TCV006_1c1 as select * from customers;
create view TCV006_1o1 as select * from orders;
SELECT ALL first_name FROM TCV006_1c1 c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM TCV006_1o1 n1 GROUP BY order_id having EXISTS(SELECT 1 from TCV006_1o1 n2 group by n2.order_id having c.first_name != 'James'));
SELECT ALL first_name FROM TCV006_1c1 c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM TCV006_1o1 n1 GROUP BY order_id having EXISTS(SELECT 1 from TCV006_1o1 n2 group by n2.order_id having count(c.customer_id) > 1));
SELECT ALL 1 FROM TCV006_1c1 c having EXISTS (SELECT 1 FROM TCV006_1c1 n1 GROUP BY n1.last_name having count((select count(1) from TCV006_1c1 n3 limit 1))=1);
SELECT ALL 1 FROM TCV006_1c1 c having EXISTS (SELECT 1 FROM TCV006_1c1 n1 GROUP BY n1.last_name having count((select count(n3.first_name) from TCV006_1c1 n3 limit 1))=1);
SELECT ALL 1 FROM TCV006_1c1 c having EXISTS (SELECT 1 FROM TCV006_1c1 n1 GROUP BY n1.last_name having count((select count(n1.last_name||n3.first_name) from TCV006_1c1 n3 limit 1))=1);
SELECT ALL 1 FROM TCV006_1c1 c group by last_name having EXISTS (SELECT 1 FROM TCV006_1c1 n1 GROUP BY n1.last_name having count((select count(c.last_name||n3.first_name) from TCV006_1c1 n3 limit 1))=1);
SELECT ALL first_name FROM TCV006_1c1 c GROUP BY first_name HAVING EXISTS (SELECT 1 FROM TCV006_1c1 n1 GROUP BY n1.first_name having EXISTS(SELECT 1 from TCV006_1o1 n2 group by n2.order_id having ((c.first_name=n1.first_name))));
drop view TCV006_1c1;
drop view TCV006_1o1;
