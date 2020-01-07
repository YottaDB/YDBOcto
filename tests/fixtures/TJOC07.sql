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

-- TJOCO7 : OCTO426 : Incorrect results from LEFT/RIGHT/FULL JOIN if ON clause has = or IN operator


select c.customer_id,o.order_id from customers c left join orders o on c.customer_id = o.customer_id;
select c.customer_id,o.order_id from customers c left join (select * from orders) o on c.customer_id = o.customer_id;
select c.customer_id,o.order_id from customers c left join (select * from orders UNION ALL select * from orders) o on c.customer_id = o.customer_id;
select c.customer_id,o.order_id from customers c left join orders o on c.customer_id = 4;
select c.customer_id,o.order_id from customers c left join (select * from orders) o on c.customer_id = 4;
select c.customer_id,o.order_id from customers c left join (select * from orders UNION ALL select * from orders) o on c.customer_id = 4;
select c.customer_id,o.order_id from customers c right join orders o on c.customer_id = o.customer_id;
select c.customer_id,o.order_id from customers c right join (select * from orders) o on c.customer_id = o.customer_id;
select c.customer_id,o.order_id from customers c right join (select * from orders UNION ALL select * from orders) o on c.customer_id = o.customer_id;
select c.customer_id,o.order_id from customers c right join orders o on c.customer_id = 4;
select c.customer_id,o.order_id from customers c right join (select * from orders) o on c.customer_id = 4;
select c.customer_id,o.order_id from customers c right join (select * from orders UNION ALL select * from orders) o on c.customer_id = 4;
select c.customer_id,o.order_id from customers c full join orders o on c.customer_id = o.customer_id;
select c.customer_id,o.order_id from customers c full join (select * from orders) o on c.customer_id = o.customer_id;
select c.customer_id,o.order_id from customers c full join (select * from orders UNION ALL select * from orders) o on c.customer_id = o.customer_id;

select c.customer_id,o.order_id from customers c left  join orders o on c.customer_id = 4;
select c.customer_id,o.order_id from customers c right join orders o on c.customer_id = 4;
select c.customer_id,o.order_id from customers c left  join orders o on o.order_id = 4;
select c.customer_id,o.order_id from customers c right join orders o on o.order_id = 4;
select c.customer_id,o.order_id from customers c left  join orders o on o.customer_id = c.customer_id;
select c.customer_id,o.order_id from customers c right join orders o on o.customer_id = c.customer_id;
select c.customer_id,o.order_id from customers c full  join orders o on o.customer_id = c.customer_id;

select c1.customer_id,o.order_id,c2.customer_id from customers c1 right join orders o on o.customer_id = c1.customer_id right join customers c2 on c1.customer_id = 3;
select c1.customer_id,o.order_id,c2.customer_id from customers c1 right join orders o on o.customer_id = c1.customer_id right join customers c2 on c1.customer_id = 4;

-- Note that the below FULL JOIN queries are commented below as Postgres issues the following error
--	`ERROR:  FULL JOIN is only supported with merge-joinable or hash-joinable join conditions`
-- But Octo works fine on them.

-- select c.customer_id,o.order_id from customers c full join orders o on c.customer_id = 4;
-- select c.customer_id,o.order_id from customers c full join (select * from orders) o on c.customer_id = 4;
-- select c.customer_id,o.order_id from customers c full join (select * from orders UNION ALL select * from orders) o on c.customer_id = 4;
-- select c.customer_id,o.order_id from customers c full  join orders o on c.customer_id = 4;
-- select c.customer_id,o.order_id from customers c full  join orders o on o.order_id = 4;

