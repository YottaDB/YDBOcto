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

-- TNK01 : OCTO311 : Miscellaneous tests of NULL keyword using the `customers` schema

-- Test Integer2Boolean^%ydboctoplanhelpers handles NULL correctly
select customer_id from (select customer_id::boolean from (select c.customer_id from customers c right join orders o on c.customer_id = o.customer_id) subquery1 group by customer_id) subquery2;
select customer_id from (select customer_id::boolean from (select c.customer_id from customers c right join orders o on c.customer_id = o.customer_id) subquery1 group by customer_id) subquery2 where customer_id is NOT null;
select customer_id from (select customer_id::boolean from (select c.customer_id from customers c right join orders o on c.customer_id = o.customer_id) subquery1 group by customer_id) subquery2 where customer_id is null;

-- Test String2Boolean^%ydboctoplanhelpers and Boolean2String^%ydboctoplanhelpers handles NULL correctly
select bool1::boolean from (select bool1::text from (select (NULL = first_name) as bool1 from (select c.first_name from customers c right join orders o on c.customer_id = o.customer_id) subquery1) subquery2) subquery3 where bool1::boolean is NOT null;
select bool1::boolean from (select bool1::text from (select (NULL = first_name) as bool1 from (select c.first_name from customers c right join orders o on c.customer_id = o.customer_id) subquery1) subquery2) subquery3 where bool1::boolean is null;

-- Test of few NULL related edge cases from http://www-cs-students.stanford.edu/~wlam/compsci/sqlnulls
---- Test HAVING and NULL
select c.customer_id from orders o left join customers c on o.customer_id = c.customer_id group by c.customer_id having c.customer_id < 3;

