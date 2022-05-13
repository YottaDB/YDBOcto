#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Below query's OrderBy has incorrect expression. It is supposed to match or build upon the one in GroupBy.
select NOT (3 >= customers.customer_id) from customers group by NOT (3 >= customers.customer_id) order by (3 >= customers.customer_id);

-- Below query's Having clause has incorrect expression. It is supposed to match or build upon the one in GroupBy.
select NOT (3 >= customers.customer_id) from customers group by NOT (3 >= customers.customer_id) having (3>= customers.customer_id);

-- Both OrderBy and Having clause are incorrect
select NOT (3 >= customers.customer_id) from customers group by NOT (3 >= customers.customer_id) having (3>= customers.customer_id) order by (3 >= customers.customer_id);

-- Below query validates that GroupBy expression forms the smallest expression element which having clause can use in the presence of GroupBy
SELECT id+1 from names group by id+1 having id=1;
