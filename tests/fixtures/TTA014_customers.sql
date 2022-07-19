#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Following queries test edge cases for comparison operation. First query tests whether an avg usage effects the operation based on its parameter type or not.
-- The second query checks that literal INTEGER and NUMERIC comparison are allowed. These queries were found during analysis of how AVG gets affected by its
-- parameter type. Following thread has more details on AVG change: https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1163#note_1037496957.
select n1.* = n2.* from (select avg(customer_id) from orders where customer_id = 3) n1, (select round(avg(customer_id),1) from orders where customer_id = 3) n2;
select n1.column1 = n2.column1 from (select column1 from (VALUES(3))nx)n1,(select column1 from (VALUES(3.0))ny)n2;
