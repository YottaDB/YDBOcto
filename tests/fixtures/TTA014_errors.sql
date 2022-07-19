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

select n1.column1 from (VALUES('testa')) n1 where n1.* < ANY (select n2.* from (VALUES('test')) n2);
select n1.column1 from (VALUES('testa')) n1 where n1.* < ANY (select NULL);
select n1.column1 from (VALUES('testa')) n1 where n1.* < ALL (select NULL);
select n1.column1 from (VALUES('testa')) n1 where n1.* < SOME (select NULL);
select n1.column1 from (VALUES('testa')) n1 where n1.* < ANY (select 1);
select n1.column1 from (VALUES('testa')) n1 where n1.* < ALL (select 1);
select n1.column1 from (VALUES('testa')) n1 where n1.* < SOME (select 1);

-- Literal comparison
select n1.* < 1 from (VALUES(NULL)) n1;
select n1.* < 1 from (VALUES(1)) n1;
select n1.* < 'test' from (VALUES(NULL)) n1;
select n1.* < 'test' from (VALUES('test')) n1;

-- Subquery comparison
select n1.* > (select 'test') from (VALUES(NULL)) n1;
select n1.* > (select 1) from (VALUES(NULL)) n1;
select n1.* > (select NULL) from (VALUES(NULL)) n1;

-- Comparison between `table.*` values is not allowed when one table has NUMERIC as its column type and the other has INTEGER as its column type
select n1.* = n2.* from (select column1 from (VALUES(3))nx)n1,(select column1 from (VALUES(3.0))ny)n2;
select n1.* = n2.* from (select column1 from (VALUES(3))nx)n1,(select ROUND(column1,1) as column1 from (VALUES(3.0))ny)n2;
select n1.* = n2.* from (select customer_id from orders where customer_id = 3) n1, (select round(avg(customer_id),1) from orders where customer_id = 3) n2;

