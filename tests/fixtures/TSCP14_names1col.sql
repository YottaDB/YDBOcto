#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Below are queries that use aggregate functions on the only column in the table
select SUM(n1.id) from names1col n1;
select MIN(n1.id) from names1col n1;
select MAX(n1.id) from names1col n1;
select AVG(n1.id) from names1col n1;
select SUM(DISTINCT n1.id) from names1col n1;
select MIN(DISTINCT n1.id) from names1col n1;
select MAX(DISTINCT n1.id) from names1col n1;
select AVG(DISTINCT n1.id) from names1col n1;

select SUM(n2.column1) from (values (1), (2), (NULL)) n2;
select MIN(n2.column1) from (values (1), (2), (NULL)) n2;
select MAX(n2.column1) from (values (1), (2), (NULL)) n2;
select AVG(n2.column1) from (values (1), (2), (NULL)) n2;
select SUM(DISTINCT n2.column1) from (values (1), (2), (NULL)) n2;
select MIN(DISTINCT n2.column1) from (values (1), (2), (NULL)) n2;
select MAX(DISTINCT n2.column1) from (values (1), (2), (NULL)) n2;
select AVG(DISTINCT n2.column1) from (values (1), (2), (NULL)) n2;

select n1.id from names1col n1 group by n1.id having SUM(n1.id)>1;
select n1.id from names1col n1 group by n1.id having MIN(n1.id)>1;
select n1.id from names1col n1 group by n1.id having MAX(n1.id)>1;
select n1.id from names1col n1 group by n1.id having AVG(n1.id)>1;
select n1.id from names1col n1 group by n1.id having SUM(DISTINCT n1.id)>1;
select n1.id from names1col n1 group by n1.id having MIN(DISTINCT n1.id)>1;
select n1.id from names1col n1 group by n1.id having MAX(DISTINCT n1.id)>1;
select n1.id from names1col n1 group by n1.id having AVG(DISTINCT n1.id)>1;

