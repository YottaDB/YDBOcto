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

-- Subquery within view definition
-- Single subquery
create view v1 as select * from (select id,firstname,lastname from names) n1,(select id,firstname,lastname from names) n2; -- ERROR:  column "id" specified more than once
-- names=> create view v1 as select n1.id,n1.firstname,n1.lastname,n2.id,n2.firstname,n2.lastname from (select id, firstname, lastname from names)n1, (select id, firstname,lastname from names)n2;
-- ERROR:  column "id" specified more than once

create view v1 (v1_n1_id,v1_n1_firstname,v1_n1_lastname,v1_n2_id,v1_n2_firstname,v1_n2_lastname) as select n1.* from (select * from names) n1 left join (select * from names) n2 on n1.firstname = n2.firstname where n1.lastName = 'Cool' AND (n1.firstName = 'Zero' OR n1.lastName = 'Burn');
-- ERROR:  CREATE VIEW specifies more column names than columns

-- Multiple nested subqueries
create view v1 as select * from (select id,firstname,lastname from (select id,firstname,lastname from (select id,firstname,lastname from (select id,firstname,lastname from names)n1)n2)n3)n4;
select * from v1 group by firstname;
--OCTO> select * from v1 group by firstname;
--[ERROR]: ERR_GROUP_BY_OR_AGGREGATE_FUNCTION: Column ID must appear in the GROUP BY clause or be used in an aggregate function

