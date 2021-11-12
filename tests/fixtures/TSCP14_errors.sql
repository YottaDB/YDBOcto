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

-- TSCP14 : OCTO386 : Verify AVG/SUM/MIN/MAX issue error with table.* even if table has only one column

-- Test of ERR_MISTYPED_FUNCTION_TABLE_ASTERISK error

select SUM(n1.*) from names1col n1;
select MIN(n1.*) from names1col n1;
select MAX(n1.*) from names1col n1;
select AVG(n1.*) from names1col n1;
select SUM(DISTINCT n1.*) from names1col n1;
select MIN(DISTINCT n1.*) from names1col n1;
select MAX(DISTINCT n1.*) from names1col n1;
select AVG(DISTINCT n1.*) from names1col n1;

select SUM(n2.*) from (values (1), (2), (NULL)) n2;
select MIN(n2.*) from (values (1), (2), (NULL)) n2;
select MAX(n2.*) from (values (1), (2), (NULL)) n2;
select AVG(n2.*) from (values (1), (2), (NULL)) n2;
select SUM(DISTINCT n2.*) from (values (1), (2), (NULL)) n2;
select MIN(DISTINCT n2.*) from (values (1), (2), (NULL)) n2;
select MAX(DISTINCT n2.*) from (values (1), (2), (NULL)) n2;
select AVG(DISTINCT n2.*) from (values (1), (2), (NULL)) n2;

select MIN(n2.*) from (select n1.firstname from names n1) n2;
select MAX(n2.*) from (select n1.firstname from names n1) n2;
select min(DISTINCT n2.*) from (select n1.firstname from names n1) n2;
select max(DISTINCT n2.*) from (select n1.firstname from names n1) n2;

