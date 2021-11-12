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
select MIN(n2.firstname) from (select n1.firstname from names n1) n2;
select MAX(n2.firstname) from (select n1.firstname from names n1) n2;
select min(DISTINCT n2.firstname) from (select n1.firstname from names n1) n2;
select max(DISTINCT n2.firstname) from (select n1.firstname from names n1) n2;

