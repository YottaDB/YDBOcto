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

-- Following query verifies that similar tablenames are not mistaken to be same
select count(DISTINCT n1.*) from names1col n11;

-- Type mismatch
select n1.* = n1.firstname from names1col n1;

-- Missing FROM-clause entry
select id from names1col n1 group by names1col.*;

-- Wrong aggregate function usage
select avg(n2.*) from (select n1.firstname from names n1) n2;
select sum(n2.*) from (select n1.firstname from names n1) n2;
select avg(DISTINCT n2.*) from (select n1.firstname from names n1) n2;
select sum(DISTINCT n2.*) from (select n1.firstname from names n1) n2;
