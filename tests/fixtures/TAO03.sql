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

-- Test STRING type returned in sub-query for ALL/ANY/SOME operator
select n1.firstname from names n1 where n1.firstname = ALL (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname != ALL (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname < ALL (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname <= ALL (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname > ALL (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname >= ALL (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname = ANY (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname != ANY (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname < ANY (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname <= ANY (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname > ANY (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname >= ANY (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname = SOME (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname != SOME (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname < SOME (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname <= SOME (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname > SOME (select n2.firstname from names n2);
select n1.firstname from names n1 where n1.firstname >= SOME (select n2.firstname from names n2);

-- Test NUMERIC type returned in sub-query for ALL/ANY/SOME operator
select n1.id from names n1 where n1.id = ALL (select n2.id from names n2);
select n1.id from names n1 where n1.id != ALL (select n2.id from names n2);
select n1.id from names n1 where n1.id < ALL (select n2.id from names n2);
select n1.id from names n1 where n1.id <= ALL (select n2.id from names n2);
select n1.id from names n1 where n1.id > ALL (select n2.id from names n2);
select n1.id from names n1 where n1.id >= ALL (select n2.id from names n2);
select n1.id from names n1 where n1.id = ANY (select n2.id from names n2);
select n1.id from names n1 where n1.id != ANY (select n2.id from names n2);
select n1.id from names n1 where n1.id < ANY (select n2.id from names n2);
select n1.id from names n1 where n1.id <= ANY (select n2.id from names n2);
select n1.id from names n1 where n1.id > ANY (select n2.id from names n2);
select n1.id from names n1 where n1.id >= ANY (select n2.id from names n2);
select n1.id from names n1 where n1.id = SOME (select n2.id from names n2);
select n1.id from names n1 where n1.id != SOME (select n2.id from names n2);
select n1.id from names n1 where n1.id < SOME (select n2.id from names n2);
select n1.id from names n1 where n1.id <= SOME (select n2.id from names n2);
select n1.id from names n1 where n1.id > SOME (select n2.id from names n2);
select n1.id from names n1 where n1.id >= SOME (select n2.id from names n2);

