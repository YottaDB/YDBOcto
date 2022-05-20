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

-- Binary expression
-- =
select n1.* = NULL from names n1;
select NULL = n1.* from names n1;
select n1.* = n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1) ,(2) ,(1)) n2 on (n1.column1=2);
select n1.* = n2.* from (VALUES('test') ,('nottest') ,('test')) n1 JOIN (VALUES('test') ,('nottest') ,('test')) n2 on (n1.column1='nottest');

-- !=
select n1.* != NULL from names n1;
select NULL != n1.* from names n1;
select n1.* != n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1) ,(2) ,(1)) n2 on (n1.column1=2);
select n1.* != n2.* from (VALUES('test') ,('nottest') ,('test')) n1 JOIN (VALUES('test') ,('nottest') ,('test')) n2 on (n1.column1='nottest');

-- >
select n1.* < n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1) ,(2) ,(1)) n2 on (n1.column1=2);

-- <
select n1.* > n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1) ,(2) ,(1)) n2 on (n1.column1=2);
