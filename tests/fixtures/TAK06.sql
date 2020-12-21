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

-- TAK06 : OCTO520 : Avoid multiple plan generation when the query only differs by identifier name

-- All queries in the below list (differing column alias values) should hash to different plans
select id as ida from names;
select id as idb from names;
select id as "strlit1" from names;
select id as "strlit2" from names;

-- All queries in the below list (differing table alias values) should hash to just ONE plan
select * from names n1;
select * from names n2;
select * from names as n3;
select * from names as n4;
select * from names "strlit1";
select * from names "strlit2";
select * from names as "strlit3";
select * from names as "strlit4";

