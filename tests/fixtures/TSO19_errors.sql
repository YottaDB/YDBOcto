#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TS019 : OCTO803 : Test SET operations used as an operand to a binary operator do not SIG-11

-- Test of ERR_SUBQUERY_MULTIPLE_ROWS error

select (select 0 union select 1) > 1;
select * from (select (select 0 union select 1)>1 from names) n2;
select * from (select (select.1 union select 1)>1 from names) n2;

