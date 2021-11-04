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

-- TVC03 : OCTO502 : Test various errors with VALUES clause

-- Test of ERR_UNKNOWN_COLUMN_NAME error
select * from (values (id));
select * from names n1 where id in (values (1), (n1.invalid));

-- Test of ERR_MISSING_FROM_ENTRY error
select * from (values (tbl.id)) as abcd;
select * from names n1 where id in (values (1), (n2.id));

-- Test of ERR_TYPE_MISMATCH error
select * from names n1 where id in (values (1), (n1.firstname));
select * from names n1 where id in (values (n1.lastname), (n1.firstname));

