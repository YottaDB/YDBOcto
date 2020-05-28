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

-- TW24 : Test of MODULO operator % with negative operands (Octo behavior differs from Postgres)

select 11 % 4;
select 11 % (-4);
select (-11) % 4;
select (-11) % (-4);

