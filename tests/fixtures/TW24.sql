#################################################################
#								#
# Copyright (c) 2020-2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TW24 : Test of MODULO operator % with negative operands (#1030 : match Postgres/SQL standard behavior)

select 11 % 4;
select 11 % (-4);
select (-11) % 4;
select (-11) % (-4);
select 0 % 4;
select 0 % (-4);
select 8 % (-4);
select (-8) % 4;
select 11.5 % (-4);
select NULL % (-4);
select 11 % NULL;

-- Use % in an ORDER BY expression to exercise the code path with dot_count=0
select id, id % -3 from names order by id % -3;

