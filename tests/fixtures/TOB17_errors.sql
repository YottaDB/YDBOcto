#################################################################
#								#
# Copyright (c) 2022-2025 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select 1 from names order by 1.0;
select 1 from names order by 111111111111111111111111111111111;
select 1 from names order by -1;
select 1 from names order by 'test';
select 1 from names order by "test";
select 1 from names order by NULL;
select 1 from names order by TRUE;
select 1 from names order by FALSE;