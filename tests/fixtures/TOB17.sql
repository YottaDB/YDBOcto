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

select 1 from names order by 1;
select 1 from names order by TRUE;
select 1 from names order by FALSE;
select 1 from names order by nullif(1,1);
select 1 from names order by case 1 when 1 then 1 end;
select 1 from names order by 1+1;
select 1 from names order by (VALUES (1));
select 1 from names order by (select 1);
select 1 from names order by (select id from names limit 1);
