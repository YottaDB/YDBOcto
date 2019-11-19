#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TOB08 : OCTO280 : ORDER BY column_number

select * from names order by 1;
select * from names order by 2;
select * from names order by 3;
select * from names order by 4;
select * from names order by -1;
select * from names order by 0;
