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

-- TCF014 : OCTO345 : Equivalent queries produce the same output due to accounting for function return type

select id*3 from names order by 1;
select ABS(id*3) from names order by 1;

