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

-- TCT010 : OCTO437 : DIVZERO error even though query that does no divide by 0 is run with and without :: (type cast operator)

select id/2 from names;
select id::integer/2 from names;
select id::numeric/2 from names;

