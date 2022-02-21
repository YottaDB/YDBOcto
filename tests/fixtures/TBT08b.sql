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

-- TBT08 : OCTO498 : Test correct results in successive queries using IS (NOT) TRUE, IS (NOT) FALSE, and IS (NOT) UNKNOWN

discard all;
select id::boolean is unknown from names;
select id::boolean is true from names;

select * from stock_availability where available is unknown;
select * from stock_availability where available is false;
