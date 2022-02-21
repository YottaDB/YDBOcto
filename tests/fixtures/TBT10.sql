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

-- TBT10 : OCTO498 : Confirm proper number of crossreference plans generated for queries using IS (NOT) TRUE, IS (NOT) FALSE, and IS (NOT) UNKNOWN

select * from stock_availability where available is false;
select * from stock_availability where available is true;
select * from stock_availability where available is null;
select * from stock_availability where available is unknown;
