#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

set datestyle="mdy";
select '2391-11-03 06:52:07-05'::timestamp with time zone;
-- ERROR
set datestyle="ymd";
select '2391-11-03 06:52:07-05'::timestamp with time zone;
-- Works
