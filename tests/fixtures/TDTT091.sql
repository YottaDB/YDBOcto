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

-- datestyle is MDY
show datestyle;
select concat(date'01-01-2023', 'sample text');
select concat(date(fileman)'3230101', 'sample text');
set datestyle="YMD";
show datestyle;
select concat(date'2023-01-01', 'sample text');
select concat(date(fileman)'3230101', 'sample text');
set datestyle="DMY";
show datestyle;
select concat(date'14-01-2023', 'sample text');
select concat(date(fileman)'3230114', 'sample text');
