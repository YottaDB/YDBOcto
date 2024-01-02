#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

create function samevalue(date(fileman)) returns date as $$samevalue^datetime;
select samevalue(date(fileman)'3230101'); -- Function is expected to return text format according to the function definition

-- the date/time format will not be part of the hash so the following will error
create function samevalue(date(horolog)) returns date as $$samevalue^datetime;

