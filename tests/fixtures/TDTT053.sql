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

select date_format(now(),'%m-%d-%Y');
select CAST(now() as date);
select CAST(now() as char(17));

-- localtimestamp
select cast(localtimestamp as date);
select cast(localtimestamp as char(17));
-- current_timestamp
select cast(current_timestamp as date);
select cast(current_timestamp as char(17));
