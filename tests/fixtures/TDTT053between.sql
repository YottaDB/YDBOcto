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

-- Following queries output 0 occasionally with MySQL which is not expected so we cannot use cross check function here
select current_timestamp between current_timestamp - time'01:01:01' and current_timestamp + time'01:01:01';
select now() between now() - time'01:01:01' and now() + time'01:01:01';
select localtimestamp between localtimestamp - time'01:01:01' and localtimestamp + time'01:01:01';
