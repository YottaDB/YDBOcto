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

select timestamp with time zone'8805-1-1 15:14:11+13:56'; -- null result
select timestamp with time zone'5989-1-1 2:39:22-09:02'; -- null result
select timestamp(zhorolog) with time zone'1515026,83842,,44640'; -- error result
