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

-- ZHOROLOG "66787,43301,962541,18000"
-- TEXT "11-09-2023 12:01:41.962541+0530"

select date(zhorolog)'66787,43301,962541,18000';
select time(zhorolog)'66787,43301,962541,18000';
-- select time(zhorolog) with time zone'66787,43301,962541,18000';
select timestamp(zhorolog)'66787,43301,962541,18000';
select timestamp(zhorolog) with time zone'66787,43301,962541,18000';

select date(zhorolog)'66787,43301,,';
select time(zhorolog)'66787,43301,,';
-- select time(zhorolog) with time zone'66787,43301,,';
select timestamp(zhorolog)'66787,43301,,';
select timestamp(zhorolog) with time zone'66787,43301,,';
