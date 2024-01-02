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

-- Leap year:
--	Yes if Exactly divided by 4
--	except when it can be exactly divided by 100
--	Also, Above exception is not true when it can be exactly dividied by 400
select date'2024-02-29'; -- valid leap year
select timestamp'2024-02-28 01:00:00' + time'23:00:00';

-- Leap Second
select timestamp with time zone'2016-12-31 18:59:60-05:00';

-- select date'02-30-2024'; -- Invalid date
-- select date '02-29-2023'; -- Invalid leap year
